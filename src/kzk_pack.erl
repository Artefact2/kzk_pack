% Author: Romain "Artefact2" Dalmaso <artefact2@gmail.com>
%
% This program is free software. It comes without any warranty, to the
% extent permitted by applicable law. You can redistribute it and/or
% modify it under the terms of the Do What The Fuck You Want To Public
% License, Version 2, as published by Sam Hocevar. See
% http://sam.zoy.org/wtfpl/COPYING for more details.

-module(kzk_pack).
-include("pack.hrl").
-export([open/2, dispose/1, create/1, commit/1, append_file/3, list/1, has_file/2, get_file/2, integrity_check/1]).

-define(MAGIC, "KZKPACK_").
-define(BUFFER, 65024).
-define(HEADER_LENGTH, 8 + 4 + 8 + 8 + 20).
-define(ENTRY_LENGTH, 20 + 8 + 8 + 4).
-define(VERSION, 1).
-define(dbg(X), io:format("~n===== DEBUG OUTPUT =====~n~s~n~w~n~n", [??X, X]), X).

-spec(open(string(), boolean()) -> {ok, pack()}).
open(Filename, Readonly) ->
    Modes = case Readonly of
		true -> [read];
		false -> [read, write]
	    end,
    {ok, IoDevice} = file:open(Filename, Modes ++ [raw, binary, delayed_write, read_ahead]),
    {ok, DataOffset, TOCOffset, TOCSha1} = read_header(IoDevice),
    {ok, NameTable, HashTable} = read_toc(IoDevice, TOCOffset),
    {ok, #pack{
       file_name = Filename, 
       io_device = IoDevice, 
       data_offset = DataOffset, 
       toc_offset = TOCOffset, 
       toc_sha1 = TOCSha1, 
       sha1_table = HashTable, 
       file_table = NameTable
      }}.

-spec(dispose(pack()) -> ok).
dispose(Pack) ->
    ok = file:close(Pack#pack.io_device),
    true = ets:delete(Pack#pack.sha1_table),
    true = ets:delete(Pack#pack.file_table),
    ok.

-spec(create(string()) -> {ok, pack()}).
create(Filename) ->
    {ok, IoDevice} = file:open(Filename, [read, write, raw, binary, delayed_write, read_ahead]),
    commit(#pack{
	      file_name = Filename,
	      io_device = IoDevice,
	      data_offset = ?HEADER_LENGTH,
	      toc_offset = ?HEADER_LENGTH,
	      toc_sha1 = <<0:160/integer>>,
	      sha1_table = ets:new(hashes, [set]),
	      file_table = ets:new(names, [ordered_set])
	 }).

-spec(commit(pack()) -> {ok, pack()}).
commit(Pack) ->
    TOC = generate_toc(Pack),
    NewPack = Pack#pack{toc_sha1 = crypto:sha(TOC)},
    ok = write_header(NewPack),
    ok = write_toc(NewPack, TOC),
    {ok, NewPack}.

-spec(append_file(pack(), string(), binary()) -> {ok, pack()} | {error, name_clash}).
append_file(Pack, Filename, Data) ->
    Sha1 = crypto:sha(Data),
    case ets:lookup(Pack#pack.file_table, Filename) of
	[{Filename, Sha1}] -> {ok, Pack}; % Adding an existing file with identical contents: nothing to do.
	[{Filename, _Sha1}] -> {error, name_clash}; % Adding an existing file with different contents.
	[] ->
	    true = ets:insert(Pack#pack.file_table, {Filename, Sha1}),
	    case ets:lookup(Pack#pack.sha1_table, Sha1) of
		[{Sha1, _DOffset, _DLength}] -> {ok, Pack}; % Contents already in the pack.
		[] -> % Append the contents in the Data section.
		    DOffset = Pack#pack.toc_offset - Pack#pack.data_offset,
		    DLength = size(Data),
		    true = ets:insert(Pack#pack.sha1_table, {Sha1, DOffset, DLength}),
		    ok = file:pwrite(Pack#pack.io_device, Pack#pack.data_offset + DOffset, Data),
		    {ok, Pack#pack{toc_offset = Pack#pack.data_offset + DOffset + DLength}}
	    end
    end.

-spec(has_file(pack(), string()) -> boolean()).
has_file(Pack, Filename) ->
    case ets:lookup(Pack#pack.file_table, Filename) of
	[] ->
	    false;
	[{Filename, Sha1}] ->
	    case ets:lookup(Pack#pack.sha1_table, Sha1) of
		[] ->
		    false;
		_ -> true
	    end
    end.

-spec(get_file(pack(), string()) -> {ok, binary()} | {error, file_not_found} | {error, contents_not_found}).
get_file(Pack, Filename) ->
    case ets:lookup(Pack#pack.file_table, Filename) of
	[] ->
	    {error, file_not_found};
	[{Filename, Sha1}] ->
	    case ets:lookup(Pack#pack.sha1_table, Sha1) of
		[] ->
		    {error, contents_not_found};
		[{Sha1, DOffset, DLength}] ->
		    case DLength =:= 0 of
			true -> {ok, <<>>};
			false -> 
			    {ok, Data} = file:pread(Pack#pack.io_device, Pack#pack.data_offset + DOffset, DLength),
			    {ok, Data}
		    end
	    end
    end.

-spec(list(pack()) -> {ok, [{binary(), integer(), string()}]}).
list(Pack) ->
    {ok, list(Pack, [], ets:last(Pack#pack.file_table))}.

-spec(integrity_check(pack()) -> ok | {error, corrupted_toc} | {error, corrupted_files, [string()]}).
integrity_check(Pack) ->
    case check_toc(Pack) of
	ok ->
	    check_files(Pack);
	NotOK -> NotOK
    end.

% Unexported functions
% ====================

read_header(IoDevice) ->
    {ok, <<?MAGIC, (?VERSION):32/unsigned-integer-big,
	   DataOffset:64/unsigned-integer-big,
	   TOCOffset:64/unsigned-integer-big,
	   TOCSha1:160/bits >>} = file:pread(IoDevice, 0, ?HEADER_LENGTH),
    {ok, DataOffset, TOCOffset, TOCSha1}.

read_toc(IoDevice, TOCOffset) ->
    read_toc(IoDevice, TOCOffset, ets:new(names, [ordered_set]), ets:new(hashes, [set])).

read_toc(IoDevice, TOCOffset, NameTable, HashTable) ->
    case file:pread(IoDevice, TOCOffset, ?ENTRY_LENGTH) of
	eof -> {ok, NameTable, HashTable};
        {ok, <<FileSha1:160/bits, DOffset:64/unsigned-integer-big,
	       DLength:64/unsigned-integer-big,
	       FilenameLength:32/unsigned-integer-big >>} ->
	    {ok, BinFilename} = file:pread(IoDevice, TOCOffset + ?ENTRY_LENGTH, FilenameLength),
	    ets:insert(HashTable, {FileSha1, DOffset, DLength}),
	    ets:insert(NameTable, {unicode:characters_to_list(BinFilename, utf8), FileSha1}),
	    read_toc(IoDevice, TOCOffset + ?ENTRY_LENGTH + FilenameLength, NameTable, HashTable)
    end.

write_header(Pack) ->
    ok = file:pwrite(Pack#pack.io_device, 0, 
		     <<?MAGIC, (?VERSION):32/unsigned-integer-big,
		       (Pack#pack.data_offset):64/unsigned-integer-big,
		       (Pack#pack.toc_offset):64/unsigned-integer-big,
		       (Pack#pack.toc_sha1):160/bits>>),
    ok.

write_toc(Pack, TOC) ->
    ok = file:pwrite(Pack#pack.io_device, Pack#pack.toc_offset, TOC),
    ok.

generate_toc(Pack) ->
    generate_toc_entry(Pack, ets:first(Pack#pack.file_table)).

generate_toc_entry(_Pack, '$end_of_table') ->
    <<>>;
generate_toc_entry(Pack, Filename) ->
    [{Filename, Sha1}] = ets:lookup(Pack#pack.file_table, Filename),
    [{Sha1, DOffset, DLength}] = ets:lookup(Pack#pack.sha1_table, Sha1),
    BinFilename = unicode:characters_to_binary(Filename, utf8),
    FilenameLength = size(BinFilename),
    <<Sha1/binary, DOffset:64/unsigned-integer-big,
      DLength:64/unsigned-integer-big,
      FilenameLength:32/unsigned-integer-big,
      BinFilename/binary,
      (generate_toc_entry(Pack, ets:next(Pack#pack.file_table, Filename)))/binary>>.

list(_Pack, Acc, '$end_of_table') -> Acc;
list(Pack, Acc, Filename) ->
    [{Filename, Sha1}] = ets:lookup(Pack#pack.file_table, Filename),
    [{Sha1, _DOffset, DLength}] = ets:lookup(Pack#pack.sha1_table, Sha1),
    list(Pack, [{Sha1, DLength, Filename} | Acc], ets:prev(Pack#pack.file_table, Filename)).

pread_until_eof(IoDevice, Position) ->
    case file:pread(IoDevice, Position, ?BUFFER) of
	{ok, Data} ->
	    <<Data/binary, (pread_until_eof(IoDevice, Position + ?BUFFER))/binary>>;
	eof ->
	    <<>>
    end.

check_toc(Pack) ->
    TOC = pread_until_eof(Pack#pack.io_device, Pack#pack.toc_offset),
    case crypto:sha(TOC) =:= Pack#pack.toc_sha1 of
	true ->
	    ok;
	false -> {error, corrupted_toc}
    end.

check_files(Pack) ->
    {ok, List} = list(Pack),
    case check_files(Pack, List, []) of
	[] ->
	    ok;
	NotEmptyList ->
	    {error, corrupted_files, NotEmptyList}
    end.

check_files(_Pack, [], Corrupted) ->
    Corrupted;
check_files(Pack, [{Sha1, _Size, Filename} | T], Corrupted) ->
    {ok, Data} = get_file(Pack, Filename),
    case crypto:sha(Data) =:= Sha1 of
	true ->
	    check_files(Pack, T, Corrupted);
	false ->
	    check_files(Pack, T, [Filename | Corrupted])
    end.