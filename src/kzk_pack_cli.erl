% Author: Romain "Artefact2" Dalmaso <artefact2@gmail.com>
%
% This program is free software. It comes without any warranty, to the
% extent permitted by applicable law. You can redistribute it and/or
% modify it under the terms of the Do What The Fuck You Want To Public
% License, Version 2, as published by Sam Hocevar. See
% http://sam.zoy.org/wtfpl/COPYING for more details.

-module(kzk_pack_cli).
-include_lib("kernel/include/file.hrl").
-export([main/1]).

-define(warning(Format, Args), io:format(standard_error, string:concat("kzkpack: ", Format), Args)).

main(Args) ->
    code:add_path(string:concat(filename:dirname(escript:script_name()), "/ebin")),
    case Args of
	["create", PackName] ->
	    create_pack(PackName, fun next_from_stdin/1, [], create);
	["create", PackName, "-"] ->
	    create_pack(PackName, fun next_from_stdin/1, [], create);
	["create", PackName | List] ->
	    create_pack(PackName, fun next_from_list/1, List, create);
	["append", PackName] ->
	    create_pack(PackName, fun next_from_stdin/1, [], append);
	["append", PackName, "-"] ->
	    create_pack(PackName, fun next_from_stdin/1, [], append);
	["append", PackName | List] ->
	    create_pack(PackName, fun next_from_list/1, List, append);
	["cat", PackName, Filename] ->
	    cat_from_pack(PackName, Filename);
	["integrity-check", PackName] ->
	    integrity_check(PackName);
	["ls", PackName] ->
	    list(PackName);
	["checksums", PackName] ->
	    list_checksums(PackName);
	_ ->
	    usage()
    end.

usage() ->
    {ok, Usage} = file:read_file(string:concat(filename:dirname(escript:script_name()), "/README.usage")),
    io:format("~s", [Usage]).

list(PackName) ->
    {ok, Pack} = kzk_pack:open(PackName, true),
    {ok, List} = kzk_pack:list(Pack),
    ok = print_list(List),
    ok = kzk_pack:dispose(Pack),
    ok.

print_list([]) ->
    ok;
print_list([{_Sha1, Size, Name} | T]) ->
    ok = io:format("~w~c~ts~n", [Size, 9, Name]), % 9 = ASCII <Tab> (\t)
    print_list(T).

list_checksums(PackName) ->
    {ok, Pack} = kzk_pack:open(PackName, true),
    {ok, List} = kzk_pack:list(Pack),
    ok = print_checksums(List),
    ok = kzk_pack:dispose(Pack),
    ok.

print_checksums([]) ->
    ok;
print_checksums([{Sha1, _Size, Name} | T]) ->
    ok = io:format("~s  ~ts~n", [list_to_hex_list(binary_to_list(Sha1)), Name]),
    print_checksums(T).

hex_to_ascii(X) when X >= 0 andalso X =< 9 ->
    $0 + X;
hex_to_ascii(X) when X >= 10 andalso X =< 15 ->
    $a + (X - 10).

list_to_hex_list(List) ->
    list_to_hex_list(List, []).

list_to_hex_list([], Acc) ->
    lists:reverse(Acc);
list_to_hex_list([H | T], Acc) ->
    R = hex_to_ascii(H rem 16),
    D = hex_to_ascii(H div 16),
    list_to_hex_list(T, [R, D | Acc]).

integrity_check(PackName) ->
    {ok, Pack} = kzk_pack:open(PackName, true),
    case kzk_pack:list(Pack) of
	{ok, _Pack} ->
	    io:format("kzkpack: integrity check successful.~n"),
	    halt(0);
	{error, Error} ->
	    ?warning("integrity check failed: ~w~n", [Error]),
	    halt(1)
    end.

next_from_list([]) ->
    {[], []};
next_from_list([H | T]) ->
    {H, T}.

next_from_stdin([]) ->
    case io:get_line(standard_io, "") of
	eof ->
	    {[], []};
	Data ->
	    {string:strip(unicode:characters_to_list(Data), right, 10), []}
    end.

is_filename_safe(H) ->
    case string:str(H, "/") of
	1 ->
	    ?warning("omitting path with trailing \"/\": ~ts~n", [H]),
	    false;
	_ ->
	    case string:str(H, "../") of
		0 ->
		    true;
		_ ->
		    ?warning("omitting path containing \"..\": ~ts~n", [H]),
		    false
	    end
    end.

is_regular_file(H) ->
    case file:read_file_info(H) of
	{error, Reason} ->
	    ?warning("omitting file (could not read file info: ~w): ~ts~n", [Reason, H]),
	    false;
	{ok, FileInfo} ->
	    case FileInfo#file_info.type of
		regular ->
		    true;
		NonRegular ->
		    ?warning("omitting file of type ~w: ~ts~n", [NonRegular, H]),
		    false
	    end
    end.

is_not_in_pack(Pack, H) ->
    case kzk_pack:has_file(Pack, H) of
	true ->
	    ?warning("omitting file already in archive: \"~s\"~n", [H]),
	    false;
	false -> true
    end.

create_pack(PackName, Next, NAcc, Mode) ->
    {ok, Pack} = case Mode of
		     create -> kzk_pack:create(PackName);
		     append -> kzk_pack:open(PackName, false)
		 end,
    NewPack = append_files_to_pack(Pack, Next, NAcc),
    {ok, NewPack2} = kzk_pack:commit(NewPack),
    ok = kzk_pack:dispose(NewPack2).

append_files_to_pack(Pack, Next, Acc) ->
    {F, NewAcc} = Next(Acc),
    case F =:= [] of
	true ->
	    Pack;
	false ->
	    case is_filename_safe(F) andalso is_regular_file(F) andalso is_not_in_pack(Pack, F) of
		false ->
		    append_files_to_pack(Pack, Next, NewAcc);
		true ->
		    io:format("A ~ts", [F]),
		    {ok, NewPack} = kzk_pack:append_file_raw(Pack, F, F),
		    io:format("~n"),
		    append_files_to_pack(NewPack, Next, NewAcc)
	    end
    end.

cat_from_pack(PackName, Filename) ->
    {ok, Pack} = kzk_pack:open(PackName, true),
    cat_from_pack(standard_io, Pack, Filename, 0).
    
cat_from_pack(IoDevice, Pack, Filename, Offset) ->
    case kzk_pack:get_file(Pack, Filename, Offset, 64 * 1024) of
	{ok, eof} ->
	    ok;
	{ok, Data} ->
	    file:write(IoDevice, Data),
	    cat_from_pack(IoDevice, Pack, Filename, Offset + 64 * 1024);
	{error, file_not_found} ->
	    ?warning("file not found in pack: \"~s\"~n", [Filename])
    end.
