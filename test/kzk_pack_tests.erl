% Author: Romain "Artefact2" Dalmaso <artefact2@gmail.com>
%
% This program is free software. It comes without any warranty, to the
% extent permitted by applicable law. You can redistribute it and/or
% modify it under the terms of the Do What The Fuck You Want To Public
% License, Version 2, as published by Sam Hocevar. See
% http://sam.zoy.org/wtfpl/COPYING for more details.

-module(kzk_pack_tests).
-include_lib("eunit/include/eunit.hrl").
-define(TEST_PACK_NAME, "test_pack.pack").
-define(TEST_FILE_NAME, "test_file.foo.bar____").
-define(TEST_FILE_CONTENTS, <<"Hello, World. This is a test-generated file.">>).

create_test_files() ->
    ok = file:write_file(?TEST_FILE_NAME, ?TEST_FILE_CONTENTS).

delete_test_pack(_) ->
    file:delete(?TEST_FILE_NAME),
    file:delete(?TEST_PACK_NAME).

test_empty_pack() ->
    {ok, Pack} = kzk_pack:create(?TEST_PACK_NAME),
    ok =  kzk_pack:integrity_check(Pack),
    {ok, []} = kzk_pack:list(Pack),
    {ok, Pack2} = kzk_pack:commit(Pack),
    {error, file_not_found} = kzk_pack:get_file(Pack2, "foo"),
    {error, file_not_found} = kzk_pack:get_file(Pack2, "foo", 42, infinity),
    {error, file_not_found} = kzk_pack:get_file(Pack2, "foo", 1, 1),
    false = kzk_pack:has_file(Pack2, "bar"),
    ok = kzk_pack:dispose(Pack2),
    {ok, Pack3} = kzk_pack:open(?TEST_PACK_NAME, true),
    {error, file_not_found} = kzk_pack:get_file(Pack3, "bar"),
    {error, file_not_found} = kzk_pack:get_file(Pack3, "bar", 2, infinity),
    {error, file_not_found} = kzk_pack:get_file(Pack3, "bar", 30, 30),
    false = kzk_pack:has_file(Pack3, "foo"),
    {ok, []} = kzk_pack:list(Pack3).

test_nonempty_pack() ->
    {ok, Pack1} = kzk_pack:create(?TEST_PACK_NAME),
    {ok, Pack2} = kzk_pack:append_file(Pack1, "foo0.txt", <<>>),
    {ok, Pack3} = kzk_pack:append_file(Pack2, "foo1.txt", <<"bar">>),
    {ok, Pack4} = kzk_pack:append_file(Pack3, "foo2.txt", <<"bar">>),
    {ok, Pack5} = kzk_pack:append_file(Pack4, "foo3.txt", <<"baz">>),
    {ok, Pack6} = kzk_pack:append_file_raw(Pack5, "ext.txt", ?TEST_FILE_NAME),
    {error, name_clash} = kzk_pack:append_file(Pack6, "foo0.txt", <<"not_empty">>),
    {ok, Pack7} = kzk_pack:append_file(Pack6, "foo0.txt", <<>>), % Inserting the SAME file twice is legal
    {ok, Pack} = kzk_pack:commit(Pack7),
    ok = kzk_pack:integrity_check(Pack),
    {ok, <<>>} = kzk_pack:get_file(Pack, "foo0.txt"),
    {ok, <<"bar">>} = kzk_pack:get_file(Pack, "foo1.txt"),
    {ok, <<"bar">>} = kzk_pack:get_file(Pack, "foo2.txt"),
    {ok, <<"baz">>} = kzk_pack:get_file(Pack, "foo3.txt"),
    {ok, <<"az">>} = kzk_pack:get_file(Pack, "foo3.txt", 1, infinity),
    {ok, <<"az">>} = kzk_pack:get_file(Pack, "foo3.txt", 1, 2),
    {ok, <<"z">>} = kzk_pack:get_file(Pack, "foo3.txt", 2, 2),
    {ok, eof} = kzk_pack:get_file(Pack, "foo3.txt", 3, 1),
    {ok, <<"ba">>} = kzk_pack:get_file(Pack, "foo3.txt", 0, 2),
    {ok, ?TEST_FILE_CONTENTS} = kzk_pack:get_file(Pack, "ext.txt"),
    true = kzk_pack:has_file(Pack, "foo1.txt"),
    false = kzk_pack:has_file(Pack, "nonexistent.txt"),
    ExtSize = size(?TEST_FILE_CONTENTS),
    {ok, 
     [{_Hash3, ExtSize, "ext.txt"},
      {_Hash, 0, "foo0.txt"}, 
      {Hash1, 3, "foo1.txt"}, 
      {Hash1, 3, "foo2.txt"}, 
      {_Hash2, 3, "foo3.txt"}]} = kzk_pack:list(Pack).

empty_pack_test_() ->
    {setup,
     fun () -> ok end,
     fun delete_test_pack/1,
     [fun test_empty_pack/0]
    }.

nonempty_pack_test_() ->
    {setup,
     fun create_test_files/0,
     fun delete_test_pack/1,
     [fun test_nonempty_pack/0]
    }.
