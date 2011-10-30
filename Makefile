ROOT_DIR=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))
TEST_MODULES="[kzk_pack]"

default: kzkpack

all: beamfiles kzkpack docs tests

beamfiles:
	@cd $(ROOT_DIR); erl -make

tests: beamfiles
	@cd $(ROOT_DIR); erl -noshell -pa ebin -eval "halt(case eunit:test($(TEST_MODULES), [verbose]) of ok -> 0; _ -> 1 end)."

docs:
	@cd $(ROOT_DIR); erl -noshell -env ERL_LIBS "." -eval "halt(case edoc:application(kzk_pack, \".\", []) of ok -> 0; _ -> 1 end)."

kzkpack: beamfiles
	@cd $(ROOT_DIR); echo "#!/usr/bin/escript" > kzkpack
	@cd $(ROOT_DIR); echo "%% -*- erlang -*-" >> kzkpack
	@cd $(ROOT_DIR); cat ebin/kzk_pack_cli.beam >> kzkpack
	@cd $(ROOT_DIR); chmod +x kzkpack

clean:
	@cd $(ROOT_DIR); rm ebin/*.beam
	@cd $(ROOT_DIR); rm -R doc

.PHONY: beamfiles tests kzkpack docs default clean