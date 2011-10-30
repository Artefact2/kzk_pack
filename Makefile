ROOT_DIR=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))
TEST_MODULES="[kzk_pack]"
VERSION=$(shell cd $(ROOT_DIR); git describe --match "v*" 2>/dev/null | sed -e "s/^v//")

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

tarball_source: kzk_pack-$(VERSION).tar

kzk_pack-$(VERSION).tar:
	@cd $(ROOT_DIR); git archive --prefix=kzk_pack-$(VERSION)/ -o kzk_pack-$(VERSION).tar v$(VERSION)
	@echo $@

.PHONY: beamfiles tests kzkpack docs default clean tarball_source