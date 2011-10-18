TEST_MODULES="[kzk_pack]"

all: beamfiles kzkpack

ebin:
	@mkdir ebin

beamfiles: ebin
	@erl -make

tests: beamfiles
	@erl -noshell -pa ebin -eval "halt(case eunit:test($(TEST_MODULES), [verbose]) of ok -> 0; _ -> 1 end)."

kzkpack: beamfiles
	@echo "#!/usr/bin/escript" > kzkpack
	@echo "%% -*- erlang -*-" >> kzkpack
	@cat ebin/kzk_pack_cli.beam >> kzkpack
	@chmod +x kzkpack

.PHONY: beamfiles tests kzkpack