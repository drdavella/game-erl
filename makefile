ERL_COMPILER:=erlc

COMPONENTS:=main rom cpu memory jump utils
SOURCE_FILES:=$(foreach name,$(COMPONENTS),$(name).erl)
BEAM_FILES:=$(foreach name,$(COMPONENTS),$(name).beam)


.PHONY: all run clean


all:
	@erl -make

run: all
	@erl -pa ebin -s game_erl -s init stop

clean:
	@rm -f ebin/*.beam
