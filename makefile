ERL_COMPILER:=erlc
ENTRY_POINT:=main

COMPONENTS:=main cpu
SOURCE_FILES:=$(foreach name,$(COMPONENTS),$(name).erl)
BEAM_FILES:=$(foreach name,$(COMPONENTS),$(name).beam)


.PHONY: all run clean


all: $(SOURCE_FILES) $(BEAM_FILES)


run: all
	@erl -noshell -s $(ENTRY_POINT) start -s init stop

clean:
	@rm -f $(BEAM_FILES)

%.beam: %.erl
	@echo BUILDING $<
	@erlc $^
