ERL_COMPILER:=erlc
ENTRY_POINT:=main


.PHONY: all run clean

all: $(ENTRY_POINT).beam


run: main.beam
	@erl -noshell -s $(ENTRY_POINT) start -s init stop

clean:
	@rm -f main.beam

%.beam: %.erl
	@echo BUILDING $<
	@erlc $^
