.PHONY: all run clean

all: main.beam


run: main.beam
	@erl -noshell -s main start -s init stop

clean:
	@rm -f main.beam

%.beam: %.erl
	@echo BUILDING $<
	@erlc $^
