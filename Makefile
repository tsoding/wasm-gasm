.PHONY: all
all: gol.wasm fib.wasm

%.wasm: %.wat
	wat2wasm $<

%.wat: %.rkt
	racket $< > $@
