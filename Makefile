gol.wasm: gol.wat
	wat2wasm gol.wat

gol.wat: gol.rkt
	racket gol.rkt > gol.wat
