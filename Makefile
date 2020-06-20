hello.wasm: hello.wat
	wat2wasm hello.wat

hello.wat: hello.rkt
	racket hello.rkt > hello.wat
