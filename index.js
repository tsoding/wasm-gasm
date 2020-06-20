var importObject = {
    imports: {
        print: arg => console.log(arg),
        print_pair: (a, b) => console.log(`(${a}, ${b})`)
    }
};

WebAssembly
    .instantiateStreaming(fetch('hello.wasm'), importObject)
    .then(obj => {
        let dataView = new DataView(obj.instance.exports.display.buffer);
        // .o.
        // ..o
        // ooo
        const glider = [
            [0, 1],
            [1, 2],
            [2, 0],
            [2, 1],
            [2, 2]
        ];

        const height = 5;
        glider.forEach(([row, col]) => {
            dataView.setInt8(row * height + col, 1);
        });

        console.log(obj);
        // for(let i = 0; i < 10; ++i) {
        //     console.log(obj.instance.exports.fib(i));
        // }
    });
