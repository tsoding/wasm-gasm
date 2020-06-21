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
        console.log("neighbors(0, 0) => " + obj.instance.exports.neighbors(0, 0));
        console.log("neighbors(1, 1) => " + obj.instance.exports.neighbors(1, 1));
        // for(let i = 0; i < 10; ++i) {
        //     console.log(obj.instance.exports.fib(i));
        // }
    });
