var importObject = {
    imports: {
        print: arg => console.log(arg),
        print_pair: (a, b) => console.log(`(${a}, ${b})`)
    }
};

function printDisplay(display, n, width, height) {
    for (let row = 0; row < height; ++row) {
        let line = "";
        for (let col = 0; col < width; ++col) {
            line += `${display.getInt8(row * width + col + n * width * height)} `;
        }
        console.log(line);
    }
}

WebAssembly
    .instantiateStreaming(fetch('hello.wasm'), importObject)
    .then(obj => {
        let display = new DataView(obj.instance.exports.display.buffer);
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
            display.setInt8(row * height + col, 1);
        });

        printDisplay(display, 0, 5, 5);
        console.log("------------------------------");

        obj.instance.exports.next();
        printDisplay(display, 0, 5, 5);
        console.log("------------------------------");

        obj.instance.exports.next();
        printDisplay(display, 0, 5, 5);
        console.log("------------------------------");

        obj.instance.exports.next();
        printDisplay(display, 0, 5, 5);
        console.log("------------------------------");

        obj.instance.exports.next();
        printDisplay(display, 0, 5, 5);
        console.log("------------------------------");


        // for(let i = 0; i < 10; ++i) {
        //     console.log(obj.instance.exports.fib(i));
        // }
    });
