let context = document
    .getElementById('display')
    .getContext('2d');

WebAssembly
    .instantiateStreaming(fetch('hello.wasm'), {
        imports: {
            print: arg => console.log(arg),
            print_pair: (a, b) => console.log(`(${a}, ${b})`),
            line: (x1, y1, x2, y2) => {
                context.strokeStyle = "red";
                context.moveTo(x1, y1);
                context.lineTo(x2, y2);
                context.stroke();
            },
        }
    })
    .then(obj => {
        let display =
            new DataView(obj.instance.exports.display.buffer);
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

        obj.instance.exports.render(640, 480);

        // for(let i = 0; i < 10; ++i) {
        //     console.log(obj.instance.exports.fib(i));
        // }
    });
