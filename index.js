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
            fill_rect: (x, y, w, h) => {
                context.fillStyle = "red";
                context.fillRect(x, y, w, h);
            }
        }
    })
    .then(obj => {
        obj.instance.exports.init();
        obj.instance.exports.next();
        obj.instance.exports.render(640, 480);
    });
