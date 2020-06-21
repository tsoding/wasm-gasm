let display = document.getElementById('display');
let context = display.getContext('2d');

function nextFrame(obj) {
    obj.instance.exports.next();
    obj.instance.exports.render(display.width, display.height);
    setTimeout(nextFrame, 500, obj);
}

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
            },
            clear: () => {
                context.clearRect(0, 0, display.width, display.height);
            }
        }
    })
    .then(obj => {
        obj.instance.exports.init();
        setTimeout(nextFrame, 500, obj);
    });
