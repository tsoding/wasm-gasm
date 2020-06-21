let display = document.getElementById('display');
let context = display.getContext('2d');

function nextFrame(obj) {
    obj.instance.exports.next();
    obj.instance.exports.render(display.width, display.height);
    setTimeout(nextFrame, 100, obj);
}

WebAssembly
    .instantiateStreaming(fetch('hello.wasm'), {
        imports: {
            print: arg => console.log(arg),
            fill_rect: (x, y, w, h) => {
                context.fillStyle = "red";
                context.fillRect(x, y, w, h);
            },
            clear: () => {
                context.clearRect(0, 0, display.width, display.height);
            },
            rand: () => {
                return Math.random() * 2147483648;
            }
        }
    })
    .then(obj => {
        obj.instance.exports.init();
        setTimeout(nextFrame, 500, obj);
    });
