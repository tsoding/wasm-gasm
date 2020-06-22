let fib = document.getElementById('fib');

WebAssembly
    .instantiateStreaming(fetch('fib.wasm'), {
        imports: {
            print: (x) => {
                let p = document.createElement('p');
                p.innerText = x;
                fib.appendChild(p);
            }
        }
    })
    .then(obj => {
        obj.instance.exports.fib(10);
    });
