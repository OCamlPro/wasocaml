
const memory = new WebAssembly.Memory({
  initial: 1,
  maximum: 1,
});

function print_string(str) {
        console.log('print_string');
        var res = "";
        for (i = 0; i < get_length(str); i++) {
            res = res + String.fromCharCode(get_char(str, i));
        }
        console.log(res);
    };
var str_buff = "";
function print_string_mem(off, len) {
        // console.log('print_string_mem');
        const buff = new Uint8Array(memory.buffer);
        // console.log(buff);
        var i = 0;
        for (i = 0; i < len; i++) {
            var char = String.fromCharCode(buff[i+off]);
            str_buff = str_buff + char;
        }
    };

function print_i32(arg) {
        str_buff = str_buff + arg.toString();
    };
function print_f64(arg) {
        console.log(arg);
    };

function print_endline() {
    console.log(str_buff);
    str_buff = "";
}

function putchar(i_char) {
    var char = String.fromCharCode(i_char);
    str_buff = str_buff + char;
};

function flush() {
    console.log(str_buff);
    str_buff = "";
}

const bindings = {
    "print_i32": print_i32,
    "print_f64": print_f64,
    "print_string": print_string,
    "print_string_mem": print_string_mem,
    "print_endline": print_endline,
    "putchar": putchar,
    "flush": flush,
    "memory": memory
}

const src = "./a.out.wasm"
const code = fetch(src, {
    // ...
    referrerPolicy: "unsafe-url"
});
const imports = {"js_runtime":bindings}
const wasmModule = await WebAssembly.instantiateStreaming(code, imports).then(module => {
    console.log("module loaded! listing its exports:");
    for (let key in module.instance.exports) {
            console.log(key);
    }
    console.log("done!");
});
