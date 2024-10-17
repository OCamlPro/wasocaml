import { readFile } from 'fs/promises';

const memory = new WebAssembly.Memory({
  initial: 1,
  maximum: 1,
});

function print_string(str) {
  let res = "";
  for (let i = 0; i < get_length(str); i++) {
    res = res + String.fromCharCode(get_char(str, i));
  }
  process.stdout.write(res);
};

let str_buff = "";

function print_string_mem(off, len) {
  const buff = new Uint8Array(memory.buffer);
  for (let i = off; i < len + off; i++) {
    let char = String.fromCharCode(buff[i]);
    str_buff = str_buff + char;
  }
};

function print_i32(arg) {
  str_buff = str_buff + arg.toString();
};

function print_f64(arg) {
  process.stdout.write(arg);
};

function print_endline() {
  process.stdout.write(str_buff);
  str_buff = "";
}

function putchar(i_char) {
  let char = String.fromCharCode(i_char);
  str_buff = str_buff + char;
};

function flush() {
  process.stdout.write(str_buff);
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
  "memory": memory,
  "atan2": Math.atan2,
  "sin": Math.sin,
  "cos": Math.cos,
}

const src = "./a.out.wasm"

const code = await readFile(src);
const imports = {"js_runtime":bindings}


async function f() {

  const wasmModule = await WebAssembly.instantiate(code, imports).then(module => {
    //process.stdout.write("module loaded!");
    //for (let key in module.instance.exports) {
    //  process.stdout.write(key);
    //}
    //process.stdout.write("done!");
  });
}

f();
