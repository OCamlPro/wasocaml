import { readFile } from 'fs/promises';

function print_string(str) {
  let res = "";
  for (let i = 0; i < get_length(str); i++) {
    res = res + String.fromCharCode(get_char(str, i));
  }
  process.stdout.write(res);
};

let str_buff = "";

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
  "print_endline": print_endline,
  "putchar": putchar,
  "flush": flush,
  "atan2": Math.atan2,
  "sin": Math.sin,
  "asin": Math.asin,
  "fmod": (x, y) => x % y,
  "cos": Math.cos,
}

const src = "./a.out.wasm"

const code = await readFile(src);
const imports = {
  "js_runtime" : bindings
}

async function f() {

  const wasmModule = await WebAssembly.instantiate(code, imports).then(module => {
  });
}

f();
