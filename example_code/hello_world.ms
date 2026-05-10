
extern crate libc;

//#[link(name="main")]
extern "C" fn main() -> i32 {
    libc_print("Hello, World!", 5+2+5+1);
    0
}

fn libc_print(ptr: *const [i8; ...], len: usize) {
    ::libc::stdio::printf("%.*s", len, ptr);
}
