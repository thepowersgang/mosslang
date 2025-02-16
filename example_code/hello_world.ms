
//#[link(name="main")]
extern "C" fn main() -> i32 {
    libc_print("Hello, World!")
}

fn libc_print(ptr: *const u8, len: usize) {
    printf("%.*s", len, ptr);
}
extern "C" {
    fn printf(fmt: *const u8, ...);
}
