fn main() {
    libc_print("Hello, World!")
}

fn libc_print(string: *const [u8]) {
    let ptr = ::builtin::dst_ptr(string);
    let len = ::builtin::dst_len(string);
    printf("%.*s", len, ptr);
}
extern "C" {
    fn printf(fmt: *const u8, ...);
}
