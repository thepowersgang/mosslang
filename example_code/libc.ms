// cspell:disable

pub type c_char = i8;
pub type c_STR = [c_char; ...];

// stdio.h
pub mod stdio {
    //use super::c_STR;
    type c_STR = super::c_STR;

    pub struct FILE { ... }
    pub const SEEK_END: i32 = -1;

    extern "C" {
    	pub fn perror(msg: *const c_STR);
        
	    pub fn printf(fmt: *const c_STR, ...);

        pub fn fprintf(_: *mut FILE, fmt: *const c_STR, ...);
        //pub static stderr: *mut FILE;
        pub fn fopen(path: *const c_STR, mode: *const c_STR) -> *mut FILE;
        pub fn fseek(fp: *mut FILE, ofs: i64, dir: i32);
        pub fn ftell(fp: *mut FILE) -> u64;
        pub fn fgetc(fp: *mut FILE) -> i32;
        pub fn ungetc(ch: i32, fp: *mut FILE);
        pub fn fclose(fp: *mut FILE);
    }
}

	
// stdlib.h
pub mod stdlib {
    type c_STR = super::c_STR;

    extern "C" {
        pub fn getenv(name: *const c_STR) -> *mut c_STR;
        pub fn exit(status: i32);// -> !;
        pub fn malloc(size: usize) -> *mut void;	// TODO: Make a type that can be coerced to anything (`void` can coerce _from_ anything)
        pub fn realloc(old: *mut void, size: usize) -> *mut void;
        pub fn free(ptr: *const void);
    }
}

pub mod errno {
    extern "C" {
        //pub static errno: i32;
    }
}

// string.h
pub mod string {
    type c_STR = super::c_STR;
    type c_char = super::c_char;
    
    extern "C" {
        pub fn strerror(errnum: i32) -> *mut c_STR;
        pub fn strlen(s: *const c_STR) -> usize;
        pub fn strcmp(s1: *const c_STR, s2: *const c_STR) -> i32;
        pub fn strncmp(s1: *const c_STR, s2: *const c_STR, max_len: usize) -> i32;
        pub fn strcpy(d: *mut c_STR, s: *const c_STR) -> *mut c_STR;
        pub fn strstr(s1: *const c_STR, s2: *const c_STR) -> *const c_STR;
        pub fn strchr(s1: *const c_STR, c: c_char) -> *const c_STR;
        pub fn strrchr(s1: *const c_STR, c: c_char) -> *const c_STR;
        pub fn memcmp(s1: *const void, s2: *const void, count: usize) -> i32;
        pub fn memcpy(dst: *mut void, src: *const void, count: usize) -> *mut void;
    }
}

// ctype.h
pub mod ctype {
    extern "C" {       
        pub fn isalnum(ch: i32) -> bool;
        pub fn isblank(ch: i32) -> bool;
    }
}