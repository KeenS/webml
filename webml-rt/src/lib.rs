#![feature(lang_items)]
#![feature(stdsimd)]
#![no_std]
#![cfg(target_arch = "wasm32")]


use core::ptr;
use core::arch::wasm32::*;

#[repr(C)]
struct Page {
    top: usize,
    data: *mut u8
}

static mut GC: *mut Page = 0 as *mut _;


#[no_mangle]
pub unsafe extern "C" fn init() {
    let ret = grow_memory(1);
    let page_ptr: *mut Page = ((ret as u32) * page_size()) as *mut _;
    // for alignment
    let data_ptr: *mut u8 = page_ptr.offset(8) as *mut _;
    (*page_ptr).data = data_ptr;
    GC = page_ptr as *mut _;
}

#[no_mangle]
pub unsafe extern "C" fn alloc(size: usize) -> *mut u8 {
    let ret = (*GC).data.offset(size as isize);
    (*GC).top += size;
    ret
}

static mut HEAD: *mut *mut u8 = 0 as _;

#[no_mangle]
pub unsafe extern "C" fn page_alloc() -> *mut u8 {
    if !HEAD.is_null() {
        let next = *HEAD;
        let ret = HEAD;
        HEAD = next as *mut _;
        return ret as *mut u8;
    }

    let ret = grow_memory(1);

    // if we failed to allocate a page then return null
    if ret == -1 {
        return ptr::null_mut();
    }

    ((ret as u32) * page_size()) as *mut u8
}

#[no_mangle]
pub unsafe extern "C" fn page_free(page: *mut u8) {
    let page = page as *mut *mut u8;
    *page = HEAD as *mut u8;
    HEAD = page;
}

#[no_mangle]
pub unsafe extern "C" fn memory_used() -> usize {
    (page_size() * (current_memory() as u32)) as usize
}

fn page_size() -> u32 {
    64 * 1024
}

// provided by libstd.
#[lang = "eh_personality"] extern fn eh_personality() {}
#[lang = "panic_fmt"] extern fn panic_fmt() -> ! { loop {} }
