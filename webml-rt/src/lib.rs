#![no_std]
#![cfg(target_arch = "wasm32")]
use core::arch::wasm32::{memory_grow, memory_size};
use core::mem;
use core::panic::PanicInfo;

#[repr(C)]
struct Page {
    next: *mut Page,
    size: usize,
    top: usize,
    data: *mut u8,
}

const MEMORY: u32 = 0;
const WASM_PAGE_SIZE: usize = 64 * 1024;
// GC page size including meta data
const GC_PAGE_SIZE: usize = 1 * WASM_PAGE_SIZE;
static mut GC: *mut Page = 0 as *mut _;
static mut HEAD: *mut Page = 0 as *mut _;

unsafe fn new_page() -> *mut Page {
    let ret = memory_grow(MEMORY, 1);
    // if we failed to allocate a page then panic
    if ret == usize::max_value() {
        // TODO: collect garbage
        panic!("memory exhausted")
    } else {
        let page = (ret * WASM_PAGE_SIZE) as *mut u8 as *mut Page;
        // next, top: relying wasm's page is 0 initialized
        (*page).size = GC_PAGE_SIZE - mem::size_of::<Page>();
        (*page).data = (page as *mut u8).offset(mem::size_of::<Page>() as isize);
        page
    }
}

unsafe fn add_new_page() {
    let page = new_page();
    (*HEAD).next = page;
    HEAD = page;
}

#[no_mangle]
pub unsafe extern "C" fn init() {
    let page_ptr = new_page();
    GC = page_ptr;
    HEAD = GC;
}

#[no_mangle]
pub unsafe extern "C" fn alloc(size: usize) -> *mut u8 {
    if (*HEAD).size <= (*HEAD).top + size {
        add_new_page();
    }
    let ret = (*HEAD).data.offset((*HEAD).top as isize);
    (*GC).top += size;
    ret
}

#[no_mangle]
pub unsafe extern "C" fn memory_used() -> usize {
    WASM_PAGE_SIZE * memory_size(MEMORY)
}

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    // currently no way to handle panic
    loop {}
}
