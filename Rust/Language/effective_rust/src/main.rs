mod stack_and_heap;
mod iterators;
mod concurrency;
mod error_handling_basics;
mod error_handling_multiple_types;
mod error_handling_library_traits;
mod choosing_guarantees;
mod borrow_and_asref;

fn main() {
    stack_and_heap::stack_and_heap();
    iterators::iterators();
    concurrency::concurrency();
    error_handling_basics::error_handling_basics();
    error_handling_multiple_types::error_handling_multiple_types();
    error_handling_library_traits::error_handling_library_traits();
    choosing_guarantees::choosing_guarantees();
    borrow_and_asref::borrow_asref();
}
