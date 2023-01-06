use self::gc::Gc;

mod gc;
mod prelude;
mod string_pool;

/// Oceanus Runtime
#[derive(Default)]
pub struct Oceanus<T> {
    gc: Gc<T>,
}

impl<T> Oceanus<T> {
    fn new() -> Self {
        Self { gc: Gc::new() }
    }
}
