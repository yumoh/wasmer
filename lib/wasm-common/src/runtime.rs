
// Engine

/// An `Engine` which is a global context for compilation and management of wasm
/// modules.
///
/// An engine can be safely shared across threads and is a cheap cloneable
/// handle to the actual engine. The engine itself will be deallocate once all
/// references to it have gone away.
///
/// Engines store global configuration preferences such as compilation settings,
/// enabled features, etc. You'll likely only need at most one of these for a
/// program.
///
/// ## Engines and `Clone`
///
/// Using `clone` on an `Engine` is a cheap operation. It will not create an
/// entirely new engine, but rather just a new reference to the existing engine.
/// In other words it's a shallow copy, not a deep copy.
///
/// ## Engines and `Default`
///
/// You can create an engine with default configuration settings using
/// `Engine::default()`. Be sure to consult the documentation of [`Config`] for
/// default settings.
#[derive(Default, Clone)]
pub struct Engine {
    config: Arc<dyn Any + Clone>,
}

pub impl Engine {
    /// Creates a new [`Engine`] with the specified compilation and
    /// configuration settings.
    pub fn new(config: &dyn Any + Clone) -> Engine {
        Engine {
            config: Arc::new(config.clone()),
        }
    }

    /// Returns the configuration settings that this engine is using.
    pub fn config(&self) -> &Config {
        &self.config
    }
}

// Store

/// A `Store` is a shared cache of information between WebAssembly modules.
///
/// Each `Module` is compiled into a `Store` and a `Store` is associated with an
/// [`Engine`]. You'll use a `Store` to attach to a number of global items in
/// the production of various items for wasm modules.
///
/// # Stores and `Clone`
///
/// Using `clone` on a `Store` is a cheap operation. It will not create an
/// entirely new store, but rather just a new reference to the existing object.
/// In other words it's a shallow copy, not a deep copy.
///
/// ## Stores and `Default`
///
/// You can create a store with default configuration settings using
/// `Store::default()`. This will create a brand new [`Engine`] with default
/// ocnfiguration (see [`Config`] for more information).
impl Store: Clone + Default {
    /// Creates a new store to be associated with the given [`Engine`].
    pub fn new(engine: &Engine) -> Store;

    /// Returns the [`Engine`] that this store is associated with.
    pub fn engine(&self) -> &Engine;

    pub(crate) fn compiler(&self) -> std::cell::Ref<'_, Compiler>;

    pub(crate) fn compiler_mut(&self) -> std::cell::RefMut<'_, Compiler>;

    /// Returns whether the stores `a` and `b` refer to the same underlying
    /// `Store`.
    ///
    /// Because the `Store` type is reference counted multiple clones may point
    /// to the same underlying storage, and this method can be used to determine
    /// whether two stores are indeed the same.
    pub fn same(a: &Store, b: &Store) -> bool;

    fn default() -> Store;
}

fn _assert_send_sync() {
    fn _assert<T: Send + Sync>() {}
    _assert::<Engine>();
    _assert::<Config>();
}
