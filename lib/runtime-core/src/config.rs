use crate::module::ResourceIndex;

pub struct Allowed {
    pub float_ops: bool,
    pub indirect_calls: bool,

    #[doc(hidden)]
    #[deprecated(note = "This field exists to make this struct non-exhaustive.")]
    pub __non_exhaustive: (),
}

impl Default for Allowed {
    fn default() -> Self {
        #[allow(deprecated)]
        Self {
            float_ops: true,
            indirect_calls: true,
            __non_exhaustive: (),
        }
    }
}

pub struct Metering {
    #[doc(hidden)]
    #[deprecated(note = "This field exists to make this struct non-exhaustive.")]
    pub __non_exhaustive: (),
}

impl Default for Metering {
    fn default() -> Self {
        #[allow(deprecated)]
        Self {
            __non_exhaustive: (),
        }
    }
}

/// Configuration data for the compiler
pub struct CompileConfig<'a> {
    pub symbol_map: Option<Box<dyn Fn(ResourceIndex) -> Option<String>>>,
    pub metering: &'a Metering,
    pub allowed: &'a Allowed,
}
