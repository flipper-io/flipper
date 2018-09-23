/// Generates Rust wrapper implementations of Flipper modules.
///
/// A Flipper function invocation is simply a description of a function to execute. Namely, an
/// invocation contains the following information:
///
///   1) The name of the module which the function belongs to.
///   2) The index of the function within the module.
///   3) The values and types of the arguments to the function.
///   4) The expected return type of the function.
///
/// The implementation of any remote invocation can be directly derived from the signature
/// of the function being executed. This macro accepts a description of the function index and
/// signature as input and generates the implementation for performing remote invocation as output.
///
/// Below is an example demonstrating how one would generate the implementation for the LED module.
///
/// ```
/// flipper_module! (Led, led,
///     0, fn configure() -> (),
///     1, fn rgb(red: u8, green: u8, blue: u8) -> (),
/// );
/// ```
///
/// This macro would generate the following code:
///
/// ```
/// pub mod led {
///     use flipper::{lf, Flipper};
///     pub struct Led<'a> {
///         flipper: &'a Flipper,
///     }
///
///     impl<'a> Led<'a> {
///         pub fn new(flipper: &'a Flipper) -> Led<'a> { Led { flipper } }
///
///         pub fn configure(&self) -> () {
///             let args = lf::Args::new();
///             lf::invoke(&self.flipper, "led", 0, args)
///         }
///
///         pub fn rgb(&self, red: u8, green: u8, blue: u8) -> () {
///             let args = lf::Args::new();
///             let args = args.append(red);
///             let args = args.append(green);
///             let args = args.append(blue);
///             lf::invoke(&self.flipper, "led", 1, args)
///         }
///     }
/// }
/// ```
///
/// A consumer of the generated module could then use it like this:
///
/// ```
/// use flipper::Flipper;
/// use led::Led;
///
/// let flipper = Flipper::attach().unwrap();
/// let led = Led::new(&flipper);
/// led.configure();
/// led.rgb(10, 10, 0);
/// ```
#[macro_export]
macro_rules! flipper_module (
    ($name:ident, $ns:ident,
        $(
            $idx:expr, fn $func:ident ( $($args:tt)* ) -> $ret:ty
        ),*$(,)*
    ) => {
        pub mod $ns {
            use $crate::{lf, Flipper};
            pub struct $name<'a> {
                flipper: &'a Flipper,
            }

            impl<'a> $name<'a> {
                pub fn new(flipper: &'a Flipper) -> $name<'a> { $name { flipper } }

                $(
                    pub fn $func (&self, $($args)*) -> $ret {
                        __flipper_module_func_impl!(self, stringify!($ns), $idx, $($args)*)
                    }
                )*
            }
        }
    }
);

#[doc(hidden)]
#[macro_export]
macro_rules! __flipper_module_func_impl (
    ($self_:ident, $key:expr, $idx:expr, $($name:ident: $typ:ty),*$(,)*) => {{
        let args = lf::Args::new();
        $(
            let args = args.append($name);
        )*
        lf::invoke(&$self_.flipper, $key, $idx, args)
    }}
);
