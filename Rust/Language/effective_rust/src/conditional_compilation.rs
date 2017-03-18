/*
cfg lets you compile code based on a flag passed to the compiler

#[cfg(foo)]
#[cfg(bar = "baz")]

helpers

#[cfg(any(unix,windows))]
#[cfg(all(unix, target_pointer_width = "32"))]
#[cfg(not(foo))]

these can nest

#[cfg(any(not(unix), all(target_os="macos", target_arch = "powerpc")))]

enable using cargo in Cargo.toml

[features]
# no features by default
default = []

# add feature foo, then you can use it
# foo depends on nothing else
foo = []

cargo passes to rustc: --cfg feature="${feature_name}"

the sum of these cfg flags will determine which ones get activated, and which code gets compiled

#[cfg(feature = "foo")]
mod foo {

}

if we compile it with cargo build --features "foo", it will send the
--cfg feature="foo" to the rustc and the output will have the mod foo in it
if we compile with regular cargo build, no extra flags get passed on and no foo module will exist

#[cfg_attr(a,b)]
will be the same as #[b] if a is set by cfg attribute and nothing otherwise

cfg! macro lets you use these flags elsewhere in your code, too:
if cfg!(target_on = "macos") || cfg!(target_os = "ios") {
    println!("Think different!");
}

these will be replaced by true or false at compile-time, depending on the configuration settings
*/
