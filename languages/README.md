# Languges

Due to the versatility of the Flipper platform, we are able to create language
bindings that extend the capabilities of the Flipper hardware to higher level
programming languages. Our goal is to be able to support any programming language.

Right now, all of the language bindings use the Foreign Function Interface (FFI)
of the target language to communicate with libflipper. Once Flipper is stable,
we will begin to work towards reimplementing libflipper in each of the languages
we support to eliminate the need to install additional C libraries or worry about
cross platform implementation.

All of the programming language bundings are still under development. There may be
bugs or things that don't work as expected. If you want to help change that, see
[this](https://github.com/flipper-io/flipper/blob/master/CONTRIBUTING.md#high-level-language-bindings)
page.

### Status

Here's a cheat sheet that you can use to determine which languages Flipper supports
on each platform. Windows support is on the way.

| Platform | C | C++ | Objective-C | Swift | Python | Java | Javascript | Haskell | Rust |
| :---: | :---: | :---: | :---: | :---: | :---: |  :---: | :---: | :---: | :---: |
| **Linux** | ✓ | ✓ | 𐄂 | 𐄂 | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Mac OS** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **iOS/Watch/AppleTV** | ✓ | ✓ | ✓ | ✓ | 𐄂 | 𐄂 | 𐄂 | 𐄂 | 𐄂 |
| **Android** | ✓ | ✓ | 𐄂 | 𐄂 | 𐄂 | ✓ | 𐄂 | 𐄂 | 𐄂 |
| **Web** | 𐄂 | 𐄂 | 𐄂 | 𐄂 | 𐄂 | 𐄂 | ✓ | 𐄂 | 𐄂 |
| **Windows** | 𐄂 | 𐄂 | 𐄂 | 𐄂 | 𐄂 | 𐄂 | 𐄂 | 𐄂 | 𐄂 |