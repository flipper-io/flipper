# Languges

`libflipper` is the platform level library that gives developers access to
Flipper hardware.

### Quickstart

#### C example

```c
flipper_attach();
gpio.enable(IO_1, 0); // Enable IO1
gpio.write(IO_1, 0); // Set IO1 HIGH
```

#### Python example

```python
from flipper import *
lf.attach()
gpio.enable(gpio.IO_1, 0) # Enable IO1
gpio.write(gpio.IO_1, 0) # Set IO1 HIGH
```

#### Networking

You can also connect to a Flipper device over a network.

```c
carbon_attach_hostname("192.168.1.1");
gpio.enable(IO_1, 0); // Enable IO1
gpio.write(IO_1, 0); // Set IO1 HIGH
```

Simple.

If you don't have a WiFi shield for your device, you can test `libflipper`
right now locally using the Flipper Virtual Machine (FVM). After installing
the`utils`, simply run FVM and then attach to `localhost`.

```c
carbon_attach_hostname("localhost");
gpio.enable(IO_1, 0); // Enable IO1
gpio.write(IO_1, 0); // Set IO1 HIGH
```

You should see a message printed saying `Turning GPIO pin 8 ON.` FVM is
excellent for debugging changes made to the message runtime or `libflipper`
itself.

To learn more about using fvm, see the WiKi page
[here](https://github.com/georgemorgan/flipper/wiki/Flipper-Virtual-Machine).
=======
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
>>>>>>> move api to top, move langs and library, arch->kernel, add tests, start device detection
