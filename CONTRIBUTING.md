# Contributing to Flipper

Thank you for your interest in contributing to Flipper! Flipper is a growing project and contribution is welcome. There are many moving parts that must come together to make our platform work. You can read about these components, from highest to lowest level, below.

## High Level Language Bindings

**Language -** Any

**Source -** [`languages`](../../tree/master/languages)

The capabilities of the Flipper hardware are extended to higher level programming languages via bindings to our platform level library. Development of bindings for the following languages are already underway.

- C
- C++
- Objective-C
- Swift
- Rust
- Python
- Java
- Javascript
- Haskell

Work needs to be done to make interacting with emebedded hardware more idiomatic in the above languages. Contributions of bindings for languages not listed here are also welcome! Please visit [this](https://github.com/georgemorgan/flipper/wiki/Creating-a-Language-Binding) Wiki page to learn more about how to create a language binding.

## Console

**Language -** Rust

**Source -** [`console`](../../tree/master/console)

The Flipper console is the user interface of the Flipper board. It is a command line application written in Rust and is responsible for the following:

- Flash firmware images to the hardware
- Build and distribute Flipper packages
- Generate language bindings to Flipper packages

Work needs to be done to improve language binding generation, improve management of the build system used to compile packages for the device, and more. To learn more about the console, see the Wiki page here.

## libflipper

**Language -** C

**Source -** [`library`](../../tree/master/library)

The platform level library, libflipper, is the foundation upon which higher level language bindings are constructed. It provides all of the boilerplate needed to talk to Flipper hardware over a variety of endpoints (USB, network, etc). It is reposnible for the following:

- Construct message runtime packets and send them to the hardware
- Define and managing the data structures that communicate information between the host and device
- Load and update packages, and launch applications

Work needs to be done to improve error handling, improve load times, memory footprint, and more. To learn more about libflipper, see the Wiki page here.

## Firmware / Kernel

**Language -** C

**Source -** [`kernel`](../../tree/master/kernel)

The fimware and kernel images are the two most integral software components of the Flipper platform. The firmware for the microcontroller manages communications with the microprocessor, and the kernel image for the microprocessor schedules the execution of applications, implements a loader for dynamic libraries, and controls the device peripherals.

- Load applications and packages
- Schedule program execution
- Expose hardware drivers and API

## Board Support Packages

**Language -** C

**Source -** [`carbon`](../../tree/master/carbon)

A board support packages contains the code that interacts with a chip's registers to control hardware peripherals. All of the drivers, along with their APIs, are exposed in the board support package published for the Flipper boards we support. Currently, Carbon is the only Flipper device.

- Peripheral drivers
- Hardware abstraction layer
- Platform specific code

To learn more about the Carbon device, see the Wiki page here.

## Hardware

The hardware is the bridge between the software stack and the real world. Currently, the hardware designs for the Carbon board are not open source. Hardware contributions are welcome for backpacks, small expansion boards that plug into the Carbon platform and extend the device's functionality.

- WiFi Backpack
- Sensor Backpack
- Display Backpack

To learn more about the expansion interface, see the Wiki page here.
