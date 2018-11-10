# Carbon: Hardware Abstraction Layer

This directory contains board-specific implementation for the
**Flipper: Carbon** hardware platform.

The firmware exposes API that enables [libflipper](../library) to control the
**Carbon** board's hardware peripherals. The Carbon board contains two CPUs: a
microcontroller and a microprocessor.

---

### ASF (Advanced Software Framework for Atmel SAM)

**Products: ** asf_4s.a

---

### Microcontroller (Atmel ATMEGA32U2) Firmware

The microcontroller connects to the host via USB since we wanted to keep the
microprocessor's USB controller available to user applications. The
microcontroller serves a number of functions.

- Control the power (ON/OFF) and reset state of the microprocessor
- Put the microprocessor into Device Firmware Update (DFU) mode
- Flash new firmware images to the microprocessor and verify them
- Halt the microprocessor and debug it using SWD (planned)
- Allow the host to talk to the device's filesystem
- Control the device's LED and button peripherals
- Send message runtime packets to the microprocessor
- Pass printf messages sent by the microprocessor back to the host

---

### Microprocessor (Atmel ATSAM4S16BA) Firmware

The microprocessor is the main CPU on the device.

- Load and run programs
- Schedule the execution of multiple running programs
- Communicate with the device's hardware peripherals
- Interact with backpacks plugged into the expansion connector

---

### HAL

The Hardware Abstraction Layer (HAL) exposes the **Carbon** hardware as a
`struct _lf_device *` that can be interacted with by `libflipper`.

- Scan the USB bus using `libusb` to find boards with the VID/PID combo of
**Carbon**. Get a list of the `struct _lf_endpoint *`s corresponding to the
attached devices.
- Iterate through this list of endpoints and create a new `struct _lf_device *`
for each of them. This is the U2 device.
- Create a new `struct _lf_endpoint *` to be used as the bridge endpoint and
configure it to use the U2 device's `uart0` module.
- Create a new `struct _lf_device *` using this bridge endpoint. This will be
the 4S device.
- Return the 4S device as the attached device.
