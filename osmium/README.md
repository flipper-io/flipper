# Osmium

Osmium is an operating system designed from the ground up explicitly for Flipper hardware. Osmium natively integrates with the Flipper Toolbox to provide dynamic, real-time control of Flipper hardware directly out of the box. Osmium also offers an optional foundation upon which applications can be built, providing basic IO fascilities such as various device drivers, a simple filesystem, and a scheduler to speed up the development process.

## Targets

Osmium currently supports three targets, the `AT91SAM4S`, the `ATMEGA16U2`, and the `ESP8266`.

Each target has a different identity, and understands its place in the FMR heirarchy. (See the FMR README in `/fmr`).