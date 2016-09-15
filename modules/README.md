## Standard Modules

Modules are a construct that provide relatively high level access to device
hardware and peripherals with a portable API. Drivers are designed to have the
same interface, regardless of operating environment or programming language. The
goal of this design methodology is to simplify the overhead requireed to bind
the interfaces of these control blocks to intermediary and high level languages
for use across multiple address spaces via the message runtime. Collectively,
modules form a hardware abstraction layer over APIs specific to the platforms
that they target.

Standard modules are modules that come by default on Flipper hardware and can be
used to build other more powerful and application specific modules.

### Compilation

When compiled, each module will yeild a file `NAME.module` where `NAME` is the
name of the module undergoing compilation. The `.module` extension indicates the
presence of an archival format that can be read by tools such as the Flipper
Console to install the module. The `.module` format is simply a zip archive that
contains the binary code for each of the module's targets, as well as a header
file exposing the API for the module.
