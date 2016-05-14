# The Flipper Toolbox

### Introduction

The **Toolbox** is a repository of sofware components that work together to
enable dynamic control of Flipper hardware across a wide variety of host
platforms and programming environments.

At the heart of the toobox is a lightweight yet sophistocated control stack
designed to fascilitate real time interaction with Flipper hardware.

#### Building the Toolbox

The toolbox uses the [shake](http://shakebuild.com/) build system. The easiest
way to get shake is:

- Install the [Haskell Tool Stack](http://docs.haskellstack.org/en/stable/README/)
- Run `stack setup`
- Run `stack install shake`
- Make sure that `~/.local/bin` is in your `$PATH`.

`shake` may be invoked like `make` with a list of targets. On an SMP computer,
`shake` will automatically detect the number of cores available. The following
targets are available:

- `clean` removes all build artifacts.
- `install` builds and installs the toolbox and the Flipper console. The
  installation prefix will be read from the `$PREFIX` environment variable.
  Otherwise `/usr/local` will be used as the default.
- `install-libflipper` will build and install the toolbox.
- `install-console` will build and install the Flipper console.
- `native` will build all native targets.
- `flipper-library` will build the toolbox.
- `flipper-console` will build the Flipper console.
- `flipper-osmium` will build Osmium for all targets.
- `burn-at91sam4s` will burn Osmium to the AT91SAM4S.
- `burn-atmega16u2` will burn Osmium to the ATMEGA16u2.

See the [shake manual](http://shakebuild.com/manual) and
[shake's Hackage page](http://shakebuild.com/manual) for more information.
