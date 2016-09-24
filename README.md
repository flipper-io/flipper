# Flipper Toolbox

### Overview

Flipper is an embedded development platform designed from the ground up to
provide seamless abstraction between the application layer and common hardware
peripherals such as **GPIO**, **I2C**, **SPI**, **UART**, and more.

The **Flipper Toolbox** is a repository of discrete sofware components designed
to control Flipper hardware from a wide variety of host platforms and
programming environments.

This repository is organized into a variety of submodules pertainent to the
differing layers of abstraction mentioned above. More on each of these
submodules can be found within the `README` of the submodule.

### Building

> ⚠️ **Warning:** Python (>= 2.5) must be installed to build the toolbox.

The **toolbox** uses the [**Waf**](https://github.com/waf-project/waf) build
system. *"Waf is a Python-based framework for configuring, compiling and
installing applications."*

This repository ships with an executable Waf script. The only dependancy
required to execute the Waf script is Python. The entire source can be
configured and compiled using the following command.

```
./waf configure build
```

Any platform specific dependancies not found will be listed duing the
configuration phase. Subsequent compilations can be performed simply by
executing the Waf script.

```
./waf
```

### FMR Documentation

Following the deprication of the FMR 1.0 specification formally defined in "A Multi-Archiectural Approach to the Development of Embedded Hardware," a more refined specification and API (FMR 2.0) has been set forth. This implementation provides a better defined suite of generator functions that provide granular control over the construction of outgoing packets and the parsing of incoming packets. FMR 2.0 also enables the transmission of arbitrary data between address spaces using a more advanced push/pull architecture that can balance the load of data transfer on a bus alternative to the one used by the message runtime (in favor of more data transfer per unit time as opposed to responsiveness to packets).

The superficial API exposed by the FMR 2.0 interface is defined below:

