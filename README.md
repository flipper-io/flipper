# Flipper Toolbox

### Overview

Flipper is an embedded development platform that provides application level abstraction over common hardware peripherals such as **GPIO**, **I2C**, **SPI**, **UART**, and more. The Flipper "Toolbox", or simply toolbox, is a repository of discrete sofware components designed to provide effortless control of Flipper hardware from a wide variety of host platforms and programming environments.

This repository is organized into a variety of submodules that contain the sources to discrete components pertainent to the differing layers of abstraction mentioned above. More on each of these submodules can be found within the `README` of the submodule. 

### Building

> ⚠️ **Warning:** Python (>= 2.5) must be installed to build the toolbox.

The **toolbox** uses the [**Waf**](https://github.com/waf-project/waf) build system. *"Waf is a Python-based framework for configuring, compiling and installing applications."*

This repository ships with an executable Waf script. The only dependancy required to execute the Waf script is Python. The entire source can be configured and compiled using the following command.

```
./waf configure build
```

Any platform specific dependancies not found will be listed duing the configuration phase. Subsequent compilations can be performed simply by executing the Waf script.

```
./waf
```