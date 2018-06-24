# Runtime

<<<<<<< HEAD
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
The runtime is the cross-platform implementation that communicates with a
Flipper HAL.
>>>>>>> restructure project, separate makefiles, fix asf
