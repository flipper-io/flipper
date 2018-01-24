# Library

`libflipper` is the platform level library that gives developers access to
Flipper hardware.

### Quickstart

#### C example

```c
flipper_attach();
gpio_write(8, 0);
```

#### Python example

```
from flipper import *
lf.attach()
gpio.write(8, 0)
```

#### Networking

You can also connect to a Flipper device over a network.

```c
flipper_attach_network("192.168.1.1")
gpio_write(8, 0);
```

Simple.

If you don't have a WiFi shield for your device, you can test `libflipper`
right now locally using the Flipper Virtual Machine (FVM). After installing
the`utils`, simply run FVM and then attach to `localhost`.

```
flipper_attach_network("localhost")
gpio_write(8, 0);
```

You should see a message printed saying `Turning GPIO pin 8 ON.` FVM is
excellent for debugging changes made to the message runtime or `libflipper`
itself.

To learn more about using fvm, see the WiKi page (here)[].
