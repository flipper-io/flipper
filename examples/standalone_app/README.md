# Application

This example demonstrates how to build an application for the device. An
application is  standalone code that runs on the hardware without intervention
from a host computer. This example application blinks an LED that is attached
to `IO_1`.

#### Building

```sh
make
```

#### Running

```sh
make install
```

By running `make install` the application will be loaded onto the attached
device and start executing immediately. If there is an LED attached to `IO_1`, it will begin to blink.
