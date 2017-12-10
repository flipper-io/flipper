# Library

`libflipper` is the platform level library that brings the message runtime to a host wishing to control a Flipper device.

### Quickstart

Attach to the first available Flipper device.

```
flipper_attach();
```

Turn the device's pin 8 on.

```
gpio_write(8, 0);
```

That's literally it.

#### Networking

You can also connect to a Flipper device over a network.

```
flipper_attach_network("192.168.1.1")
gpio_write(8, 0);
```

Simple.

If you don't have a WiFi shield for your device, you can test `libflipper` right now locally using the Flipper Virtual Machine (FVM). After installing the`utils`, simply run FVM and then attach to `localhost`.

```
flipper_attach_network("localhost")
gpio_write(8, 0);
```

You should see a message printed saying `Turning GPIO pin 8 ON.` FVM is excellent for debugging changes made to the message runtime or `libflipper` itself.
