libflipper
===

All mechanisms of interaction with the device's hardware occur through a dynamically linked library called `libflipper` compiled for the host platform. This library contains the core data structures and functions needed to connect to a device, determine if its firmware version is compatible with the installed version of the library, and begin sending it messages to control its hardware peripherals.

---

### Attach

The first `libflipper` function called from the context of an application running on the host is as follows.

```c
flipper.attach();
```

This function is a top-level wrapper around functionality that exists within `libflipper` to discover the first available device over the default [endpoint](./libflipper-endpoints.html) and "attach" to it. When a device is attached to libflipper, any subsequent calls made to functions within `libflipper` that expect to communicate with a device will be routed to the device that was previously discovered and attached by the `flipper.attach()` function.

In advanced use cases, users can construct their own endpoints to route communications between the host and Flipper devices not accessible by either of the two default endpoints. This is described in detail in the [Endpoints](./libflipper-endpoints.html) section.

### Select

Each of the attach functions returns a pointer to the device that was attached if attaching was a success, otherwise a <span style="color: #c0392b">NULL</span> pointer is returned. If users have disabled automatic error handling, this pointer should be verified to be non-<span style="color: #c0392b">NULL</span> before proceeding. Any number of devices can be attached using this technique, but only the most recently attached device will receive messages sent by `libflipper`. To change which device receives these messages, the select function can be run given a pointer to the target device.

```c
flipper.select(device);
```
