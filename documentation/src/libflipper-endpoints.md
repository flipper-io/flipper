Endpoints
===

> An **endpoint**, from the context of `libflipper`, is a physical channel via which data can be sent to or received from a Flipper device.

The following endpoints are currently supported by `libflipper`:
- [USB](#USB)
- [Network](#Network)

---

### USB

The default endpoint used by `libflipper` is USB. Most Flipper users will have their devices connected to the host via USB. As a result, USB is the endpoint to via which devices are attached implicitly.

`libflippper` uses [libusb](http://libusb.info/) as a backend to communicate with attached devices. As a result, no drivers need to be installed on the host platform in order to use the hardware. On some platforms, the operating system must be alerted that `libusb` will be used to control the device. On Windows, an `ini` file is used to accomplish this. On Linux, a `udev` rule must be installed to allow a non-root user to interact with the device.

On platforms that do not support USB, a different endpoint must be chosen.

#### Network

The other endpoint currently supported by `libflipper` is the `network` endpoint. Using this endpoint, a device with a known IP address can be attached to the current instance of `libflipper` using the function below.

```c
flipper.attach_network("some.random.ip.address");
```

#### Custom Endpoints

In advanced use cases, a custom endpoint can be made.

```c
struct _lf_endpoint _my_custom_endpoint = {
    /* TODO */
};
```

Once the endpoint has been created, a device can be attached to it using the following syntax.
```c
flipper.attach_endpoint("my device", &_my_custom_endpoint);
```
