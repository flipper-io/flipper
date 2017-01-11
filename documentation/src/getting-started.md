## Getting Started

Before you can use your device, you must install a software package to your computer called the Flipper Toolbox. The toolbox bundles the [console](./console.html), [libflipper](./libflipper.html), and the development headers your platform needs to communicate with your device.

#### macOS

On macOS, you must have homebrew installed to install the Flipper toolbox. Homebrew will take care of installing the dependancies needed by the toolbox as well as keep it up to date as we release updates in the future.

```bash
$ brew install flipper
```

#### Linux (Fedora)

```bash
$ sudo dnf install flipper
```

#### Linux (Debian/Ubuntu)

```
$ sudo apt-get install flipper
```

### Configuring your device.

If you wish to skip the configuration phase, you may. Configuration provides a means of personalized device setup such as naming your device(s), detecting backpacks that may require configuration, as well as ensuring that your system has access to all of the utilites that are required to build and deploy to your device.

Ensure you have only one Flipper device connected to your computer and run the following command. Follow the on-screen instructions to configure your device. If your device has an existing configuration, you will be asked if you would like to modify the configuration.

```bash
$ flipper configure
```

### Interacting with your device.

All interaction with your Flipper device will occur through your terminal emulator via a command line utility called the Flipper console, or simply console for short. The console will enable you to create modules and applications for your device, as well as ensure that it is up to date.

#### Updating your device.

Since you just received your device, its software may be out of date. Run the command below to pull any pending updates and write them to your device.

```bash
$ flipper update
```

#### Get device info.

To print information about your device, run the `info` command.

```bash
$ flipper info
```

#### Interacting with the hardware.

Now that your device is set up, you may begin interacting with the hardware peripherals of the device through the console.
