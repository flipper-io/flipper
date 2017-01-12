Package Manager
===

Bundled into the [console](./console.html) is a utility that is used to create, install, and distribute modules and applications written for Flipper devices. This utility is called the Flipper Package Manager, or **FPM** for short.

A module or application created by FPM is called a package, and has information associated with it such as the package's *name*, *version*, and a list of its *dependancies*.

---

#### Creating

FPM makes it easy to create new modules from the command line. Below is an example of how to use the package manager to create a module named *my_module*.
```bash
$ flipper init my_module --module
```
This will create a directory `my_module`, and populate it with the basic structure needed to build a module for the device. Go to the [modules](./libflipper-modules.html) section to learn about the file structure that was created, and how to add functions to your new module.

The package manager can also be used to create applications in the same manner.
The example below demonstrates how to create an application named *my_app* using the console.
```bash
$ flipper init my_app --app
```
The generated directory and file structure is no different to that of a module. The only property that differentiates applications from modules is the presence of the `main()` function. If this function is present in the code, it will automatically be called when the application is loaded onto a target device.

---

#### Building

To build your package, once it has been created, the `build` command is used.
```bash
$ flipper build
```
Running this command will report any warnings or errors encountered while building the package. If building was successful, the output files will be written to a `build` directory that is created automatically if it does not exist.

Running the command again will scan the source tree for changes and repeat the build process, compiling only the source files that have changed.

---

#### Deploying

To test your package, it can be deployed to the attached device using the following command.
```bash
$ flipper deploy
```
Alternatively, the package can be deployed to a specific device using the following syntax, where `device_name` is the name of the target device that was specified during the device's configuration.
```
$ flipper deploy --device device_name
```

The `deploy` command loads the package into the device's RAM for testing purposes. During the development of a package, it will be deployed many times. Because of this, is preferable to use RAM over ROM due to the fact that RAM is faster than ROM, and writing to ROM repeatedly causes wear.

> **Note:** The `deploy` command does not install the package to the device; when the device loses power, the loaded package will be lost. To preserve the package on the device when power is lost, use the `install` command.

---

#### Installing

To install the package onto the device, the `install` command is used.
```bash
$ flipper install
```
Running this command will ensure that the build of the package in the current directory is up to date, and write it into the device's ROM. Installing will make the package available after a loss of power to the device.

Like the `deploy` commmand, the install command can be directed to act on a specific device.
```
$ flipper install --device device_name
```

---

#### Distributing

Once a package is completed, we encourage that it be shared with the community. We have made it easy to accomplish this with the `publish` command.
```bash
$ flipper publish
```
Running this command will ask for permission to proceed with the publication as well as verify with the user that the name and description of the package matches what they wish to have go public. Given permission to do so, the package manager will wrap the package and its source up in an archive and send it to the package repository, [packages.flipper.io](packages.flipper.io).

Once a package has been published, anybody with a Flipper device can download and install the package to their device using the install command provided with the name under which the package was published.
```bash
$ flipper install my_package
```

The publish command will also return a URL at which the installed package can be found so it can easily be shared! 😁
