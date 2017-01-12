Console
===

The Flipper Console, or **console** for short, is a command line utility that is installed alongside the toolbox. The console makes it easy to interact with the modules that come preinstalled on the device, as well as perform functions such as configuring the device, updating the device, etc.

---

### Obtaining version information.

If the toolbox was installed properly, the console should be accessable using the `flipper` command. To find out which version of the console is installed, use the command below.
```bash
$ flipper version
```

An example of the output is as follows.
```bash
$ flipper version

- console version: 0.1.2
- libflipper version: 0.1.8
- firmware on "flipper": 0.1.3
```
If there are one or more devices plugged in, the firmware versions of the attached devices will be printed alongside the versions of the console and the platform library, `libflipper`.

If running `flipper` produces an error, ensure the toolbox was installed by visiting [getting started](./getting-started.html).

---

### Listing commands.

To display a list of the supported commands, the `help` command can be used.
```bash
$ flipper help
```

---

### Running commands.

All commands passed to the console have a common syntax.

For the invocation of functions on the device, the syntax is as follows:
```bash
$ flipper [module] [function] [arguments ...]
```

For example, to change the color of the device's LED, the following command would be used.
```bash
$ flipper led rgb 0 10 0
```
The execution of this command would turn the LED a dim green. Neat. 😎

#### Help running a command.

For help running a specific command, simply run help on the module or command you need help with.
```bash
$ flipper help led
```

---

### Data Accessors

Different data accessors can be passed to functions that support them. Supported accessors are *strings*, *files*, and *URLs*.

#### Strings

Consider the [usart](./modules-usart.html) module. If you want to ssend data out of the USART peripheral, you would use the module's `push` function. To push a string over USART, the following command syntax is used.
```bash
$ flipper usart push "my data as a string"
```
As you may have guessed, this syntax passes a string to the push function, and as a result, sends the string data out the device's USART bust.

#### Files

However, the `push` function can also be given a file path. For example:
```bash
$ flipper usart push /path/to/my_file.txt
```
This syntax would instruct the console to read the file named `my_file` in the directory `/path/to` and send it over the USART bus.

#### URLs

The same can be done for URLs.
```bash
$ flipper usart push http://www.mycoolsite.com/data
```
This syntax would first download the raw contents of the page `http://www.mycoolsite.com/data`, and proceed to send it out over the USART bus.

Several other modules support the use of data accessors. Information about the syntax that can be used into invoke a module function using the console can be found in the module's documentation entry. For a list of modules, head to [modules](./modules.html).

---

### REPL

The read–eval–print loop, or REPL, functionality of the console enables you to execute multiple commands without exiting the console. To enter the REPL, invoke the console with no arguments.
```bash
$ flipper
```

You can then resume execting commands normally without needing to type `flipper` in front of them. To exit the REPL, use the `exit` command, or `^C`.

---

### Package Manager

The Flipper Package Manager is integrated into the console. To learn how to create, deploy, and distribute packages for your device, check out [package manager](./package-manager.html).
