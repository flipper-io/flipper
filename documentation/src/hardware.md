## Flipper Hardware

Any of Flipper's on-board peripherals can be interacted with straight out of the box. Using the Flipper console, these peripherals can be interacted with using the syntax below.

```bash
$ flipper [module] [function] [arguments ...]
```

Flipper comes with a set of built-in modules called "standard modules". These standard modules enable control of preliminary hardware and can be used to create more complex systems. Any of the built-in standard modules can be controlled from the console using this syntax.

#### LED

The simplest peripheral on the hardware is the built-in RGB LED. To set the color of the LED, use the following command.

```
$ flipper led rgb 0 0 10
```

The `led` module is the built-in standard module that controls the LED hardware. The `rgb` function writes an RGB value to the LED. The arguments to the function are the red, green, and blue values separated by spaces. 

After running the command, you should see your device's LED turn a dim blue. This is 10 units of intensity on the blue channel out of a maximum of 255.