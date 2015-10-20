# Flipper Message Runtime (FMR)
-

The Flipper Message Runtime, or FMR for short, is a critical component of the Flipper Toolbox. The FMR contains a set of specialized data types and helper functions that enable `targets` to communicate with eachother.

### Targets

A target is a device capible of sending or receiving FMR packets. An FMR packet is a quantized data stream that conveys metadata to a target regarding the desired function call and method of forwarding. A message can be forwarded from one target to the next by specifying the `target` field of the message to be a target other than `_self`.

### The `host`, `self`, and `device` relationship:

|        | **PC** | **U2**  | **7S**  |
|:------:|:------:|:-------:|:-------:|
| **PC** | &self  |  &host  | &device |
| **U2** | &host  |  &self  | &device |
| **7S** | &host  | &device |  &self  |

### The `target` driver:

Any driver that "_inherits_" from the target data type is capible of sending and receiving FMR packets.