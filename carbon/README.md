Flipper: Carbon Edition
===

This directory contains board-specific implementation for the **Flipper: Carbon Edition** hardware platform. Included within this directory are the definitions of the standard modules exposed by this board, as well as their platform specific implementations. A Hardware Abstraction Layer, or HAL, is provided for interfacing `libflipper` with the Carbon Edition board.

Hardware Architecture
---
The Carbon edition platform incorperates two control units, a microcontroller for handling interactions with the host platform via USB, as well as the primary microprocessor that runs the our operating system, Osmium.

An `ATMega16U2` (or **U2** for short) acts as the USB controller while the `ATSAM4S16B` (or **4S** for short) runs programs and handles interaction with the device peripherals. These devices share a USART bus, as well as exist in multi-master mode to a shared SPI flash device. The U2 is responsible for flashing firmware updates to the 4S, installing programs into flash memory, handing libflipper level interactions with the LED and button, powering and resetting the 4S, as well as acting as a bridge device between the host and the 4S.

The U2 enables the host instance of `libflipper` to communicate with the 4S over its USART bus. This bus is run at `2M baud` so it does not impose a bandwith limitation between the host and the 4S; all bandwidth limitations between the host and the 4S device are a factor of such limitations imposed by the transfer rates of full-speed USB, as well as the workload of the host's USB controller. The HAL presents the U2 as an endpoint to which `libflipper` can be attached. As `libflipper` performs invocations through this
endpoint, the HAL automatically routes packets to their corresponding devices. The active instance of `libflipper` is aware of which device certain modules belong on due to the fact that HAL sets the double pointer to the specific device upon which the standard or user module belongs when the U2 is set as the active endpoint.
