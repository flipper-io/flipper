//! The hardware module provides utilities for maintaining and managing
//! Flipper's hardware. Some responsibilities of the `hardware` module include
//! flashing new firmware, renaming Flipper with a new identifier, configuring
//! Wifi (if a wifi backpack is attached), and similar tasks.

pub mod fdfu;
