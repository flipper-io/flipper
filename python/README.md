# Python

The `python` extension of the Flipper Toolbox enables native control of Flipper hardware from any platform with a supporting a Python interpreter.

Python has become an increasingly popular programming language amongst both beginner and advanced programmers. Its powerful API gives its users the power to integrate the simplest of modules with the most complex.

It would makes sense to be able to control Flipper hardware natively using Python. This is where the Python component of the Flipper Toolbox comes in.

# Installation

The Flipper Python package will be installed by default during the installation of the Flipper SDK. To install it manually, use the makefile in this directory.

# Usage

At the beginning of a new python program, use `from flipper import *` to begin a session. By default, importing the module will connect to the first device available over USB. To attach a device at a different endpoint, `flipper.attach` can be invoked with the appropriate arguments.