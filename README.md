# The Flipper Toolbox

### Introduction

The **Toolbox** is a repository of sofware components that work together enable dynamic control of Flipper hardware across a wide variety of host platforms and programming environments.

At the heart of the toobox is a lightweight yet sophistocated control stack designed to fascilitate real time interaction with Flipper hardware.

### Getting Started

The python script `manage.py` is a simple script that maintains the directory structure of the toolbox. It can create and destroy modules as well as rebuild the toolbox's symlink tree. Before attempting to build individual components of the toolbox for the first time, the command `python manage.py rebuild` must be executed in order to prepare the targets of the toolbox for compilation. Running the makefile in the root directory of the toolbox will automatically execute this command.

#### Building the Toolbox

The entire toolbox can be built using the command `make all` in the toolbox's root directory. This command will subsequently build all of the minor targets of the toolbox and aggregate the outputs of the build into a directory in preparation for install. Minor targets can be built and installed individualy if desired from the root directory of the respective minor component.

#### Installing the Toolbox

The toolbox can be installed using the command `make install PREFIX=YOUR_PREFIX` where `YOUR_PREFIX` is the destination of the install. Most UNIX-like systems accept `/usr/local/` as the the prefix for user-installed software. As before, minor targets of the toolbox can be installed using the same command from their root directory.