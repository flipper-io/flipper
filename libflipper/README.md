### Install Procedures for Linux

Add `export FLIPPERSDK="/usr/local/flipper"` to `~/.bashrc`
Add `export PATH=$PATH:"$FLIPPERSDK/bin"` to `~/.bashrc`
Add `export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$FLIPPERSDK/lib"` to `~/.bashrc`

`sudo dnf install libusb-devel.x86_64` for Fedora

`sudo apt-get install libusb-dev python-dev gcc-avr avr-libc avrdude` for Ubuntu