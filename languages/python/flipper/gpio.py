import lf
from ctypes import *

funcs = dict(
	_gpio_configure = 0,
	_gpio_enable = 1,
    _gpio_write = 2,
    _gpio_read = 3
)

class gpio:
	module = None
	def __init__(self, device = None):
		self.module = lf.getModule("gpio")
	def enable(self, enable, disable):
		lf.invoke(self.module, funcs['_gpio_enable'], lf.types['void'], [lf.uint32(enable), lf.uint32(disable)])
	def write(self, set, clear):
		lf.invoke(self.module, funcs['_gpio_write'], lf.types['void'], [lf.uint32(set), lf.uint32(clear)])
	def read(self, mask):
		lf.invoke(self.module, funcs['_gpio_read'], lf.types['void'], [lf.uint32(mask)])
