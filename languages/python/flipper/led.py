import lf
from ctypes import *

funcs = dict(
	_led_configure = 0,
	_led_rgb = 1,
)

class led:
	module = None
	def __init__(self, device = None):
		self.module = lf.getModule("led")
	def rgb(self, r, g, b):
		lf.invoke(self.module, funcs['_led_rgb'], lf.types['void'], [lf.u8(r), lf.u8(g), lf.u8(b)])
