import lf
from ctypes import *

class led:
	module = None
	def __init__(self, device = None):
		self.module = lf.module("led", device)

	def rgb(self, r, g, b):
		lf.invoke(self.module, 1, [r, g, b])
		print('Setting R: ' + str(r) + ' G: ' + str(g) + ' B: ' + str(b))
