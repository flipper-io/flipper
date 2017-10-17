from ctypes import *

libflipper = CDLL('libflipper.so')

class flipper:
	device_ref = None
	def __init__(self, name = None):
		if (name == None):
			device = attach()
		else:
			device = attach_network(name)
		self.device_ref = device
		return

class module:
	module_ref = None
	def __init__(self, name, device = None):
		if (device == None): device = flipper()
		libflipper.lf_module_create.restype = c_void_p
		self.module_ref = libflipper.lf_module_create(c_char_p(name), device.device_ref)
		bind(self, device)
	def byRef(ref):
		module = module()
		module.module_ref = ref
		return module

	byRef = staticmethod(byRef)

def attach_network(hostname):
	libflipper.carbon_attach_hostname.restype = c_void_p
	return libflipper.carbon_attach_hostname(c_char_p(hostname))

def attach():
	libflipper.flipper_attach.restype = c_void_p
	return libflipper.flipper_attach()

def bind(module, device):
	libflipper.lf_bind(c_void_p(module.module_ref), c_void_p(device.device_ref))

class lf_arg(Structure):
	_fields_ = [("type", c_byte),
				("value", c_ulong)]

def lf_ll_from_list(list):
	ll = c_void_p(0)
	for i in list:
		val = lf_arg(2, i)
		libflipper.lf_ll_append(byref(ll), byref(val), POINTER(c_void_p)())
	return ll

def invoke(module, function, arguments):
	libflipper.lf_invoke(c_void_p(module.module_ref), c_byte(function), lf_ll_from_list(arguments))
	return
