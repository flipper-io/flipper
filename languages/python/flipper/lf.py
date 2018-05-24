from ctypes import *

types = dict(
	void = 2,

	int = 4,
	ptr = 6,

	uint8 = 0,
	uint16 = 1,
	uint32 = 3,
	uint64 = 7,

	int8 = 8,
	int16 = 9,
	int32 = 11,
	int64 = 15,

	_default_ = -1,
)

class lf_type:
	type = None
	value = None
	def __init__(self, type, value):
		self.type = type
		self.value = value
		return

def uint8(value):
	return lf_type(types['uint8'], value)
def uint16(value):
	return lf_type(types['uint16'], value)
def uint32(value):
	return lf_type(types['uint32'], value)
def uint64(value):
	return lf_type(types['uint64'], value)

def int8(value):
	return lf_type(types['int8'], value)
def int16(value):
	return lf_type(types['int16'], value)
def int32(value):
	return lf_type(types['int32'], value)
def int64(value):
	return lf_type(types['int64'], value)

libflipper = CDLL('libflipper.so')
#libflipper.lf_set_debug_level(3)

class flipper:
	ref = None
	def __init__(self, name = None):
		if (name == None):
			device = current_device()
		self.ref = device
		return

def attach():
	libflipper.flipper_attach.restype = c_void_p
	return libflipper.flipper_attach()

def attach_network(hostname):
	libflipper.carbon_attach_hostname.restype = c_void_p
	return libflipper.carbon_attach_hostname(c_char_p(hostname.encode("utf-8")))

def bind(module, device):
	libflipper.lf_bind(c_void_p(device.ref), c_void_p(module.ref))

def current_device():
	libflipper.lf_get_selected.restype = c_void_p
	return libflipper.lf_get_selected()

def lf_ll_from_list(l):
	ll = c_void_p(0)
	if l:
		for i in l:
			libflipper.lf_arg_create.restype = c_void_p
			arg = libflipper.lf_arg_create(c_byte(i.type), c_ulonglong(i.value))
			libflipper.lf_ll_append(byref(ll), c_void_p(arg), libflipper.lf_arg_release)
	return ll

def getModule(name):
	_module = c_void_p.in_dll(libflipper, '_' + name)
	return module(name, None, byref(_module))

def invoke(module, function, ret, arguments):
	libflipper.lf_invoke(c_void_p(current_device()), c_char_p(module.encode("utf-8")), c_byte(function), c_byte(ret), lf_ll_from_list(arguments))
	return
