#include <flipper/flipper.h>

#include <Python.h>

static PyObject *py_led_rgb(PyObject *self, PyObject *arguments) {
	
	int r, g, b;
	
	PyArg_ParseTuple(arguments, "iii", &r, &g, &b);
	
	led.rgb(r, g, b);
	
	return Py_BuildValue("");
	
}

static PyMethodDef py_led_methods[] = {
	
	{"rgb", py_led_rgb, METH_VARARGS},
	
	{ NULL, NULL }
	
};

void initled() {
	
	Py_InitModule("led", py_led_methods);
	
	flipper.attach(FLIPPER_SOURCE_USB);
	
}
