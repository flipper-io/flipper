#include <flipper/flipper.h>

#include <Python.h>

static PyObject *py_sam_reset(PyObject *self, PyObject *arguments) {
	
	sam.reset();
	
	return Py_BuildValue("");
	
}

static PyObject *py_sam_set_power(PyObject *self, PyObject *arguments) {
	
	uint8_t power;
	
	PyArg_ParseTuple(arguments, "b", &power);
	
	sam.power(power);
	
	return Py_BuildValue("");
	
}

static PyMethodDef py_led_methods[] = {

	{"reset", py_sam_reset, METH_O},
	
	{"power", py_sam_set_power, METH_VARARGS},
	
	{ NULL, NULL }
	
};

void initsam() {
	
	Py_InitModule("sam", py_led_methods);
	
	flipper.attach(FLIPPER_USB, "");
	
}
