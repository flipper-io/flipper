#include <flipper.h>
#include <Python.h>

static PyObject *py_led_configure(PyObject *self, PyObject *arguments) {
    led.configure();
    return Py_BuildValue("");
}

static PyObject *py_led_rgb(PyObject *self, PyObject *arguments) {
	int r, g, b;
	PyArg_ParseTuple(arguments, "iii", &r, &g, &b);
	led.rgb(r, g, b);
	return Py_BuildValue("");
}

static PyMethodDef py_led_methods[] = {
    {"configure", py_led_configure, METH_VARARGS},
	{"rgb", py_led_rgb, METH_VARARGS},
	{ NULL }
};

void initled() {
	Py_InitModule("led", py_led_methods);
	flipper.attach();
}
