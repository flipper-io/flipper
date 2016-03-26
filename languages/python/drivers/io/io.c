#include <flipper/flipper.h>

#include <Python.h>

static PyObject *py_io_direction(PyObject *self, PyObject *arguments) {
	
	int p, d;
	
	PyArg_ParseTuple(arguments, "ii", &p, &d);
	
	io.direction(p, d);
	
	return Py_BuildValue("");
	
}

static PyObject *py_io_write(PyObject *self, PyObject *arguments) {
	
	int p, s;
	
	PyArg_ParseTuple(arguments, "ii", &p, &s);
	
	io.write(p, s);
	
	return Py_BuildValue("");
	
}

static PyMethodDef py_io_methods[] = {
	
	{ "direction", py_io_direction, METH_VARARGS },

	{ "write", py_io_write, METH_VARARGS },
	
	{ NULL, NULL }
	
};

void initio() {
	
	Py_InitModule("io", py_io_methods);
	
	flipper.attach(FLIPPER_USB, "");
	
}
