include_dirs = $(shell python -c "from distutils.sysconfig import get_python_inc; print(get_python_inc())")
includes = $(foreach include,$(include_dirs),-I $(include))

all:
	gcc -shared $(includes) -lflipper -lpython led.c -o led.so
