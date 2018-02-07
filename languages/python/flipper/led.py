import lf

# ! KEEP IN SYNC WITH THE FILES BELOW
# carbon/include/flipper/led.h

funcs = dict(
	_led_configure = 0,
	_led_rgb = 1,
)

module = lf.getModule("led")

def rgb(r, g, b):
	lf.invoke(module, funcs['_led_rgb'], lf.types['void'], [lf.uint8(r), lf.uint8(g), lf.uint8(b)])
