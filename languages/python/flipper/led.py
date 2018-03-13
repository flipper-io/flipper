import lf

funcs = dict(
	_led_configure = 1,
	_led_rgb = 0,
)

def rgb(r, g, b):
	lf.invoke("led", funcs['_led_rgb'], lf.types['void'], [lf.uint8(r), lf.uint8(g), lf.uint8(b)])
