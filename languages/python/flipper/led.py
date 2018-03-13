from . import lf

def configure():
	lf.invoke("led", 1, lf.types['void'], [lf.uint8(r), lf.uint8(g), lf.uint8(b)])
def rgb(r, g, b):
	lf.invoke("led", 0, lf.types['void'], [lf.uint8(r), lf.uint8(g), lf.uint8(b)])
