# ledfun.py - Harlan Haskins - August 29, 2015

from flipper import led
from math import *

def to_color(n):
	return int(abs(sin(n) * 75.0))

r = 1.0
g = 2.0
b = 3.0
while True:
	r += 0.01
	g += 0.01
	b += 0.01
	led.rgb(to_color(r), to_color(g), to_color(b))


