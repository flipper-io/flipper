from flipper import *

from math import *

import time

def fade(delay):
	
	seed = 0

	count = 0

	while 1:

		x = abs(20 * sin(seed))

		seed += 0.01

		led.rgb(int(x), int(x), int(x))

		time.sleep(delay)

		count += 1

def main():

	fade(0.001)

main()
