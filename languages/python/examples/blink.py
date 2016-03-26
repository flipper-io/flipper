import time

from flipper import led

def blink():
	time.sleep(0.5)
	led.rgb(0, 0, 10)
	time.sleep(0.5)
	led.rgb(0, 10, 0)
	time.sleep(0.5)
	led.rgb(10, 0, 0)
