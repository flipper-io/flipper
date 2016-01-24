from flipper import *
from time import *

def main():
	state = 0
	io.direction(7, 1)
	while True:
		io.write(8, state)
		state ^= 1
		sleep(1)

main()
