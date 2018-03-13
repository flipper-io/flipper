import lf

# GPIO signals
IO_1 = (1 << 22)
IO_2 = (1 << 23)
IO_3 = (1 << 2)
IO_4 = (1 << 30)
IO_5 = (1 << 29)
IO_6 = (1 << 28)
IO_7 = (1 << 27)
IO_8 = (1 << 7)
IO_9 = (1 << 8)
IO_10 = (1 << 26)
IO_11 = (1 << 25)
IO_12 = (1 << 24)
IO_14 = (1 << 16)
IO_15 = (1 << 7)
IO_16 = (1 << 6)
# USB signals
IO_DP = (1 << 11)
IO_DM = (1 << 10)
# USART signals
IO_RX = (1 << 5)
IO_TX = (1 << 6)
# SPI signals
IO_MOSI = (1 << 13)
IO_MISO = (1 << 12)
IO_SCK = (1 << 14)
IO_SS = (1 << 31)
# I2C signals
IO_SCL = (1 << 4)
IO_SDA = (1 << 3)
# Analog signals
IO_A1 = (1 << 20)
IO_A2 = (1 << 19)
IO_A3 = (1 << 18)
IO_A4 = (1 << 17)
IO_A5 = (1 << 3)
IO_A6 = (1 << 2)
IO_A7 = (1 << 1)
IO_A8 = (1 << 0)

funcs = dict(
	_gpio_configure = 3,
	_gpio_enable = 2,
    _gpio_write = 1,
    _gpio_read = 0
)

def enable(enable, disable):
	lf.invoke("gpio", funcs['_gpio_enable'], lf.types['void'], [lf.uint32(enable), lf.uint32(disable)])
def write(set, clear):
	lf.invoke("gpio", funcs['_gpio_write'], lf.types['void'], [lf.uint32(set), lf.uint32(clear)])
def read(mask):
	lf.invoke("gpio", funcs['_gpio_read'], lf.types['void'], [lf.uint32(mask)])
