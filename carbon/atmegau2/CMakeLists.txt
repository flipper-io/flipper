cmake_minimum_required(VERSION 3.11)
project(atmegau2 C ASM)
include(../../cmake/avr.cmake)

add_avr_executable(atmegau2
        src/main.c
        src/bulk.c
        src/control.c
        src/debug.c
        src/descriptors.c
        src/error.c
        src/interrupt.c
        api/button.c
        api/gpio.c
        api/led.c
        api/spi.c
        api/uart0.c
        api/wdt.c
        ../../library/c/crc.c
        ../../library/c/debug.c
        ../../library/c/device.c
        ../../library/c/dyld.c
        ../../library/c/error.c
        ../../library/c/fmr.c
        ../../library/c/libflipper.c
        ../../library/c/ll.c
        ../../library/c/module.c
        ../../kernel/arch/avr8/fmr.S
        ../../api/c/client/adc.c
        ../../api/c/client/button.c
        ../../api/c/client/dac.c
        ../../api/c/client/gpio.c
        ../../api/c/client/i2c.c
        ../../api/c/client/led.c
        ../../api/c/client/pwm.c
        ../../api/c/client/rtc.c
        ../../api/c/client/spi.c
        ../../api/c/client/swd.c
        ../../api/c/client/temp.c
        ../../api/c/client/timer.c
        ../../api/c/client/uart0.c
        ../../api/c/client/usart.c
        ../../api/c/client/wdt.c
        )

include_directories(atmegau2
        ../../api/c/driver/include
        ../../library/c
        ../../kernel/include
        ../../carbon/atmegau2/include
        )

add_custom_target(install-atmegau2
        COMMAND dfu-programmer atmega32u2 erase --force
        COMMAND dfu-programmer atmega32u2 flash atmegau2.hex
        COMMAND dfu-programmer atmega32u2 start
        DEPENDS atmegau2.hex
        )
