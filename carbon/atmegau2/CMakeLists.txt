project(atmegau2 C ASM)
include(${CMAKE_SOURCE_DIR}/cmake/avr.cmake)

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
        ${CMAKE_SOURCE_DIR}/library/c/crc.c
        ${CMAKE_SOURCE_DIR}/library/c/debug.c
        ${CMAKE_SOURCE_DIR}/library/c/device.c
        ${CMAKE_SOURCE_DIR}/library/c/dyld.c
        ${CMAKE_SOURCE_DIR}/library/c/error.c
        ${CMAKE_SOURCE_DIR}/library/c/fmr.c
        ${CMAKE_SOURCE_DIR}/library/c/libflipper.c
        ${CMAKE_SOURCE_DIR}/library/c/ll.c
        ${CMAKE_SOURCE_DIR}/library/c/module.c
        ${CMAKE_SOURCE_DIR}/kernel/arch/avr8/fmr.S
        ${CMAKE_SOURCE_DIR}/api/c/client/adc.c
        ${CMAKE_SOURCE_DIR}/api/c/client/button.c
        ${CMAKE_SOURCE_DIR}/api/c/client/dac.c
        ${CMAKE_SOURCE_DIR}/api/c/client/gpio.c
        ${CMAKE_SOURCE_DIR}/api/c/client/i2c.c
        ${CMAKE_SOURCE_DIR}/api/c/client/led.c
        ${CMAKE_SOURCE_DIR}/api/c/client/pwm.c
        ${CMAKE_SOURCE_DIR}/api/c/client/rtc.c
        ${CMAKE_SOURCE_DIR}/api/c/client/spi.c
        ${CMAKE_SOURCE_DIR}/api/c/client/swd.c
        ${CMAKE_SOURCE_DIR}/api/c/client/temp.c
        ${CMAKE_SOURCE_DIR}/api/c/client/timer.c
        ${CMAKE_SOURCE_DIR}/api/c/client/uart0.c
        ${CMAKE_SOURCE_DIR}/api/c/client/usart.c
        ${CMAKE_SOURCE_DIR}/api/c/client/wdt.c
        )

include_directories(atmegau2
        ${CMAKE_SOURCE_DIR}/api/c/driver/include
        ${CMAKE_SOURCE_DIR}/library/c
        ${CMAKE_SOURCE_DIR}/kernel/include
        ${CMAKE_SOURCE_DIR}/carbon/atmegau2/include
        )

add_custom_target(install-atmegau2
        COMMAND dfu-programmer atmega32u2 erase --force
        COMMAND dfu-programmer atmega32u2 flash atmegau2.hex
        COMMAND dfu-programmer atmega32u2 start
        DEPENDS atmegau2.hex
        )
