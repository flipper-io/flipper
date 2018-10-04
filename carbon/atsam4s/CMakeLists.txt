cmake_minimum_required(VERSION 3.11)
project(atsam4s C ASM)
include(../../cmake/arm.cmake)

configure_file(config/sam4s16.ld ${CMAKE_CURRENT_BINARY_DIR}/config/sam4s16.ld COPYONLY)

add_arm_executable(atsam4s
        src/main.c
        src/isr.c
        src/vectors.c
        api/adc.c
        api/button.c
        api/dac.c
        api/gpio.c
        api/i2c.c
        api/led.c
        api/pwm.c
        api/rtc.c
        api/spi.c
        api/swd.c
        api/temp.c
        api/timer.c
        api/uart0.c
        api/usart.c
        api/usb.c
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
        ../../kernel/arch/armv7/fmr.S
        ../../kernel/arch/armv7/pendsv.S
        ../../kernel/arch/armv7/scheduler.c
        ../../kernel/arch/armv7/syscalls.c
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
        ../../api/c/client/usb.c
        ../../api/c/client/wdt.c
        )

include_directories(atsam4s
        ../../api/c/driver/include
        ../../library/c
        ../../kernel/include
        ../../carbon/atsam4s/include
        ../../carbon/asf/include
        ../../carbon/asf/src
        )

get_property(atsam4s-linkflags TARGET atsam4s.elf PROPERTY LINK_FLAGS)

set_target_properties(atsam4s.elf
        PROPERTIES
        LINK_FLAGS "${atsam4s-linkflags} -Wl,-T ${CMAKE_CURRENT_BINARY_DIR}/config/sam4s16.ld"
        )
