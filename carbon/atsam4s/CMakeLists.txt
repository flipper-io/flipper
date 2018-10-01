add_executable(atsam4s
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
        )

include_directories(atsam4s
        ${PROJECT_SOURCE_DIR}/api/c/driver/include
        ${PROJECT_SOURCE_DIR}/library/c
        ${PROJECT_SOURCE_DIR}/kernel/include
        ${PROJECT_SOURCE_DIR}/carbon/atsam4s/include
        )

set_target_properties(atsam4s PROPERTIES
        COMPILE_FLAGS "-mcpu=cortex-m4"
        COMPILE_FLAGS "-mthumb"
        COMPILE_FLAGS "-march=armv7e-m"
        COMPILE_FLAGS "-mtune=cortex-m4"
        COMPILE_FLAGS "-mfloat-abi=soft"
        COMPILE_FLAGS "-DATSAM4S"
        COMPILE_FLAGS "-D__SAM4S16B__"
        )

set_target_properties(atsam4s PROPERTIES
        LINK_FLAGS "-nostartfiles"
        LINK_FLAGS "-mcpu=cortex-m4"
        LINK_FLAGS "-mthumb"
        LINK_FLAGS "-march=armv7e-m"
        LINK_FLAGS "-mtune=cortex-m4"
        LINK_FLAGS "-mfloat-abi=soft"
        LINK_FLAGS "-Wl,--gc-sections"
        )
