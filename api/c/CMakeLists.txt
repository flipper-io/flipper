add_library(flipper-api SHARED
        adc.c
        button.c
        dac.c
        gpio.c
        i2c.c
        led.c
        pwm.c
        rtc.c
        spi.c
        swd.c
        temp.c
        timer.c
        uart0.c
        usart.c
        usb.c
        wdt.c
        )

include_directories(api ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(flipper-api PUBLIC flipper)

target_include_directories(flipper-api PUBLIC
        $<TARGET_PROPERTY:flipper,INCLUDE_DIRECTORIES>
        )
