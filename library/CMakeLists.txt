add_library(flipper SHARED
        ${PROJECT_SOURCE_DIR}/library/c/crc.c
        ${PROJECT_SOURCE_DIR}/library/c/debug.c
        ${PROJECT_SOURCE_DIR}/library/c/device.c
        ${PROJECT_SOURCE_DIR}/library/c/dyld.c
        ${PROJECT_SOURCE_DIR}/library/c/error.c
        ${PROJECT_SOURCE_DIR}/library/c/fmr.c
        ${PROJECT_SOURCE_DIR}/library/c/libflipper.c
        ${PROJECT_SOURCE_DIR}/library/c/ll.c
        ${PROJECT_SOURCE_DIR}/library/c/module.c
        ${PROJECT_SOURCE_DIR}/kernel/arch/x64/fmr.S
        ${PROJECT_SOURCE_DIR}/carbon/hal/src/carbon.c
        ${PROJECT_SOURCE_DIR}/platforms/posix/network.c
        ${PROJECT_SOURCE_DIR}/platforms/posix/usb.c
        ${PROJECT_SOURCE_DIR}/api/c/client/adc.c
        ${PROJECT_SOURCE_DIR}/api/c/client/button.c
        ${PROJECT_SOURCE_DIR}/api/c/client/dac.c
        ${PROJECT_SOURCE_DIR}/api/c/client/gpio.c
        ${PROJECT_SOURCE_DIR}/api/c/client/i2c.c
        ${PROJECT_SOURCE_DIR}/api/c/client/led.c
        ${PROJECT_SOURCE_DIR}/api/c/client/pwm.c
        ${PROJECT_SOURCE_DIR}/api/c/client/rtc.c
        ${PROJECT_SOURCE_DIR}/api/c/client/spi.c
        ${PROJECT_SOURCE_DIR}/api/c/client/swd.c
        ${PROJECT_SOURCE_DIR}/api/c/client/temp.c
        ${PROJECT_SOURCE_DIR}/api/c/client/timer.c
        ${PROJECT_SOURCE_DIR}/api/c/client/uart0.c
        ${PROJECT_SOURCE_DIR}/api/c/client/usart.c
        ${PROJECT_SOURCE_DIR}/api/c/client/usb.c
        ${PROJECT_SOURCE_DIR}/api/c/client/wdt.c
        )

include_directories(flipper
        ${PROJECT_SOURCE_DIR}/api/c/client/include
        ${PROJECT_SOURCE_DIR}/library/c
        ${PROJECT_SOURCE_DIR}/carbon/atmegau2/include
        ${PROJECT_SOURCE_DIR}/carbon/atsam4s/include
        ${PROJECT_SOURCE_DIR}/carbon/hal/include
        ${PROJECT_SOURCE_DIR}/platforms
        )

find_path(LIBUSB_INCLUDE_DIR
        NAMES libusb.h
        PATH_SUFFIXES "include" "libusb" "libusb-1.0"
        )

find_library(LIBUSB_LIBRARY
        NAMES usb
        PATH_SUFFIXES "lib" "lib32" "lib64"
        )

target_link_libraries(flipper ${LIBUSB_LIBRARY})

target_include_directories(flipper PUBLIC ${LIBUSB_INCLUDE_DIR})

install(TARGETS flipper
        LIBRARY DESTINATION lib
        )
