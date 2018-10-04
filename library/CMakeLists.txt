#find_path(HOST_TOOLCHAIN_ROOT
#        NAMES
#        gcc
#        PATHS
#        /usr/bin
#        /usr/local/bin
#        /bin
#        $ENV{AVR_ROOT}
#        )
#
#set(CMAKE_C_COMPILER   ${HOST_TOOLCHAIN_ROOT}/gcc     CACHE PATH "gcc" FORCE)
#set(CMAKE_ASM_COMPILER ${HOST_TOOLCHAIN_ROOT}/gcc     CACHE PATH "asm" FORCE)
#set(CMAKE_CXX_COMPILER ${HOST_TOOLCHAIN_ROOT}/g++     CACHE PATH "g++" FORCE)
#set(CMAKE_AR           ${HOST_TOOLCHAIN_ROOT}/ar      CACHE PATH "ar" FORCE)
#set(CMAKE_LINKER       ${HOST_TOOLCHAIN_ROOT}/ld      CACHE PATH "ld" FORCE)
#set(CMAKE_NM           ${HOST_TOOLCHAIN_ROOT}/nm      CACHE PATH "nm" FORCE)
#set(CMAKE_OBJCOPY      ${HOST_TOOLCHAIN_ROOT}/objcopy CACHE PATH "objcopy" FORCE)
#set(CMAKE_OBJDUMP      ${HOST_TOOLCHAIN_ROOT}/objdump CACHE PATH "objdump" FORCE)
#set(CMAKE_STRIP        ${HOST_TOOLCHAIN_ROOT}/strip   CACHE PATH "strip" FORCE)
#set(CMAKE_RANLIB       ${HOST_TOOLCHAIN_ROOT}/ranlib  CACHE PATH "ranlib" FORCE)
cmake_minimum_required(VERSION 3.11)
project(Flipper C ASM)

add_library(flipper SHARED
        c/crc.c
        c/debug.c
        c/device.c
        c/dyld.c
        c/error.c
        c/fmr.c
        c/libflipper.c
        c/ll.c
        c/module.c
        ../kernel/arch/x64/fmr.S
        ../carbon/hal/src/carbon.c
        ../platforms/posix/network.c
        ../platforms/posix/usb.c
        ../api/c/client/adc.c
        ../api/c/client/button.c
        ../api/c/client/dac.c
        ../api/c/client/gpio.c
        ../api/c/client/i2c.c
        ../api/c/client/led.c
        ../api/c/client/pwm.c
        ../api/c/client/rtc.c
        ../api/c/client/spi.c
        ../api/c/client/swd.c
        ../api/c/client/temp.c
        ../api/c/client/timer.c
        ../api/c/client/uart0.c
        ../api/c/client/usart.c
        ../api/c/client/usb.c
        ../api/c/client/wdt.c
        )

include_directories(flipper
        ./c
        ../api/c/client/include
        ../carbon/atmegau2/include
        ../carbon/atsam4s/include
        ../carbon/hal/include
        ../platforms
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
