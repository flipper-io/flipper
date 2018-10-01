set(CMAKE_SYSTEM_PROCESSOR avr)
set(CMAKE_C_COMPILER avr-gcc)

add_executable(atmegau2
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
        )

include_directories(atmegau2
        ${PROJECT_SOURCE_DIR}/api/c
        ${PROJECT_SOURCE_DIR}/library/c
        ${PROJECT_SOURCE_DIR}/kernel/include
        ${PROJECT_SOURCE_DIR}/carbon/atmegau2/include
        )

set_target_properties(atmegau2 PROPERTIES
        COMPILE_FLAGS "-mmcu=atmega32u2"
        COMPILE_FLAGS "-DARCH=ARCH_AVR8"
        COMPILE_FLAGS "-D__AVR_ATmega32U2__"
        COMPILE_FLAGS "-DF_CPU=16000000UL"
        COMPILE_FLAGS "-DATMEGAU2"
        COMPILE_FLAGS "-DLF_DISABLE_DEBUG"
        COMPILE_FLAGS "-DLF_CONFIG_OMIT_ERRORS"
        COMPILE_FLAGS "-Os"
        )

set_target_properties(atmegau2 PROPERTIES
        LINK_FLAGS "-mmcu=atmega32u2"
        LINK_FLAGS "-Wl,--gc-sections"
        )
