macro(add_avr_executable target_name)

    set(CMAKE_SYSTEM_PROCESSOR avr)
    set(CMAKE_CROSSCOMPILING 1)

    find_path(AVR_TOOLCHAIN_ROOT
            NAMES
            avr-gcc
            PATHS
            /usr/bin
            /usr/local/bin
            /bin
            $ENV{AVR_ROOT}
            )

    set(CMAKE_C_COMPILER   ${AVR_TOOLCHAIN_ROOT}/avr-gcc     CACHE PATH "avr-gcc" FORCE)
    set(CMAKE_ASM_COMPILER ${AVR_TOOLCHAIN_ROOT}/avr-gcc     CACHE PATH "avr-asm" FORCE)
    set(CMAKE_CXX_COMPILER ${AVR_TOOLCHAIN_ROOT}/avr-g++     CACHE PATH "avr-g++" FORCE)
    set(CMAKE_AR           ${AVR_TOOLCHAIN_ROOT}/avr-ar      CACHE PATH "avr-ar" FORCE)
    set(CMAKE_LINKER       ${AVR_TOOLCHAIN_ROOT}/avr-ld      CACHE PATH "avr-ld" FORCE)
    set(CMAKE_NM           ${AVR_TOOLCHAIN_ROOT}/avr-nm      CACHE PATH "avr-nm" FORCE)
    set(CMAKE_OBJCOPY      ${AVR_TOOLCHAIN_ROOT}/avr-objcopy CACHE PATH "avr-objcopy" FORCE)
    set(CMAKE_OBJDUMP      ${AVR_TOOLCHAIN_ROOT}/avr-objdump CACHE PATH "avr-objdump" FORCE)
    set(CMAKE_STRIP        ${AVR_TOOLCHAIN_ROOT}/avr-strip   CACHE PATH "avr-strip" FORCE)
    set(CMAKE_RANLIB       ${AVR_TOOLCHAIN_ROOT}/avr-ranlib  CACHE PATH "avr-ranlib" FORCE)
    set(AVR_SIZE           ${AVR_TOOLCHAIN_ROOT}/avr-size    CACHE PATH "avr-size" FORCE)

    set(elf_file ${target_name}.elf)
    set(hex_file ${target_name}.hex)

    add_executable(${elf_file}
            ${ARGN}
            )

    set_target_properties(${elf_file}
            PROPERTIES
            COMPILE_FLAGS "-mmcu=atmega32u2 -DARCH=ARCH_AVR8 -D__AVR_ATmega32U2__ -DF_CPU=16000000UL -DATMEGAU2 -DLF_DISABLE_DEBUG -DLF_CONFIG_OMIT_ERRORS -Os"
            LINK_FLAGS "-mmcu=atmega32u2 -Wl,--gc-sections"
            )

    add_custom_command(
            OUTPUT ${hex_file}
            COMMAND ${CMAKE_OBJCOPY} -O ihex ${elf_file} ${hex_file}
            DEPENDS ${elf_file}
            )

    add_custom_command(
            OUTPUT "print-size-${elf_file}"
            COMMAND ${AVR_SIZE} ${elf_file}
            DEPENDS ${elf_file}
    )

    add_custom_target(${target_name}
            ALL
            DEPENDS ${hex_file} "print-size-${elf_file}"
            )

    set_target_properties(${target_name}
            PROPERTIES
            OUTPUT_NAME ${elf_file}
    )

endmacro(add_avr_executable)
