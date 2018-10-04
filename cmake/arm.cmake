macro(add_arm_executable target_name)

    set(CMAKE_SYSTEM_NAME Generic)
    set(CMAKE_SYSTEM_PROCESSOR arm)
    set(CMAKE_CROSSCOMPILING 1)

    find_path(ARM_TOOLCHAIN_ROOT
            NAMES
            arm-none-eabi-gcc
            PATHS
            /usr/bin
            /usr/local/bin
            /bin
            $ENV{ARM_ROOT}
            )

    #set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

    set(CMAKE_C_COMPILER   arm-none-eabi-gcc)
    set(CMAKE_ASM_COMPILER arm-none-eabi-gcc)
    set(CMAKE_CXX_COMPILER arm-none-eabi-g++)
    set(CMAKE_AR           arm-none-eabi-ar)
    set(CMAKE_LINKER       arm-none-eabi-ld)
    set(CMAKE_NM           arm-none-eabi-nm)
    set(CMAKE_OBJCOPY      arm-none-eabi-objcopy)
    set(CMAKE_OBJDUMP      arm-none-eabi-objdump)
    set(CMAKE_STRIP        arm-none-eabi-strip)
    set(CMAKE_RANLIB       arm-none-eabi-ranlib)

    set(elf_file ${target_name}.elf)
    set(hex_file ${target_name}.hex)
    set(bin_file ${target_name}.bin)

    add_executable(${elf_file}
            ${ARGN}
            )

    set_target_properties(${elf_file}
            PROPERTIES
            COMPILE_FLAGS "-mcpu=cortex-m4 -mthumb -march=armv7e-m -mtune=cortex-m4 -mfloat-abi=soft -DATSAM4S -D__SAM4S16B__"
            LINK_FLAGS "-nostartfiles -mcpu=cortex-m4 -mthumb -march=armv7e-m -mtune=cortex-m4 -mfloat-abi=soft -Wl,--gc-sections"
            )

    add_custom_command(
            OUTPUT ${bin_file}
            COMMAND ${CMAKE_OBJCOPY} -O binary ${elf_file} ${bin_file}
            DEPENDS ${elf_file}
    )

    add_custom_target(${target_name}
            ALL
            DEPENDS ${bin_file}
            )

    set_target_properties(${target_name}
            PROPERTIES
            OUTPUT_NAME ${elf_file}
            )

endmacro(add_arm_executable)
