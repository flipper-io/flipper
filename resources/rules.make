# ~ Use 'find' to isolate compilable files. Any file names denoted by $(exclude) will not be factored into the search. ~ #

targets = $(shell find . -follow -type f $(findflags) -name '*.c' -or $(findflags) -name '*.s' -or $(findflags) -name '*.S' -or $(findflags) -name '*.asm')

# ~ Convert the target file names into object file names ~ #

objects = $(foreach source, $(targets), $(addsuffix .o, $(basename $(source))))

# ~ Use 'find' to discover any include directories. ~ #

includes = $(foreach directory, $(include_directories) /usr/local/include, -I "$(directory)")

# ~ Gather compatable linker scripts. ~ #

ldflags = $(foreach file, $(shell find $(hardware_directory) -follow -name '*.ld'), -L "$(dir $(file))" -Wl,-T "$(file)") $(libraries)

# ~ Specify the compilation prefix. ~ #

prefix = $(includes) $(preprocess)

# ~ On include, use the recipies below to compile all source files. ~ #

all: $(objects)

# ~ C source files with the '.c' extension. ~ #

$(patsubst %.c, %.o, $(filter %.c, $(targets))) : %.o : %.c

	$(cc) $(prefix) -c "$<" -o "$@"

# ~ Assembly source files with the '.s' extension. ~ #

$(patsubst %.s, %.o, $(filter %.s, $(targets))) : %.o : %.s

	$(cc) $(prefix) -x assembler-with-cpp -c "$<" -o "$@"

# ~ Assembly source files with the '.S' extension. ~ #

$(patsubst %.S, %.o, $(filter %.S, $(targets))) : %.o : %.S

	$(cc) $(prefix) -c "$<" -o "$@"

# ~ Assembly source files with the '.asm' extension. ~ #

$(patsubst %.asm, %.o, $(filter %.asm, $(targets))) : %.o : %.asm

	$(nasm) -f $(objformat) -o "$@" "$<"
