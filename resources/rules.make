ifndef PREFIX
ifneq (,$(findstring install,$(MAKECMDGOALS)))
$(error "Error. Please specify the environment variable 'PREFIX'. The 'PREFIX' variable will direct this install script to the install location appropriate for your system.")
endif
endif

# ~ Specify platform-inspecific utilities needed to deploy to any target. ~ #
rsync = $(shell which 'rsync')

# ~ Use 'find' to isolate compilable files. ~ #
targets = $(shell find . -follow -type f $(findflags) -name '*.c' -or $(findflags) -name '*.s' -or $(findflags) -name '*.S' -or $(findflags) -name '*.asm')

# ~ Convert the target file names into object file names ~ #
objects = $(foreach source, $(targets), $(addsuffix .o, $(basename $(source))))

# ~ Use 'find' to discover any include directories. ~ #
includes = $(foreach directory, $(include_directories),-I "$(directory)")

# ~ Gather compatable linker scripts. ~ #
ldflags = $(foreach file, $(shell find . -follow -name '*.ld'),-L "$(dir $(file))" -T "$(file)")

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