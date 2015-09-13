# ~ Remove any lingering '.DS_Store' files. ~ #

$(shell find . -name '.DS_Store' -exec rm -rf {} \;)

# ~ Use 'find' to isolate compilable files. Any file names denoted by $(exclude) will not be factored into the search. ~ #

targets = $(shell find . -follow -type f $(findflags) -name '*.c' -or -name '*.s' -or -name '*.S')

$(info $(targets))

# ~ Convert the target file names into object file names ~ #

objects = $(foreach source, $(targets), $(addsuffix .o, $(basename $(source))))

# ~ Use 'find' to discover any include directories. ~ #

includes = $(foreach directory, $(FLIPPERSDK)/include $(shell find . -follow -type d -name 'include') $(include_directories), -I "$(directory)")

# ~ Gather compatable linker scripts. ~ #

ldflags = $(foreach directory, $(shell find . -follow -name '*.ld'), -Wl,-T "$(directory)")

# ~ Specify the compilation prefix. ~ #

prefix = $(includes) $(preprocess)

# ~ On include, use the recipies below to compile all source files. ~ #

all: $(objects)

# ~ C source files with the '.c' extension. ~ #

$(patsubst %.c, %.o, $(filter %.c, $(targets))) : %.o : %.c

	$(cc) $(prefix) -c "$<" -o "$@"

# ~ Assembly source files with the '.s' extension. ~ #

$(patsubst %.s, %.o, $(filter %.s, $(targets))) : %.o : %.S

	$(cc) $(prefix) -c "$<" -o "$@"

# ~ Assembly source files with the '.S' extension. ~ #

$(patsubst %.S, %.o, $(filter %.S, $(targets))) : %.o : %.S

	$(cc) $(prefix) -c "$<" -o "$@"
