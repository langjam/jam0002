.PHONY: all clean

# Define custom functions
rwildcard = $(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))
platformpth = $(subst /,$(PATHSEP),$1)

target 		:= bin/wally.exe
sources 	:= $(call rwildcard,src/,*.cpp)
objects 	:= $(patsubst src/%, bin/%, $(patsubst %.cpp, %.o, $(sources)))
depends		:= $(patsubst %.o, %.d, $(objects))
includes 	:= -I vendor\PEGTL\include -I include

all: $(target) Makefile
	.\$(target) examples/if_test.wly

# Link the program and create the executable
$(target): $(objects)
	$(CXX) $(objects) -std=c++17 -o $(target)

# Add all rules from dependency files
-include $(depends)

# Compile objects to the build directory
bin/%.o: src/%.cpp Makefile
	$(CXX) -MMD -MP -c -std=c++17 $(includes) $< -o $@ $(CXXFLAGS)


%.wly: FORCE all
	.\$(target) $@
	gcc $*.c -o $*
	.\$*

FORCE: ;

clean:
	del bin\\*.exe bin\\*.d bin\\*.o
	del examples\\*.exe examples\\*.c
