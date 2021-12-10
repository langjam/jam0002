UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	FILES=lang/*.c map/*.c raylib/lib/libraylib.a
else
	FILES=lang/*.c map/*.c -lraylib
endif
FILES_WIN=lang/*.c map/*.c raylib/lib/libraylib_windows.a
FILES_MAC=lang/*.c map/*.c raylib/lib/libraylib_macos.a
TESTS=test/*.c
FLAGS=-I. -Iraylib/include/ 
LINUX_FLAGS=-ldl -lpthread -lm
DEBUG=-Wextra -Wall -Werror -pedantic -g -fsanitize=address -Wno-unused-parameter

ifeq ($(OS),Windows_NT)
	MAKECMD=$(CC) -mwindows -static main.c $(FLAGS) $(FILES_WIN) -O2 -lgdi32 -lwinmm -o release/templ.exe
	RUNCMD=bin/templ.exe input/playground.css
else	
	RUNCMD=bin/templ input/playground.css
	ifeq ($(UNAME_S),Darwin)
		MAKECMD=$(CC) main.c $(FLAGS) $(LINUX_FLAGS) $(FILES_MAC) -o bin/templ
		echo "NOTE: BUILDING ON MAC OS MAY BE BROKEN, ITS NOT TESTED AT ALL!" 
	else
		MAKECMD=$(CC) main.c $(FLAGS) $(LINUX_FLAGS) $(FILES) $(DEBUG) -o bin/templ
	endif
endif

templ: 
	mkdir -p bin
	$(MAKECMD)

all: templ
	clear
	$(RUNCMD) 

release:
	mkdir -p release
	$(CC) main.c $(FLAGS) $(LINUX_FLAGS) $(FILES) -O2 -o release/templ_linux.x86_64
	x86_64-w64-mingw32-gcc -mwindows -static main.c $(FLAGS) $(FILES_WIN) -O2 -lgdi32 -lwinmm -o release/templ.exe

tests:
	mkdir -p bin
	$(CC) $(FILES) $(FLAGS) $(TESTS) $(DEBUG) -o bin/test
	clear
	./bin/test
	
	

.PHONY: all tests release templ 
