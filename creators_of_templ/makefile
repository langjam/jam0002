UNAME_S := $(shell uname -s)

FILES=lang/*.c map/*.c 

TESTS=test/*.c
FLAGS=-I. -lraylib -Iraylib/include/ -I/usr/local/include -L/usr/local/lib
LINUX_FLAGS=-ldl -lpthread -lm
DEBUG=-Wextra -Wall -Werror -pedantic -g -fsanitize=address -Wno-unused-parameter

ifeq ($(OS),Windows_NT)
	MAKECMD=$(CC) -mwindows -static main.c $(FLAGS) $(FILES) -O2 -lgdi32 -lwinmm -o release/templ.exe
	RUNCMD=bin/templ.exe input/playground.css
else	
	RUNCMD=bin/templ input/playground.css
	ifeq ($(UNAME_S),Darwin)
		MAKECMD=$(CC) main.c $(FLAGS) $(LINUX_FLAGS) $(FILES) -o bin/templ
		echo "NOTE: BUILDING ON MAC OS MAY BE BROKEN, ITS NOT TESTED AT ALL!" 
	else
		MAKECMD=$(CC) main.c $(FLAGS) $(LINUX_FLAGS) $(FILES) $(DEBUG) -o bin/templ
	endif
endif

all: templ
	clear
	$(RUNCMD) 

templ: 
	mkdir -p bin
	$(MAKECMD)
	
release:
	mkdir -p tbin
	$(CC) main.c $(FLAGS) $(LINUX_FLAGS) $(FILES) -O2 -o bin/templ

tests:
	mkdir -p bin
	$(CC) $(FILES) $(FLAGS) $(TESTS) $(DEBUG) -o bin/test
	clear
	./bin/test
	
	

.PHONY: all tests templ 
