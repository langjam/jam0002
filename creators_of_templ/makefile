FILES=lang/*.c map/*.c
TESTS=test/*.c
FLAGS=-I.
DEBUG=-Wextra -Wall -Werror -pedantic -g -fsanitize=address

all:
	mkdir -p bin
	$(CC) main.c $(FLAGS) $(FILES) $(DEBUG) -o bin/templ
	clear
	./bin/templ

tests:
	mkdir -p bin
	$(CC) $(FILES) $(FLAGS) $(TESTS) $(DEBUG) -o bin/test
	clear
	./bin/test
	
	
.PHONY: all tests
