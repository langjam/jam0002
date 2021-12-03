FILES=lang/*.c
DEBUG=-Wextra -Wall -Werror -pedantic -g -fsanitize=address

all:
	mkdir -p bin
	$(CC) $(FILES) $(DEBUG) -o bin/templ
	clear
	./bin/templ

