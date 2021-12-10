.PHONY: all
all: patty

CXX=g++
CXXFLAGS=-std=c++20 -Wall -Wextra -Werror=switch

Objects=build/context.o\
				build/intrinsic.o\
				build/patty.o\
				build/sequence.o\
				build/value.o

patty: $(Objects) src/*.hh
	$(CXX) $(CXXFLAGS) -O3 -lfmt -o $@ $(Objects)

build/%.o: src/%.cc src/*.hh | build
	$(CXX) $(CXXFLAGS) -c -O3 -o $@ $<

build:
	mkdir -p build

.PHONY: clean
clean:
	rm -f patty build/*
	rmdir -f build
