CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -O2 -g
BINARY_NAME = quantization
objects = $(patsubst src/%.c, release/%.o, $(wildcard src/*.c))
.PHONY: all clean 

all: $(BINARY_NAME)

$(BINARY_NAME): $(objects)
	$(CC) $^ -o $(BINARY_NAME)
	
release/%.o: src/%.c
	@mkdir -p release/
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@
	
clean:
	@rm -rf release/
	@rm -f $(BINARY_NAME)
