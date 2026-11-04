#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void print_boolean(uint8_t value) {
    if (value) printf("true\n");
    else printf("false\n");
}

void print_integer(int32_t value) {
    printf("%d\n", value);
}

void print_character(uint8_t value) {
    printf("%c\n", value);
}

struct sum {
    int32_t tag;
    void *value;
};

struct string {
    int32_t c;
    void *next;
};

void print_string(struct sum *s) {
    if (s->tag == 1) {
        printf("\n");
        return;
    }

    struct string *content = s->value;
    printf("%c", (char)(content->c));
    print_string(content->next);
}

struct io_closure {
    void *(*function)(void *); // a function that accepts a single argument and returns one value
    void *argument; // the argument to be passed to the function
};

void do_io(struct sum *io) {
    // We disregard tag, since IO is a newtype and the tag will always be zero
    struct io_closure *closure = io->value;
    (closure->function)(closure->argument);
}

void *io_character(void *argument) {
    char character = (char) argument;
    printf("%c", character);
    return NULL;
}

struct sum *print_character_monad(void *character) {
    struct io_closure *closure = malloc(sizeof(struct io_closure));
    closure->function = io_character;
    closure->argument = character;

    struct sum *io = malloc(sizeof(struct sum));
    io->tag = 0;
    io->value = closure;

    return io;
}

int main() {
    struct sum* io = print_character_monad((void *) 'a');
    do_io(io);
}
