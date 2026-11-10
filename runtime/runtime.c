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

struct closure {
    void *(*function)(void *, void *);
    void *environment;
};

void *call_function(struct closure *c, void *argument) {
    return (c->function)(c->environment, argument);
}

struct io_closure {
    void *(*function)(void *); // a function that accepts a single argument and returns one value
    void *argument; // the argument to be passed to the function
};

struct io_binding {
    struct sum *left;
    struct closure *right;
};

void *do_io(struct sum *io) {
    // We disregard tag, since IO is a newtype and the tag will always be zero
    struct io_closure *closure = io->value;
    return (closure->function)(closure->argument);
}

void *do_binding(struct io_binding *binding) {
    void *left_value = do_io(binding->left);
    struct sum *right = call_function(binding->right, left_value);
    return do_io(right);
}

struct sum *bind(struct sum *left, struct closure *right) {
    struct io_binding *binding = malloc(sizeof(struct io_binding));
    binding->left = left;
    binding->right = right;

    struct io_closure *closure = malloc(sizeof(struct io_closure));
    closure->function = (void *(*)(void *)) do_binding;
    closure->argument = binding;

    struct sum *s = malloc(sizeof(struct sum));
    s->tag = 0;
    s->value = closure;

    return s;
}

void *io_read_character(void *unused) {
    char character;
    scanf("%c", &character);
    return (void *)character;
}

void *io_character(void *argument) {
    char character = (char) argument;
    printf("%c", character);
    return NULL;
}

void *io_identity(void *argument) {
    return argument;
}

struct sum *read_character_monad() {
    struct io_closure *closure = malloc(sizeof(struct io_closure));
    closure->function = io_read_character;
    closure->argument = NULL;

    struct sum *io = malloc(sizeof(struct sum));
    io->tag = 0;
    io->value = closure;

    return io;
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

struct sum *io_constructor(void *argument) {
    struct io_closure *closure = malloc(sizeof(struct io_closure));
    closure->function = io_identity;
    closure->argument = argument;

    struct sum *io = malloc(sizeof(struct sum));
    io->tag = 0;
    io->value = closure;

    return io;
}
