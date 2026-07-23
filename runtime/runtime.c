#include <stdint.h>
#include <stdio.h>

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
