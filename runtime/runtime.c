#include <stdio.h>

struct sum {
    int tag;
    void *value;
};

struct string {
    int c;
    void *next;
};

void print(struct sum *s) {
    if (s->tag == 1) {
        printf("\n");
        return;
    }

    struct string *content = s->value;
    printf("%c", (char)(content->c));
    print(content->next);
}
