#include <stdio.h>

/* This attribute works only with GCC compiler because it's not a part of the language */
/* it implements destructor-like behaviour, useful for freeing variables when they go out of scope */

void scoped (int *pvar) {
    printf ("variable (%d) goes out of scope\n", *pvar);
}

int main () {
    printf ("before scope\n");

    {
        int watched __attribute__((cleanup (scoped)));
        watched = 42;
        printf ("in the scope - %d\n", watched);
    }

    printf ("after the scope");
    return 0;
}






