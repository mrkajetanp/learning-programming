#include <stdio.h>

char *monthName (int n) {
    static char *name[] = {
        "Illegal month",
        "January", "February", "March",
        "April", "May", "June",
        "July", "August", "September",
        "October", "November", "December"
    };
    return (n < 1 || n > 12) ? name[0] : name[n];
}

void echoAsArray (int argc, char *argv[]) {
    int i;
    for (i = 1 ; i < argc ; ++i)
        printf ("%s%s", argv[i], (i < argc-1) ? " " : "");
    printf("\n");
}

void echoAsPointer (int argc, char *argv[]) {
    while (--argc > 0)
        printf ((argc > 1) ? "%s " : "%s", *++argv);
    printf ("\n");
}

int main (int argc, char *argv[]) {
    printf("%s -> %s -> %s\n", monthName(6), monthName(7), monthName(8));
    echoAsPointer (argc, argv);
    return 0;
}
