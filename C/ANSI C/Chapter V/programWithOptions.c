#include <stdio.h>

#define true 1
#define false 0

typedef int bool;

void myFunction (int a) {
    printf ("%d\n", a);
}

int main (int argc, char *argv[]) {
    int c;
    bool except = false, number = false;

    while (--argc > 0 && (*++argv)[0] == '-')
        while ((c = *++argv[0]))
            switch (c) {
                case 'x':
                    except = true;
                    break;
                case 'n':
                    number = true;
                    break;
                default:
                    printf ("Illegal option %c\n", c);
                    argc = 0;
                    break;
            }
    if (except)
        printf ("Except (-x) set !\n");
    if (number)
        printf ("Number (-n) set !\n");

    void (*foo)(int) = myFunction;
    foo(24);
    return 0;
}
