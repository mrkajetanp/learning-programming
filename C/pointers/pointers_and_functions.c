#include <stdio.h>
#include <stdlib.h>

typedef int (*fptr)(int);

int square (int a) {
    return a*a;
}

int add (int a, int b) {
    return a+b;
}

int sub (int a, int b) {
    return a-b;
}

typedef int (*fptrOperation)(int, int);

int comp (fptrOperation operation, int a, int b) {
    return operation (a, b);
}

fptrOperation pick(char opcode) {
    switch (opcode) {
    case '+':
        return add;
    case '-':
        return sub;
    }
    return NULL;
}

int evaluate (char opcode, int a, int b) {
    fptrOperation operation = pick(opcode);
    return comp(operation, a, b);
}

int main () {
    printf("4^2 = %d\n", square(4));

    int (*fptrs1)(int) = square;
    printf("4^2 = %d\n", fptrs1(4));

    fptr s2 = square; /* thanks to the typedef */
    printf("4^2 = %d\n", s2(4));

    printf("8+4 = %d\n", comp(add, 8, 4));
    printf("8-4 = %d\n", comp(sub, 8, 4));

    printf("5+3 = %d\n", evaluate('+', 5, 3));
    printf("5-3 = %d\n", evaluate('-', 5, 3));

    /* arrays of function pointers */
    int (*operations[128])(int,int) = { NULL };
    operations['+'] = add;
    operations['-'] = sub;

    printf("18-6 = %d\n", operations['-'](18, 6));

    return 0;
}
