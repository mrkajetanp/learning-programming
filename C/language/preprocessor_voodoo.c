#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define _Q(x) #x
#define Q(x) _Q(x)
#define print_calc(_a, _b, _op) printf("%2d "Q(_op)" %2d = %d\n", _a, _b, _a _op _b)
#define print_calc_div(_a, _b) printf("%2d / %2d = %d r %d\n", _a, _b, _a / _b, _a % _b)

#define rand_num(x) rand() % x + 1

#define print_block(OP)                         \
    _a1 = rand_num(20);                         \
    _b1 = rand_num(20);                         \
    print_calc(_a1, _b1, OP);                   \
    _a1 = rand_num(20);                         \
    _b1 = rand_num(20);                         \
    print_calc(_a1, _b1, OP);                   \
    _a1 = rand_num(20);                         \
    _b1 = rand_num(20);                         \
    print_calc(_a1, _b1, OP);                   \
    _a1 = rand_num(20);                         \
    _b1 = rand_num(20);                         \
    print_calc(_a1, _b1, OP);                   \
    printf("\n");

#define print_block_div()                       \
    _a1 = rand_num(20);                         \
    _b1 = rand_num(20);                         \
    print_calc_div(_a1, _b1);                   \
    _a1 = rand_num(20);                         \
    _b1 = rand_num(20);                         \
    print_calc_div(_a1, _b1);                   \
    _a1 = rand_num(20);                         \
    _b1 = rand_num(20);                         \
    print_calc_div(_a1, _b1);                   \
    _a1 = rand_num(20);                         \
    _b1 = rand_num(20);                         \
    print_calc_div(_a1, _b1);                   \
    printf("\n");

int main() {
    srand(time(NULL));

    int _a1, _b1;
    print_block(+);
    print_block(-);
    print_block(*);
    print_block_div();

    return 0;
}
