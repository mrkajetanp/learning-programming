#include <stdio.h>

void print_5_times(int a) {
    int cnt = 0;
 START:
    printf("%d: %d \n", cnt+1, a);
    if (++cnt == 5)
        goto END;
    goto START;
 END:
    ;
}

int main() {
    print_5_times(4);
    return 0;
}
