#include <ncurses.h>
#include <stdio.h>
#include <assert.h>

int main() {
    initscr();
    raw();
    noecho();
    keypad(stdscr, TRUE);

    printw("test\n");
    refresh();

    char text[] = "test";

    for (int i = 0 ; i < 4 ; ++i)
        assert((int)mvinch(0, i) == text[i]);

    printf("it worked!\n");


    endwin();
    return 0;
}
