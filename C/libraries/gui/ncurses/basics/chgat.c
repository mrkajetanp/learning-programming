#include <ncurses.h>

int main(int arcg, char** argv) {
    initscr();
    start_color(); /* start color functionality */

    init_pair(1, COLOR_CYAN, COLOR_BLACK);
    printw("Some big string goes here..");
    mvchgat(0, 0, -1, A_BOLD, 1, NULL);
    /* first two - start position
       3rd - number of characters, -1 -> end of line
       4 - attribute to give
       5 - color index given during init_pair()
       0 if you don't want a color
       sixth one is always NULL
     */
    refresh();
    getch();
    endwin();
    return 0;
}
