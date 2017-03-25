#include <ncurses.h>

int main() {
    initscr(); /* start curses mode */
    printw("Hello World !"); /* print Hello World */
    refresh(); /* make actual changes to the screen */
    getch(); /* wait for user input */
    endwin(); /* end curses mode */

    return 0;
}
