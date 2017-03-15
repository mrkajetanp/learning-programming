#include <ncurses.h>

int main() {
    int ch;

    initscr(); /* start curses mode */
    raw(); /* line buffering disabled */
    keypad(stdscr, TRUE); /* F1, F2, etc.. */
    noecho(); /* don't echo while we do getch() */

    printw("Type any character to see it in bold\n");
    ch = getch(); /* if raw() hadn't been called, we have to press enter
                   * before it gets to the program */

    if (ch == KEY_F(1)) /* without keypad enabled this won't get to us either */
        printw("F1 Key pressed");
    /* without noecho some ugly escape characters might've been printed on screen */
    else {
        printw("The pressed key is ");
        attron(A_BOLD);
        printw("%c", ch);
        attroff(A_BOLD);
    }
    refresh();
    getch();
    endwin();

    return 0;
}
