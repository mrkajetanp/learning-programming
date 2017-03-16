#include <ncurses.h>
#include <string.h>

int main() {
    char msg[] = "Just a string";
    int row, col;

    initscr();
    noecho();

    getmaxyx(stdscr, row, col);
    mvprintw(row/2, (col-strlen(msg))/2, "%s", msg);

    mvprintw(row-2, 0, "%d cols and %d rows\n", col, row);
    printw("Try resizing?\n");

    refresh();
    getch();
    endwin();

    return 0;
}
