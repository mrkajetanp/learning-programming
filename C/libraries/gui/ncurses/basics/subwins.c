#include <stdio.h>
#include <ncurses.h>

int main() {
    initscr();
    raw();
    noecho();
    keypad(stdscr, TRUE);

    refresh();

    WINDOW* win = newwin(20, 20, 5, 5);

    mvwprintw(win, 1, 3, "hehe");

    mvwprintw(win, 0, 0, "+");
    for (int i = 1 ; i < 19 ; ++i)
        mvwprintw(win, 0, i, "-");
    mvwprintw(win, 0, 19, "+");

    for (int i = 1 ; i < 19; ++i)
        mvwprintw(win, i, 0, "|");

    for (int i = 1 ; i < 19; ++i)
        mvwprintw(win, i, 19, "|");

    mvwprintw(win, 19, 19, "+");
    mvwprintw(win, 19, 0, "+");
    for (int i = 1 ; i < 19 ; ++i)
        mvwprintw(win, 19, i, "-");

    wrefresh(win);

    getch();

    WINDOW* sub_win = subwin(win, 4, 4, 2, 2);

    mvwprintw(sub_win, 0, 0, "ok");

    touchwin(win);
    wrefresh(sub_win);

    getch();

    delwin(sub_win);
    delwin(win);
    endwin();
    return 0;
}
