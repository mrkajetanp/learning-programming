#include <ncurses.h>

WINDOW* create_new_win(int height, int width, int starty, int startx) {

    WINDOW* local_win = newwin(height, width, starty, startx);
    box(local_win, 0, 0); /* 0,0 gives default characters for the
                           vertical and horizontal lines */
    wrefresh(local_win); /* refresh our window */

    return local_win;
}

void destroy_win(WINDOW* local_win) {
    wborder(local_win, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ');
    /* window, ls, rs, ts, bs, tl(c), tr(c), bl(c), br(c) */
    wrefresh(local_win);
    delwin(local_win);
}

int main() {
    initscr();
    cbreak();
    noecho();

    keypad(stdscr, TRUE);

    int height = 5;
    int width = 10;
    int starty = (LINES - height) / 2; /* calculating for a center placement */
    int startx = (COLS - width) / 2; /* of the window */

    printw("Press q to exit");
    refresh();

    WINDOW* my_win = create_new_win(height, width, starty, startx);

    int ch;
    while ((ch = getch()) != /*(KEY_F(1))*/ 'q') {
        switch (ch) {
        case KEY_LEFT:
            destroy_win(my_win);
            my_win = create_new_win(height, width, starty, --startx);
            break;
        case KEY_RIGHT:
            destroy_win(my_win);
            my_win = create_new_win(height, width, starty, ++startx);
            break;
        case KEY_UP:
            destroy_win(my_win);
            my_win = create_new_win(height, width, --starty, startx);
            break;
        case KEY_DOWN:
            destroy_win(my_win);
            my_win = create_new_win(height, width, ++starty, startx);
            break;
        }
    }
    endwin();

    return 0;
}
