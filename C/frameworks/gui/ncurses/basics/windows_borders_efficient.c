#include <ncurses.h>

typedef struct _winBorderStruct {
    chtype ls, rs, ts, bs; /* sides */
    chtype tl, tr, bl, br; /* corners */
} WinBorder;

typedef struct _winStruct {
    int startx, starty;
    int height, width;
    WinBorder border;
} Win;

void init_win_params(Win* p_win) {
    p_win->height = 5;
    p_win->width = 10;
    p_win->starty = (LINES - p_win->height)/2;
    p_win->startx = (COLS - p_win->width)/2;

    p_win->border.ls = '|';
    p_win->border.rs = '|';
    p_win->border.ts = '-';
    p_win->border.bs = '-';
    p_win->border.tl = '+';
    p_win->border.tr = '+';
    p_win->border.bl = '+';
    p_win->border.br = '+';
}

void print_win_params(Win* p_win) {
    mvprintw(25, 0, "%d %d %d %d", p_win->startx, p_win->starty,
             p_win->width, p_win->height);
    refresh();
}

void create_box(Win* p_win, bool flag) {
    int x = p_win->startx;
    int y = p_win->starty;
    int w = p_win->width;
    int h = p_win->height;

    if (flag == TRUE) {
        mvaddch(y, x, p_win->border.tl);
        mvaddch(y, x+w, p_win->border.tr);
        mvaddch(y+h, x, p_win->border.bl);
        mvaddch(y+h, x+w, p_win->border.br);
        mvhline(y, x+1, p_win->border.ts, w-1);
        mvhline(y+h, x+1, p_win->border.bs, w-1);
        mvvline(y+1, x, p_win->border.ls, h-1);
        mvvline(y+1, x+w, p_win->border.rs, h-1);
    }
    else {
        int i, j;
        for (i = y ; i <= y + h ; ++i)
            for (j = x ; j <= x+w ; ++j)
                mvaddch(i, j, ' ');
    }
    refresh();
}

int main() {
    initscr();
    start_color();
    cbreak();
    keypad(stdscr, TRUE);
    noecho();

    init_pair(1, COLOR_CYAN, COLOR_BLACK);
    Win win;

    /* Initialize the window parameters */
    init_win_params(&win);
    #ifdef _DEBUG
    print_win_params(&win);
    #endif

    attron(COLOR_PAIR(1));
    printw("Press q to exit");
    refresh();
    attroff(COLOR_PAIR(1));

    create_box(&win, TRUE);

    int ch;
    while ((ch = getch()) != 'q') {
        switch (ch) {
        case KEY_LEFT:
            create_box(&win, FALSE);
            --win.startx;
            create_box(&win, TRUE);
            break;
        case KEY_RIGHT:
            create_box(&win, FALSE);
            ++win.startx;
            create_box(&win, TRUE);
            break;
        case KEY_UP:
            create_box(&win, FALSE);
            --win.starty;
            create_box(&win, TRUE);
            break;
        case KEY_DOWN:
            create_box(&win, FALSE);
            ++win.starty;
            create_box(&win, TRUE);
            break;
        }
    }

    endwin();
    return 0;
}
