#include <ncurses.h>
#include <string.h>
#include <stdlib.h>

void print_in_middle(WINDOW* win, int starty, int startx, int width, char* string) {
    int x;
    int y;

    if (win == NULL)
        win = stdscr;

    getyx(win, y, x);
    if (startx != 0)
        x = startx;
    if (starty != 0)
        y = starty;
    if (width == 0)
        width = 80;

    int length = strlen(string);
    float temp = (width - length)/2;
    x = startx + (int)temp;
    mvwprintw(win, y, x, "%s", string);
    refresh();
}

int main() {
    initscr();
    noecho();

    if (has_colors() == FALSE) {
        endwin();
        printf("Your terminal does not support color\n");
        exit(1);
    }

    start_color();
    init_pair(1, COLOR_RED, COLOR_BLACK);

    attron(COLOR_PAIR(1));
    print_in_middle(stdscr, LINES / 2, 0, COLS, "This really is in color..");

    attroff(COLOR_PAIR(1));

    // possible redefining colors
    if (can_change_color()) {
        // Arguments are: COLOR_CONST, R, G, B
        init_color(COLOR_RED, 700, 0, 0);
    }

    getch();
    endwin();
    printf("Number of available colours: %d\n", COLORS);
    return 0;
}
