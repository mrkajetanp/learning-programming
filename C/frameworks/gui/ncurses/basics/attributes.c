#include <ncurses.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    int ch, prev, row, col;
    prev = EOF;
    FILE *fp;
    int y, x;

    if (argc != 2) {
        printf("Usage: %s <a c file name>\n", argv[0]);
        exit(1);
    }
    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        perror("Cannot open input file");
        exit(1);
    }

    initscr(); /* start curses mode */
    getmaxyx(stdscr, row, col); /* find the boundaries of the screen */
    while ((ch = fgetc(fp)) != EOF) { /* read the file till we reach the end */
        getyx(stdscr, y, x); /* get the current cursor position */
        if (y == (row - 1)) { /* if we're at the end of the screen */
            printw("<-Press Any Key->"); /* tell the user to press a key */
            getch();
            clear(); /* clear the screen */
            move(0, 0); /* start at the beginning of the screen */
        }
        if (prev == '/' && ch == '*') { /* if / and * next to each other */
            attron(A_BOLD); /* bold on */
            getyx(stdscr, y, x); /* get the current position */
            move(y, x-1); /* back up one space */
            printw("%c%c", '/', ch); /* the actual printing */
        }
        else {
            printw("%c", ch);
            refresh();
            if (prev == '*' && ch == '/') // switch it off at the end of a comment
                attroff(A_BOLD);
            prev = ch;
        }
    }

    attron(A_STANDOUT);
    printw("\nhohohohoh");
    attroff(A_STANDOUT);
    refresh();
    endwin();
    fclose(fp);
    return 0;
}

/*
  A_NORMAL        Normal display (no highlight)
  A_STANDOUT      Best highlighting mode of the terminal.
  A_UNDERLINE     Underlining
  A_REVERSE       Reverse video
  A_BLINK         Blinking
  A_DIM           Half bright
  A_BOLD          Extra bright or bold
  A_PROTECT       Protected mode
  A_INVIS         Invisible or blank mode
  A_ALTCHARSET    Alternate character set
  A_CHARTEXT      Bit-mask to extract a character
  COLOR_PAIR(n)   Color-pair number n

  attrset(A_BOLD); // sets the attributes of the window
  standend() == attrset(A_NORMAL);
  attr_get();
  attr_set();
  attr_on();
  etc..

  + wattr - window functions
*/
