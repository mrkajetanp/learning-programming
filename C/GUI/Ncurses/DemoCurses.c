#include <ncurses.h>
#include <unistd.h>

#define DELAY 25000

int main () {
    int x = 0, y = 2;
    int maxX = 0, maxY = 0;
    int nextX = 0;
    int direction = 1;

    initscr (); /* Initialize the window */
    noecho (); /* Don't display any keypresses */
    curs_set (FALSE); /* Don't display a cursor */

    /* mvprintw (10, 10, "Hello World!"); */
    /* refresh (); */

    while (1) {
        /* global var stdscr is created by calling initscr() */
        /* setting max coordinates in the loop handles resizing the window */
        getmaxyx (stdscr, maxY, maxX);

        clear (); /* clear the screen */
        mvprintw (y, x, "o"); /* print our "ball" at the current xy position */
        refresh ();

        usleep (DELAY); /* shorter delay between movements */

        nextX = x + direction;

        if (nextX >= maxX || nextX < 0)
            direction *= -1; /* reverse direction */
        else
            x += direction; /* continue */
    }

    endwin (); /* restore normal terminal behaviour */
}



