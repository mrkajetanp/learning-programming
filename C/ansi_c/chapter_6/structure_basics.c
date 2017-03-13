#include <stdio.h>

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

struct point {
    int x;
    int y;
};

struct rect {
    struct point pt1;
    struct point pt2;
};


struct point makepoint (int x, int y) {
    struct point temp;
    temp.x = x;
    temp.y = y;
    return temp;
}

struct point addpoint (struct point p1, struct point p2) {
    p1.x += p2.x;
    p1.y += p2.y;
    return p1;
}

int ptinrect (struct point p, struct rect r) {
    return p.x >= r.pt1.x && p.x < r.pt2.x
        && p.y >= r.pt1.y && p.y < r.pt2.y;
}

struct rect canonrect (struct rect r) {
    struct rect temp;
    temp.pt1.x = min (r.pt1.x, r.pt2.x);
    temp.pt1.y = min (r.pt1.y, r.pt2.y);
    temp.pt2.x = max (r.pt1.x, r.pt2.x);
    temp.pt2.y = max (r.pt1.y, r.pt2.y);
    return temp;
}

struct {
    int len;
    char *str;
} hey, *p = &hey;

int main () {
    struct rect screen;
    struct point middle;
    struct point makepoint (int, int);

    screen.pt1 = makepoint (0,0);
    screen.pt2 = makepoint (800, 600);
    middle = makepoint ((screen.pt1.x + screen.pt2.x)/2,
                        (screen.pt1.y + screen.pt2.y)/2);
    struct point origin, *pp;
    pp = &origin;
    printf ("Origin is (%d,%d)\n", (*pp).x, (*pp).y);
    (*pp).y = 18;
    printf ("Origin is (%d,%d)\n", (*pp).x, (*pp).y);
    printf ("Origin is (%d,%d)\n", pp->x, pp->y);
    printf ("Origin is (%d,%d)\n", origin.x, origin.y);
    printf ("Middle: (%d,%d)\n", middle.x, middle.y);
    p->len = 10;
    p->str = "test";
    printf ("Hey: %d, %s\n", p->len, p->str);
    printf ("Hey: %d, %s\n", hey.len, hey.str);

    return 0;
}
