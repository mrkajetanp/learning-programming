#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define MAXOP 100
#define NUMBER '0'
#define MAXVAL 100
#define BUFSIZE 100


int sp = 0; /* next empty stack position */
double val[MAXVAL]; /* stack */
char buf[BUFSIZE];
int bufp = 0;

void push (double);
double pop (void);
int getopt (char[]);
int getch (void);
void ungetch (int);

void push (double f) {
    if (sp < MAXVAL)
        val[sp++] = f;
    else
        printf("Error: Stack full, can't push %g\n", f);
}

double pop (void) {
    if (sp > 0)
        return val[--sp];
    else {
        printf("Error: Stack empty.\n");
        return 0.0;
    }
}

double peek (void) {
    if (sp > 0)
        return val[sp-1];
    else {
        printf ("Error: Stack empty.\n");
        return 0.0;
    }
}

int getop (char s[]) {
    int i, c, d;
    while ((s[0] = c = getch()) == ' ' || c == '\t')
        ;
    if (!isdigit(c) && c != '.') {
        if (c == '-') {
            if (isdigit(d = getch()) || d == '.')
                s[i = 1] = c = d;
            else {
                if (d != EOF)
                    ungetch (d);

                s[1] = '\0';
                return c;
            }
        }
        else {
            s[1] = '\0';
            return c;
        }
    } else {
        i = 0;
    }

    if (isdigit(c))
        while (isdigit(s[++i] = c = getch()))
            ;
    if (c == '.')
        while (isdigit(s[++i] = c = getch()))
            ;
    s[i] = '\0';
    if (c != EOF)
        ungetch(c);
    return NUMBER;
}

int getch (void) {
    return (bufp > 0) ? buf[--bufp] : getchar();
}

void ungetch (int c) {
    if (bufp >= BUFSIZE)
        printf("Ungetch: Too many characters\n");
    else
        buf[bufp++] = c;
}

int main () {
    int type;
    double op1, op2;
    char s[MAXOP];

    while ((type = getop(s)) != EOF) {
        switch (type) {
            case NUMBER:
                push(atof(s));
                break;
            case '+':
                push (pop() + pop());
                break;
            case '*':
                push (pop() * pop());
                break;
            case '-':
                op2 = pop();
                push(pop() - op2);
                break;
            case '/':
                op2 = pop();
                if (op2 != 0.0)
                    push (pop() / op2);
                else
                    printf ("Error: Zero divisor!\n");
                break;
            case '%':
                op2 = pop();
                op1 = pop();
                if (op2 != 0.0)
                    push (op1 - op2 * ((int) (op1/op2)));
                else
                    printf ("Error: Zero divisor!\n");
                break;
            case '\n':
                printf("\t%.8g\n", pop());
                break;
            default:
                printf ("Error: Unknown command %s\n", s);
                break;
        }
    }
    return 0;
}
