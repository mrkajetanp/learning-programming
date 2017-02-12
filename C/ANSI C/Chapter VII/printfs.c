#include <stdio.h>
#include <stdarg.h>

void minprintf (char *fmt, ...) {
	va_list ap; /* points to next unnamed arguments */
	char *p, *sval;
	int ival;
	double dval;

	va_start (ap, fmt); /* assings location of first unnamed argument (by passing the last named) */

	for (p = fmt ; *p ; p++) {
		if (*p != '%') {
			putchar (*p);
			continue;
		}

		switch (*++p) {
			case 'd':
				ival = va_arg (ap, int);
				printf ("%d", ival);
				break;
			case 'f':
				dval = va_arg (ap, double);
				printf ("%f", dval);
				break;
			case 's':
				for (sval = va_arg(ap, char *) ; *sval ; sval++)
					putchar (*sval);
				break;
		}
	}
	va_end (ap); /* cleaning */
}

int main () {
	char s[] = "hello";
	int max = 4;
	printf ("%.*s\n", max, s);
	printf ("%.4s\n", s);

	printf (":%s:\n", "hello, world");
	printf (":%10s:\n", "hello, world");
	printf (":%.10s:\n", "hello, world");
	printf (":%-10s:\n", "hello, world");
	printf (":%.15s:\n", "hello, world");
	printf (":%-15s:\n", "hello, world");
	printf (":%15.10s:\n", "hello, world");
	printf (":%-15.10s:\n", "hello, world");

	char s2[20];
	sprintf (s2, "Test: %d", 10);
	printf ("%s\n", s2);

	printf ("%x\n", 138);
	printf ("%o\n", 138);

	minprintf ("Testing: %d -> %s\n", 10, "hehe");

	return 0;
}