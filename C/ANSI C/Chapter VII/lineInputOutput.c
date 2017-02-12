#include <stdio.h>
#include <string.h>

/*
	char *fgets (char *line, int maxline, FILE *fp)
	int fputs (char *line, FILE *fp)

	gets()
	puts()
	^ Same functions as above, just using stdin / stdout instead of files
*/

char *fgets2 (char *s, int n, FILE *iop) {
	register int c;
	register char *cs;

	cs = s;
	while (--n > 0 && (c = getc(iop)) != EOF)
		if ((*cs++ = c) == '\n')
			break;
	*cs = '\0';
	return (c == EOF && cs == s) ? NULL : s;
}

int fputs2 (char *s, FILE *iop) {
	int c;

	while (c = *s++)
		putc (c, iop);
	return ferror (iop) ? EOF : 0;
}

int getline2 (char *line, int max) {
	if (fgets(line, max, stdin) == NULL)
		return 0;
	else
		return strlen (line);
}

int main () {

	return 0;
}