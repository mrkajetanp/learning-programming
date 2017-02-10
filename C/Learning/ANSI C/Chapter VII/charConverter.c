#include <stdio.h>
#include <ctype.h>
#include <string.h>

int main (int argc, char *argv[]) {
	int c;
	if (strcmp(argv[0], "./charConverterU") == 0) {
		while ((c = getchar()) != EOF)
			putchar (toupper(c));
	}
	else if (strcmp (argv[0], "./charConverterL") == 0) {
		while ((c = getchar()) != EOF)
			putchar (tolower(c));
	}

	return 0;
}