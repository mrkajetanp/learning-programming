#include <stdio.h>
#include <string.h>

#define MAXLINE 1000

int main () {
	FILE *fp1, *fp2;
	char line1[300], line2[300];

	if ((fp1 = fopen("test.txt", "r")) != NULL) {
		if ((fp2 = fopen("test2.txt", "r")) != NULL) {
			do {
				fgets (line1, MAXLINE, fp1);
				fgets (line2, MAXLINE, fp2);
			} while (strcmp (line1, line2) == 0);
			printf ("%s", line1);
		}
	}
	fclose (fp1);
	fclose (fp2);

	return 0;
}