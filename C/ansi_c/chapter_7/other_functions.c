#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define RESET() printf ("%s\n", s1); resetStr (s1, b);

void resetStr (char s1[], char b[]) {
	strcpy (s1, b); /* copies b to s1 */
}

int main () {

	/* <string.h> Functions */

	char b[] = "hello my ";
	char s1[] = "hello my ";
	char s2[] = "friend";

	strcat (s1, s2); /* appends s2 to s1 */

	RESET();

	strncat (s1, s2, 4); /* appends 4 sings of s2 to s1 */

	RESET();

	printf ("%d\n", strcmp(s1, s2)); /* compares two strings */ 
	printf ("%d\n", strcmp("aa", "ab"));
	printf ("%d\n", strncmp ("aa", "aabbcc", 2)); /* same, only first two characters */

	strcpy (s1, s2); /* copies s2 to s1 */

	RESET();

	strncpy (s1, s2, 4);

	RESET();

	printf ("%d\n", strlen("aaa"));

	char *ret = strrchr (s1, 'l'); /* return a pointer to the last occurrence of 'l' */
	ret = strchr (s1, 'l'); /* return a pointer to the first occurrence of 'l' */
	if (ret == NULL)
		printf ("Not found!\n");
	printf ("%c\n", *(ret+2));
	printf ("String after |%c| is - |%s|\n", 'l', ret);

	/* <ctype.h> Functions */

	int c = 'a';

	isalpha (c); /* if c is a letter */
	isupper (c); /* if c is uppercase */
	islower (c); /* if c is lowercase */
	isdigit (c); /* if c is a digit */
	isalnum (c); /* if c is alphanumeric */
	isspace (c); /* if c is a whitespace */
	toupper (c); /* converts c to uppercase */
	tolower (c); /* converts c to lowercase */

	/* ungetc (c, FILE *fp); /* returns c to the file stream */

	/* System Calls */

	system ("ls");

	/* Memory Allocation */

	int *test = (int*) malloc(sizeof(int)); /* allocates a memory for one integer */
	*test = 8;
	printf ("%d\n", *test);

	int *a = (int*) calloc(10, sizeof(int)); /* allocates an array and fills it with 0s */

	int i;
	for (i = 0 ; i < 10 ; ++i)
		printf ("%d ", a[0]);
	printf ("\n");

	free (test);
	free (a);

	/* Mathematical Functions */

	printf ("%f\n", sin(18));
	printf ("%f\n", cos(18));
	printf ("%f\n", atan2(18, 3));
	printf ("%f\n", exp(2));
	printf ("%f\n", log(2));
	printf ("%f\n", log10(2));
	printf ("%f\n", pow(2, 5));
	printf ("%f\n", sqrt(81));
	printf ("%f\n", fabs(-22));

	/* Random Number Generator */

	srand (time(NULL));

	#define randn(a) (rand() % (a))

	printf ("%d\n", randn(20)); /* random int between 0 and 19 */

	int x = randn(80);
	printf ("%d\n", x);

	return 0;
}