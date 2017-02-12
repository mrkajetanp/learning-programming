#include <stdio.h>
#include <stdlib.h>
#include "memoryLeakTest.h"

int *alTest () {
	int *p = (int*)malloc(sizeof(int));
	*p = 15;
	return p;
}

int main () {
	printf ("Hey!\n");
	int *p = (int*)malloc(sizeof(int));
	*p = 10;
	printf ("%d\n", *p);
	free (p);

	int *d = alTest();
	printf ("%d\n", *d);
	free (d);

	printf ( "Header: %d\n", HEHE );

	return 0;
}
