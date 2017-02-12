#include <stdio.h>

/*
	sscanf = scanf
*/

void simpleCalc () {
	double sum = 0, v;
	while (scanf("%lf", &v) == 1)
		printf ("\t%.2f\n", sum += v);
}

/*
void dateGetter () {
	int day, month, year;
	char line[200], monthname[20];

	while (getline(line, sizeof(line)) > 0) { // with self made getline
		if (sscanf(line, "%d %s %d", &day, monthname, &year) == 3)
			printf ("Valid date: %s\n", line);	// 10 Jan 2017
		else if (sscanf(line, "%d/%d/%d", &day, &month, &year) == 3)
			printf ("Valid date: %s\n", line); // 10/01/2017
		else
			printf ("Invalid: %s\n", line);	// invalid date format
	}
}
*/

int main () {
	int v1, v2;
	char s[] = "12 15";
	sscanf (s, "%d %d", &v1, &v2);
	printf ("%d + %d = %d\n", v1, v2, v1+v2);

	// simpleCalc ();

	int day, month, year;
	char monthname[20], s2[] = "10 January 2017", s3[] = " 10/01/2017 ";

	sscanf (s2, "%d %s %d", &day, monthname, &year);
	printf ("Date: %d-%s-%d\n", day, monthname, year);

	sscanf (s3, "%d/%d/%d", &day, &month, &year);
	printf ("%d.%d.%d\n", day, month, year);

	return 0;
}