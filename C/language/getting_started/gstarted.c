#include <stdio.h>

#define LOWER 0 /* lower limit */

int main() {

  printf("\nHello World!\n\n");

  /* Print Fahhrenheit-Celsius table for fahr 0, 20 , ...., 300 */

  printf("Fahhrenheit to Celsius:\n\n");

  float fahr, celsius;
  int upper, step;

  upper = 300;
  step = 20;

  fahr = LOWER;
  while(fahr <= upper) {
    celsius = (5.0/9.0) * (fahr-32.0);
    printf("%3.0f %6.1f\n", fahr, celsius);
    fahr = fahr + step;
  }

  printf("\nCelsius to Fahhrenheit:\n\n");

  int lower = -18;
  upper = 100;
  step = 5;

  celsius = lower;
  while(celsius <= upper) {
    fahr = celsius * (9.0/5.0) + 32;
    printf("%3.0f %6.1f\n", celsius, fahr);
    celsius += step;
  }

  printf("\nThe same using a for statement:\n\n");

  int ffahr;

  for(ffahr = LOWER; ffahr <= 300; ffahr = ffahr + 20) {
    printf("%3d %6.1f\n", ffahr, (5.0/9.0)*(ffahr-32));
  }

  printf("Now chars...\n\n");
  /*
  int c;
  while((c = getchar()) != EOF) {
    putchar(c);
  }


  double nc;
  for(nc = 0 ; getchar() != EOF ; ++nc)
    ;
  printf("%.0f\n", nc);
  */

  /* Line Counting */

  int c, lines = 0;
  while ((c = getchar()) != EOF)
    if(c == '\n')
      ++lines;
  printf("Lines: %d\n", lines);

  return 0;
}

