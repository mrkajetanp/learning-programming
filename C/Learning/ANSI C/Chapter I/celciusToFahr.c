#include <stdio.h>

float celToFahr (int cel) {
  return(cel*1.8+32);
}

int main () {
  int celcius;
  printf("Celcius\t   Fahr\n");
  for (celcius = -10 ; celcius <= 40 ; celcius += 5)
    printf("%3d\t%6.f\n", celcius, celToFahr(celcius));

  return 0;
}
