#include <stdio.h>

float fahrToC (int fahr) {
  return((5.0/9.0)*(fahr-32));
}

int main () {
  int fahr;
  printf("Fahr\tCelcius\n");
  for (fahr = 0 ; fahr <= 150 ; fahr += 20)
    printf("%3d\t%6.1f\n", fahr, fahrToC(fahr));
  return 0;
}
