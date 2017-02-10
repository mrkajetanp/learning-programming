#include <stdio.h>

int naiveDiv(int divident, int divisor) {
  int quotient = 0; // return result
  int loopTime = 0; // looping time measurement

  while(divident >= divisor) {
    loopTime++;
    divident -= divisor; // while the remainder is larger than divisor, we keep substracting
    quotient++; // updating quotient
  }
  printf("Naive loop time: %d\n", loopTime);
  return quotient;
}

int optDiv(int divident, int divisor) {
  int currentQuotientBase = 1, currentDivisor = divisor, quotient;
  int loopTime = 0; // loop time measurement

  while(divident >= divisor) {
    loopTime++;
    if(divident >= currentDivisor) {
      divident -= currentDivisor;
      quotient += currentQuotientBase;

      currentDivisor *= 2;
      currentQuotientBase *= 2;
    } else {
      currentDivisor /= 2;
      currentQuotientBase /= 2;
    }
  }
  printf("Optimal loop time: %d\n", loopTime);
  return quotient;
}

int main() {
  puts("test");
  printf("%d %d", naiveDiv(100000,2), optDiv(100000,2));

  return 0;
}
