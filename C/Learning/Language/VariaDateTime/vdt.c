#include <stdio.h>
#include <stdarg.h>
#include <time.h>
#include <stdint.h>

void simple_printf(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);

  while(*fmt != '\0') {
    if(*fmt == 'd') {
      int i = va_arg(args, int);
      printf("%d\n", i);
    } else if (*fmt == 'c') {
      // automatic conversion to integral type
      int c = va_arg(args, int);
      printf("%c\n", c);
    } else if (*fmt == 'f') {
      double d = va_arg(args, double);
      printf("%f\n", d);
    }
    ++fmt;
  }
  va_end(args);
}

int main() {
  puts("");

  /* variadic functions are functions which take a variable number of arguments */
  /* int printf(const char* format, ...);  */
  /* va_start - enables access to variadic function arguments */
  /* va_arg - accesses the next variadic function argument */
  /* va_copy - makes a copy of variadic function argumunts */
  /* va_end - ends traversal of the variadic function arguments */
  /* va_list - type holding the information */
  /* void va_copy( va_list dest, va_list src ); */

  simple_printf("dcff", 3, 'a', 1.999, 42.5);

  /* datetimes utils */
  /* double difftime( time_t time_end, time_t time_beg ); - computes difference between two calendar times as time_t objects */

  time_t now;
  time(&now);

  struct tm beg;
  beg = *localtime(&now);

  // set beg to the beginning of the month
  beg.tm_hour = 0;
  beg.tm_min = 0;
  beg.tm_sec = 0;
  beg.tm_mday = 1;

  double seconds = difftime(now, mktime(&beg));

  printf("%.f seconds have passed since the beginning of the months.\n", seconds);
  float minutes = seconds/60;
  float hours = minutes/60;
  printf("%.f days\n", hours/24);

  /* time - returns the current calendar time as time_t object */
  /* time_t time( time_t *arg ); arg - pointer to where time will be stored */

  time_t result = time(NULL);
  if(result != -1)
    printf("The current time is %s(%ju seconds since the Epoch)\n", asctime(gmtime(&result)), (uintmax_t)result);

  /* int timespec_get( struct timespec *ts, int base ); */

  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  char buff[100];
  strftime(buff, sizeof buff, "%D %T", gmtime(&ts.tv_sec));
  printf("Current time: %s.%09ld UTC\n", buff, ts.tv_nsec);


  puts("");
  return 0;
}









