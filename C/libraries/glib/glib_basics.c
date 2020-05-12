#include <glib.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("%d.%d.%d\n", glib_major_version, glib_minor_version, glib_micro_version);
  return 0;
}
