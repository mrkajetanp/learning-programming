#include <stdio.h>
#include <string.h>
#include <stdbool.h>

int main(int argc, char* argv[]) {
  printf("\n");

  /* structure allocates storage space for each of its members separately */
  /* u can use every element of struct in struct "object" */

  struct student {
    char class;
    char name[25];
    int age;
  };

  struct student s1;
  s1.age = 21;
  strcpy(s1.name, "Akki");
  printf("%s, %d\n", s1.name, s1.age);
  struct student s2;
  s2.age = 16;
  strcpy(s2.name, "Cajetan");
  printf("%s, %d\n", s2.name, s2.age);


  /* union is like a structure but allocates one common storage space for all its members */
  /* you can only use one element of a union when creating a union "object" */

  union worker {
    int age;
    char name[25];
    int salary;
  };

  union worker james = {25};
  union worker tom = {.name={"Tom"}};
  printf("%d\n", james.age);
  printf("%s\n", tom.name);

  printf("\n");

  /* a block scope */
  {
    int d = 20;
    printf("%d\n", d);
  }

  printf("Argc: %d\n", argc);
  printf("Argv[0] %s\n", argv[0]);
  printf("Argv[1] %s\n", argv[1]);

  bool a = true;
  printf("%d", a);

  return 0;
}


