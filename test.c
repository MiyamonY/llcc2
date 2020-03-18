#include <stdio.h>

int foo() {
  printf("OK\n");
  return 0;
}

int foo3(int a, int b, int c) { return a * b + c + 3; }

int foo6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}
