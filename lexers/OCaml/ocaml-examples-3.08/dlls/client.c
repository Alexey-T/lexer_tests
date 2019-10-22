/* An example client program for the fib.dll DLL. */
#include <stdio.h>

/* Importing functions from the DLL. */
extern int fib(int n);
extern char * format_result(int n);

/* Using them. */
int main()
{
  int res;
  char * s;

  printf("Calling fib(20)...\n");
  res = fib(20);
  printf("... the result is %d\n", res);
  printf("Formatting this result as a string in Caml:\n");
  s = format_result(res);
  printf("Printing it in C:\n");
  printf("%s\n", s);
  return 0;
}
