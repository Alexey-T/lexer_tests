/* File fibwrap.c. C wrappers around the Caml functions exported by fib.ml. */

#include <wtypes.h>
#include <winbase.h>
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

int fib(int n)
{
  static value * fib_closure = NULL;
  if (fib_closure == NULL) fib_closure = caml_named_value("fib");
  return Int_val(callback(*fib_closure, Val_int(n)));
}

char * format_result(int n)
{
  static value * format_result_closure = NULL;
  if (format_result_closure == NULL)
    format_result_closure = caml_named_value("format_result");
  return strdup(String_val(callback(*format_result_closure, Val_int(n))));
  /* We copy the C string returned by String_val to the C heap
     so that it remains valid after garbage collection. */
}

BOOL APIENTRY DllMain(HANDLE module, DWORD reason, void *reserved)
{
  char * argv[2];

  switch (reason) {
  case DLL_PROCESS_ATTACH:
    argv[0] = "mydll";
    argv[1] = NULL;
    caml_startup(argv);
    break;
  }
  return TRUE;
}
