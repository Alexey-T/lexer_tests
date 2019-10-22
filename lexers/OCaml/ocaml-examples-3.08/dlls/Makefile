CAMLLIB=\camlfi\lib
CAMLC=ocamlc
CAMLOPT=ocamlopt
CAMLRUNLIB=libcamlrun.lib
CAMLASM=libasmrun.lib

client.exe: client.c camldll.lib
	cl -o client.exe client.c camldll.lib

camldll.dll camldll.lib: modcaml.obj modwrap.obj
	link /nologo /dll /libpath:"$(CAMLLIB)" \
          /out:camldll.dll /implib:camldll.lib \
	  /export:fib \
	  /export:format_result \
	  modwrap.obj modcaml.obj \
	  libcamlrun.lib

modwrap.obj: modwrap.c
	ocamlc -c modwrap.c

modcaml.obj: fib.ml
	ocamlc -output-obj -o modcaml.obj fib.ml
