#(***********************************************************************)
#(*                                                                     *)
#(*                           CIME Caml                                 *)
#(*                                                                     *)
#(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
#(*                                                                     *)
#(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
#(*  en Automatique. Distributed only by permission.                    *)
#(*                                                                     *)
#(***********************************************************************)

# $Id: Makefile,v 1.1 2004/07/07 12:18:11 weis Exp $

all: gui compute biconnect

# The GUI server
gui:
	ocamlc -custom -I +labltk -o gui labltk.cma gui.ml

# The computation server
compute:
	ocamlc -o compute compute.ml

# The pipes
biconnect:
	ocamlc -c bipipe.mli
	ocamlc -c bipipe.ml
	ocamlc -o biconnect unix.cma bipipe.cmo biconnect.ml

clean:
	rm -f *.cm[io] *~
	rm -f gui compute biconnect
