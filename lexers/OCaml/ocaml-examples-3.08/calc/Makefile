# Makefile generated by imake - do not edit!
# $XConsortium: imake.c,v 1.91 95/01/12 16:15:47 kaleb Exp $

LIBDIR=/usr/local/lib/ocaml/
CAMLC=ocamlc
CAMLCOPT=ocamlopt
COMPFLAGS=-g
COMPFLAGSOPT=
CAMLLIBR=ocamllibr
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLDEP=ocamldep -pp camlp4o
CPP=/lib/cpp -P
RM=rm -f
CP=cp -p -f
TOP=../.
CONFIG=$(TOP)/config

all::
allopt:opt

# (begin Imakefile)

calc.OBJS=lexer.cmo parser.cmo calc.cmo main.cmo
calc.FILES=$(calc.OBJS:.cmo=.ml)

all::calc

depend::
	$(CAMLDEP) $(calc.FILES) >> Makefile

clean::
	$(RM) calc

calc: lexer.cmo parser.cmo calc.cmo main.cmo
	$(CAMLC) $(LINKFLAGS)  lexer.cmo parser.cmo calc.cmo main.cmo -o calc

calc.BINOBJS=lexer.cmx parser.cmx calc.cmx main.cmx
calc.FILES=$(calc.BINOBJS:.cmx=.ml)

opt::calc.opt

depend::
	$(CAMLDEP) $(calc.FILES) >> Makefile

clean::
	$(RM) calc calc.opt

calc.opt: lexer.cmx parser.cmx calc.cmx main.cmx
	$(CAMLCOPT) $(LINKFLAGSOPT)  lexer.cmx parser.cmx calc.cmx main.cmx -o calc.opt
	$(CP) calc.opt calc

parser.ml parser.mli: parser.mly
	@$(RM) parser.ml parser.mli
	$(CAMLYACC) parser.mly
	@chmod a-w parser.ml parser.mli

clean::
	$(RM) parser.ml parser.mli

beforedepend:: parser.ml parser.mli

lexer.ml: lexer.mll
	@$(RM) lexer.ml
	$(CAMLLEX) lexer.mll
	@chmod a-w lexer.ml

clean::
	$(RM) lexer.ml

beforedepend:: lexer.ml

# (end Imakefile)

clean::
	$(RM) *.cmi *.cmix *.cmo *.cmx *.o a.out* *~ *.opt #*#

depend::

beforedepend::

mfile:
	imake -I$(CONFIG) -DTOPDIR=$(TOP)

makefiles:: beforedepend mfile depend

.SUFFIXES: .mli .cmi .ml .cmo .cmx

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLCOPT) $(COMPFLAGSOPT) -c $<

# Generated dependencies below

lexer.cmo: parser.cmi 
lexer.cmx: parser.cmx 
parser.cmo: parser.cmi 
parser.cmx: parser.cmi 
calc.cmo: lexer.cmo parser.cmi 
calc.cmx: lexer.cmx parser.cmx 
main.cmo: calc.cmo 
main.cmx: calc.cmx 
lexer.cmo: parser.cmi 
lexer.cmx: parser.cmx 
parser.cmo: parser.cmi 
parser.cmx: parser.cmi 
calc.cmo: lexer.cmo parser.cmi 
calc.cmx: lexer.cmx parser.cmx 
main.cmo: calc.cmo 
main.cmx: calc.cmx 
