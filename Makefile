### Configuration section

# The laguages you're interested in, besides English
LANGUAGES=-DFRENCH #-DSPANISH -DITALIAN -DGERMAN -DPORTUGUESE

# How to invoke the C preprocessor
CPP=gcc -E -P $(LANGUAGES) -

# Where to install the binary
BINDIR=/usr/local/bin

# Where to install the man page
MANEXT=1
MANDIR=/usr/local/man/man$(MANEXT)

### End of configuration section

OCAMLC=ocamlc -g
OCAMLLEX=ocamllex
OCAMLDEP=ocamldep
OCAMLOPT=ocamlopt

BYTEOBJS=htmlscan.cmo mail.cmo database.cmo mbox.cmo wordsplit.cmo \
  rankmsg.cmo attachments.cmo processing.cmo main.cmo
BYTELIBS=unix.cma str.cma

NATOBJS=$(BYTEOBJS:.cmo=.cmx)
NATLIBS=$(BYTELIBS:.cma=.cmxa)

all: spamoracle

install:
	cp spamoracle $(BINDIR)/spamoracle
	cp spamoracle.1 $(MANDIR)/spamoracle.$(MANEXT)

spamoracle: $(NATOBJS)
	$(OCAMLOPT) -o spamoracle $(NATLIBS) $(NATOBJS)

clean::
	rm -f spamoracle

spamoracle.byte: $(BYTEOBJS)
	$(OCAMLC) -o spamoracle.byte $(BYTELIBS) $(BYTEOBJS)

clean::
	rm -f spamoracle.byte

wordsplit.mll: wordsplit.mlp
	$(CPP) < wordsplit.mlp > wordsplit.mll \
        || { rm -f wordsplit.mll; exit 2; }

clean::
	rm -f wordsplit.mll

wordsplit.ml: wordsplit.mll
	$(OCAMLLEX) wordsplit.mll

clean::
	rm -f wordsplit.ml

beforedepend:: wordsplit.ml

htmlscan.ml: htmlscan.mll
	$(OCAMLLEX) htmlscan.mll

clean::
	rm -f htmlscan.ml

beforedepend:: htmlscan.ml

clean::
	rm -f *.cm[iox] *.o

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

depend: beforedepend
	$(OCAMLDEP) *.ml *.mli > .depend

include .depend
