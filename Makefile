OCAMLC=ocamlc -g
OCAMLLEX=ocamllex
OCAMLDEP=ocamldep
OCAMLOPT=ocamlopt

BYTEOBJS=mail.cmo database.cmo mbox.cmo wordsplit.cmo rankmsg.cmo \
  processing.cmo main.cmo
BYTELIBS=unix.cma str.cma

NATOBJS=$(BYTEOBJS:.cmo=.cmx)
NATLIBS=$(BYTELIBS:.cma=.cmxa)

scrubmail: $(NATOBJS)
	$(OCAMLOPT) -o scrubmail $(NATLIBS) $(NATOBJS)

clean::
	rm -f scrubmail

scrubmail.byte: $(BYTEOBJS)
	$(OCAMLC) -o scrubmail.byte $(BYTELIBS) $(BYTEOBJS)

clean::
	rm -f scrubmail.byte

wordsplit.ml: wordsplit.mll
	$(OCAMLLEX) wordsplit.mll

clean::
	rm -f wordsplit.ml

beforedepend:: wordsplit.ml

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
