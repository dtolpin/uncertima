# $Id: Makefile 928 2010-03-02 18:52:16Z tolpin $

all: mond

.SUFFIXES: .mli .ml .cmo .cmi

PACKAGES=-package "str,netstring,netcgi2,unix,nethttpd-for-netcgi2,netplex,json-wheel"

OCC=ocamlfind ocamlc
OCFLAGS=-g $(PACKAGES)

.mli.cmi:
	$(OCC) -c $(OCFLAGS) $<

.ml.cmo:
	$(OCC) -c $(OCFLAGS) $<

all: mond test_mond

mond: mond.cmo
	$(OCC) $(OCFLAGS) -o $@  $^ -linkpkg -g

test_mond: mond.cmo test_mond.cmo
	$(OCC) $(OCFLAGS) -o $@ -linkpkg -g $^

clean:
	rm -f *.cmi *.cmo *.cmx *.o

distclean: clean
	rm -f ./#*# ./*~ ./.*.sw? mond test_mond

mond.cmo: mond.ml mond.cmi
mond.cmi: mond.mli
test_mond.cmo: mond.cmi test_mond.ml
