#    <vscr - Verilog converter to abc format.>
#    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

PARSEROPT=--table -b vparser --explain # --trace

CMO = vparser.cmo vlexer.cmo vparse.cmo
CMX = vparser.cmx vlexer.cmx vparse.cmx
CML = str.cma unix.cma
CMLX = str.cmxa unix.cmxa

all: ver_front.cma ver_front.cmxa vtop

vtop: $(CMO)
	ocamlfind ocamlmktop -package menhir,menhirlib -linkpkg -g -o $@ $(CML) -custom $(PARSERLIB) $(CMO)

ver_front.cma: $(CMO)
	ocamlc.opt -a -g -o $@ $(CMO)

ver_front.cmxa: $(CMX)
	ocamlopt.opt -a -g -o $@ $(CMX)

vparser.mli vparser.ml: idhash.cmi grammar.mly
	rm -f vparser.mli
	menhir $(PARSEROPT) grammar.mly
	ocamlfind ocamlc -package menhir,menhirlib -g -c vparser.mli vparser.ml

vparser.cmo: vparser.ml
	ocamlfind ocamlc -package menhir,menhirlib -g -c $<

vparser.cmx: vparser.ml
	ocamlfind ocamlopt -package menhir,menhirlib -g -c $<

depend: vparser.ml vlexer.ml
	ocamldep *.ml *.mli *.mll *.mly > .depend

clean:
	rm -rf *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa vchk vtop vparser vparser.conflicts
	rm -rf vparser.ml vparser.mli vlexer.ml vlexer.mli vparser.mli vparser.ml

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx

.ml.cmo:
	ocamlfind ocamlc -package menhir,menhirlib -g -c $<

.mli.cmi:
	ocamlfind ocamlc -package menhir,menhirlib -g -c $<

.mll.ml:
	ocamllex.opt $(LEXOPTS) $<

.ml.cmx:
	ocamlopt.opt $(INC) -g -c $<

vparser.cmi: vparser.mli
	ocamlfind ocamlc -package menhirlib -g -c vparser.mli

ocamlyacc:

include .depend
