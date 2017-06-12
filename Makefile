# Copyright (C) 2017 Paul Brunet

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA.

EXEC=graphisam
WEB=$(EXEC).js
MAIN=wmain

WDIR=web/javascripts/
SRC= wmain.ml wmain.mli graph.ml graph.mli tools.ml tools.mli\
	woutput.ml woutput.mli unionFind.ml unionFind.mli machine.ml machine.mli
DOCDIR=doc.docdir
WDOCDIR=web/Files/graphisam.doc
DOC=$(DOCDIR)/index.html

WFLAGS= -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o
FLAGS= -lib unix
OCAML= ocamlbuild

all :: web doc
web : $(WEB)
doc : $(DOC)
clean :
	rm -rf *~ *.gv *.png _build \
	.depend* $(WDIR)$(WEB) $(MAIN).byte $(DOCDIR)

$(MAIN).byte : $(SRC)
	$(OCAML) $(FLAGS) $(WFLAGS) $(MAIN).byte

$(WEB) : $(MAIN).byte $(WDIR)
	js_of_ocaml $(MAIN).byte
	mv $(MAIN).js $(WDIR)$(WEB)

$(WDIR) :
	mkdir -p $(WDIR)

$(WDOCDIR) : 
	mkdir -p $(WDOCDIR)

$(DOC) : $(WDOCDIR)
	$(OCAML) $(DOC)
	mv _build/$(DOCDIR)/* $(WDOCDIR)/
