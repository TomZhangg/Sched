.PHONY : all
all : schedch.native

schedch.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind schedch.native

.PHONY : clean
clean :
	rm -f schedch.native parser.ml parser.mli scanner.ml *.cmo *.cmi && \
	rm -rf _build

