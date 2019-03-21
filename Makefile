OBJS = ast.cmo parser.cmo scanner.cmo schedch.cmo

schedch : $(OBJS)
	ocamlc -o schedch $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f schedch parser.ml parser.mli scanner.ml *.cmo *.cmi

ast.cmo :
ast.cmx :
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
parser.cmi : ast.cmo
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
schedch.cmo : scanner.cmo parser.cmi ast.cmo
schedch.cmx : scanner.cmx parser.cmx ast.cmx

