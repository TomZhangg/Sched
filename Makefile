.PHONY : all
all : schedch.native

schedch.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		schedch.native

.PHONY : clean
clean :
	rm -f schedch.native parser.ml parser.mli scanner.ml *.cmo *.cmi && \
	rm -rf _build && \
	rm tests/*\.tmp tests/*\.ll tests/*\.s tests/*\.exe

