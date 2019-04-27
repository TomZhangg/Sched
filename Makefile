.PHONY : all
all : schedch.native

schedch.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		schedch.native

.PHONY : debug
debug :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		schedch.d.byte

.PHONY : clean
clean :
	rm -f schedch.native schedch.d.byte parser.ml parser.mli scanner.ml *.cmo *.cmi && \
	rm -rf _build && \
	rm tests/*\.tmp tests/*\.ll tests/*\.s tests/*\.exe

