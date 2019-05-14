.PHONY : all
all : schedch.native sched.o

schedch.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitreader -cflags -w,+a-4 \
		schedch.native
	gcc -c sched.c
	clang -emit-llvm -o sched.bc -c sched.c -Wno-varargs

.PHONY : debug
debug :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		schedch.d.byte

.PHONY : clean
clean :
	rm -f schedch.native schedch.d.byte parser.ml parser.mli scanner.ml *.cmo *.cmi *.o *.bc && \
	rm -rf _build && \
	rm tests/*\.tmp tests/*\.ll tests/*\.s tests/*\.exe

