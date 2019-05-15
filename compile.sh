make clean
make

./schedch.native $1 -c > $1.ll
clang -emit-llvm -S sched.c -o sched.ll
llvm-as $1.ll -o $1.bc
llvm-as sched.ll -o sched.bc
llvm-link $1.bc sched.bc -o out.bc
chmod 777 out.bc
rm sched.ll
