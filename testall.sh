make clean
make

./schedch.native -a tests/create.sched > tests/create.sched.out.tmp
./schedch.native -a tests/insert_statement_parsing/_smoke/insert.sched > tests/insert_statement_parsing/_smoke/insert.sched.out.tmp
./schedch.native -a tests/set_statement_parsing/_smoke/set.sched > tests/set_statement_parsing/_smoke/set.sched.out.tmp
./schedch.native -a tests/boolean_operations_parsing/_smoke/boolean.sched > tests/boolean_operations_parsing/_smoke/boolean.sched.out.tmp
./schedch.native -a tests/print/_smoke/print.sched > tests/print/_smoke/print.sched.out.tmp

./schedch.native -s tests/print_sast.sched > tests/print_sast.sched.out.tmp
./schedch.native -c tests/print_sast.sched > tests/print_sast.ll && llc tests/print_sast.ll > tests/print_sast.s &&\
	cc -o tests/print_sast.exe tests/print_sast.s && ./tests/print_sast.exe > ./tests/print_sast.exe.out.tmp

cmp --silent tests/create.sched.output tests/create.sched.out.tmp && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/insert_statement_parsing/_smoke/insert.sched.out.tmp tests/insert_statement_parsing/_smoke/insert.sched.out && echo '### SUCCESS: Files Are Identical! ###' && cmp --silent tests/set_statement_parsing/_smoke/set.sched.out.tmp tests/set_statement_parsing/_smoke/set.sched.out && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/boolean_operations_parsing/_smoke/boolean.sched.out.tmp tests/boolean_operations_parsing/_smoke/boolean.sched.output && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/print/_smoke/print.sched.out.tmp tests/print/_smoke/print.sched.output && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/print_sast.sched.out.tmp tests/print_sast.sched.out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/print_sast.exe.out.tmp tests/print_sast.exe.out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'

make clean
