make clean
make


./schedch.native -a tests/create.sched > tests/create.sched.out.tmp
./schedch.native -a tests/insert_statement_parsing/_smoke/insert.sched > tests/insert_statement_parsing/_smoke/insert.sched.out.tmp
./schedch.native -a tests/set_statement_parsing/_smoke/set.sched > tests/set_statement_parsing/_smoke/set.sched.out.tmp
./schedch.native -a tests/boolean_operations_parsing/_smoke/boolean.sched > tests/boolean_operations_parsing/_smoke/boolean.sched.out.tmp
./schedch.native -a tests/print/_smoke/print.sched > tests/print/_smoke/print.sched.out.tmp
./schedch.native -a tests/func_definition/_smoke/func.sched > tests/func_definition/_smoke/func.sched.out.tmp
./schedch.native -a tests/int_arithmetic_parsing/_smoke/intarith.sched > tests/int_arithmetic_parsing/_smoke/intarith.sched.out.tmp
./schedch.native -a tests/drop_statement_parsing/_smoke/drop.sched > tests/drop_statement_parsing/_smoke/drop.sched.out.tmp
./schedch.native -a tests/float_operations_parsing/_smoke/float.sched > tests/float_operations_parsing/_smoke/float.sched.out.tmp
./schedch.native -a tests/if_else_parsing/_smoke/ifelse.sched > tests/if_else_parsing/_smoke/ifelse.sched.output 

./schedch.native -s tests/print_sast.sched > tests/print_sast.sched.out.tmp
./schedch.native -c tests/print_sast.sched > tests/print_sast.ll && llc tests/print_sast.ll > tests/print_sast.s &&\
	cc -o tests/print_sast.exe tests/print_sast.s && ./tests/print_sast.exe > ./tests/print_sast.exe.out.tmp

./schedch.native -s tests/bool_op_semantics\&codegen/_smoke/binop.sched > tests/bool_op_semantics\&codegen/_smoke/binop.sched.s_out.tmp
./schedch.native -s tests/float_op_semantics_codegen/_smoke/binop.sched > tests/float_op_semantics_codegen/_smoke/binop.sched.s_out.tmp
./schedch.native -s tests/int_op_semantics_codegen/_smoke/binop.sched > tests/int_op_semantics_codegen/_smoke/binop.sched.s_out.tmp
./schedch.native -l tests/bool_op_semantics\&codegen/_smoke/binop.sched > tests/bool_op_semantics\&codegen/_smoke/binop.sched.l_out.tmp
./schedch.native -s tests/float_op_semantics_codegen/_smoke/unop.sched > tests/float_op_semantics_codegen/_smoke/unop.sched.s_out.tmp
./schedch.native -s tests/int_op_semantics_codegen/_smoke/unop.sched > tests/int_op_semantics_codegen/_smoke/unop.sched.s_out.tmp
./schedch.native -l tests/float_op_semantics_codegen/_smoke/binop.sched > tests/float_op_semantics_codegen/_smoke/binop.sched.l_out.tmp
./schedch.native -l tests/int_op_semantics_codegen/_smoke/binop.sched > tests/int_op_semantics_codegen/_smoke/binop.sched.l_out.tmp
./schedch.native -l tests/float_op_semantics_codegen/_smoke/unop.sched > tests/float_op_semantics_codegen/_smoke/unop.sched.l_out.tmp
./schedch.native -l tests/int_op_semantics_codegen/_smoke/unop.sched > tests/int_op_semantics_codegen/_smoke/unop.sched.l_out.tmp

cmp --silent tests/create.sched.output tests/create.sched.out.tmp && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/insert_statement_parsing/_smoke/insert.sched.out.tmp tests/insert_statement_parsing/_smoke/insert.sched.out && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/set_statement_parsing/_smoke/set.sched.out.tmp tests/set_statement_parsing/_smoke/set.sched.out && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/func_definition/_smoke/func.sched.out.tmp tests/func_definition/_smoke/func.sched.out && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/boolean_operations_parsing/_smoke/boolean.sched.out.tmp tests/boolean_operations_parsing/_smoke/boolean.sched.output && echo '### SUCCESS: Files Are Identical! ###' &&
cmp --silent tests/print/_smoke/print.sched.out.tmp tests/print/_smoke/print.sched.output && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/float_operations_parsing/_smoke/float.sched.out.tmp tests/float_operations_parsing/_smoke/float.sched.output && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/int_arithmetic_parsing/_smoke/intarith.sched.out.tmp tests/int_arithmetic_parsing/_smoke/intarith.sched.output && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/print_sast.sched.out.tmp tests/print_sast.sched.out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/print_sast.exe.out.tmp tests/print_sast.exe.out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/bool_op_semantics\&codegen/_smoke/binop.sched.s_out.tmp tests/bool_op_semantics\&codegen/_smoke/binop.sched.s_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/bool_op_semantics\&codegen/_smoke/binop.sched.l_out.tmp tests/bool_op_semantics\&codegen/_smoke/binop.sched.l_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/int_op_semantics_codegen/_smoke/binop.sched.s_out.tmp tests/int_op_semantics_codegen/_smoke/binop.sched.s_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/float_op_semantics_codegen/_smoke/binop.sched.s_out.tmp tests/float_op_semantics_codegen/_smoke/binop.sched.s_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/int_op_semantics_codegen/_smoke/unop.sched.s_out.tmp tests/int_op_semantics_codegen/_smoke/unop.sched.s_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/float_op_semantics_codegen/_smoke/unop.sched.s_out.tmp tests/float_op_semantics_codegen/_smoke/unop.sched.s_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/int_op_semantics_codegen/_smoke/binop.sched.l_out.tmp tests/int_op_semantics_codegen/_smoke/binop.sched.l_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/float_op_semantics_codegen/_smoke/binop.sched.l_out.tmp tests/float_op_semantics_codegen/_smoke/binop.sched.l_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/int_op_semantics_codegen/_smoke/unop.sched.l_out.tmp tests/int_op_semantics_codegen/_smoke/unop.sched.l_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/float_op_semantics_codegen/_smoke/unop.sched.l_out.tmp tests/float_op_semantics_codegen/_smoke/unop.sched.l_out && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'
cmp --silent tests/if_else_parsing/_smoke/ifelse.sched.out.tmp tests/if_else_parsing/_smoke/ifelse.sched.output && echo '### SUCCESS: Files Are Identical! ###' || echo '### WARNING: Files Are Different! ###'


make clean
