(* Code generation: translate takes a semantically
 * checked AST and produces LLVM IR *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(*type state = (L.llvalue StringMap.t) * L.llvalue * L.llbuilder*)
type state = {namespace: L.llvalue StringMap.t;
              func: L.llvalue;
              b: L.llbuilder}

(* translate : Sast.sprogram -> Llvm.module *)
let translate sprogram =
  let context = L.global_context () in
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module  = L.create_module     context "Schedch"
  and i1_t        = L.i1_type           context
  and i8_t        = L.i8_type           context
  and i32_t       = L.i32_type          context
  and float_t     = L.double_type       context
  and void_t      = L.void_type         context in

  let i8_tp       = L.pointer_type i8_t in
  (* A Schedule struct has 5 fields:
   * 1. Name: Points to a string that is its name.
   * 2. Start Date: Points to a string that is its start date (for now).
   * 3. n_items: 8-bit int that gives the number of items in the Schedule.
   * 4. items_arr: Pointer to start of items array.
   * 5. arr_size: 8-bit int that gives the size of the array.
   *)
  let sched_t     = L.struct_type context
    [| i8_tp; i8_tp ; i8_t ; (L.pointer_type i8_tp) ; i8_t |] in

  (* Return the LLVM type for a Schedch type *)
  (* TODO: Add types here.
  let ltype_of_typ = function
          ...
  in
  *)
  let ltype_of_typ = function
    A.Int   -> i32_t
  | A.Bool  -> i1_t
  (* | A.Float -> float_t *)
  | A.Void  -> void_t
in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (** setup main() where all the code will go **)
  (* ftype is the full llvm function signature *)
  let main_t = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_t the_module in
  let main_builder = L.builder_at_end context (L.entry_block main_func) in
  let str_format_str = L.build_global_stringptr "%s\n" "fmt" main_builder in

  let the_state:state = {namespace=StringMap.empty;
                         func=main_func;
                         b=main_builder} in


  (* Construct code for an expression; return its value. *)
  let rec sxpr builder = function
      (A.String, SStrLit s) -> L.build_global_stringptr s "" builder
      (* A String literal should result in a defined constant being
       * added to the LLVM module and a pointer to that constant. *)
    | (A.CId, SId s) -> L.build_global_stringptr s "" builder
    | (A.Bool, SBoolLit b)  -> L.const_int i1_t (if b then 1 else 0)
    | (A.Int, SBinop (e1, op, e2)) ->
     	  let e1' = sxpr builder e1
     	  and e2' = sxpr builder e2 in
     	  (match op with
     	    A.Add     -> L.build_add
     	  | A.Sub     -> L.build_sub
     	  | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
     	  | A.And     -> L.build_and
     	  | A.Or      -> L.build_or
     	  | A.Equal   -> L.build_icmp L.Icmp.Eq
     	  | A.Neq     -> L.build_icmp L.Icmp.Ne
     	  (* | A.Less    -> L.build_icmp L.Icmp.Slt
     	  | A.Leq     -> L.build_icmp L.Icmp.Sle
     	  | A.Greater -> L.build_icmp L.Icmp.Sgt
     	  | A.Geq     -> L.build_icmp L.Icmp.Sge *)
        ) e1' e2' "tmp" builder
    | (A.Bool, SBinop (e1, op, e2)) ->
         	  let e1' = sxpr builder e1
         	  and e2' = sxpr builder e2 in
         	  (match op with
         	  | A.And     -> L.build_and
         	  | A.Or      -> L.build_or
         	  | A.Equal   -> L.build_icmp L.Icmp.Eq
         	  | A.Neq     -> L.build_icmp L.Icmp.Ne
         	  (* | A.Less    -> L.build_icmp L.Icmp.Slt
         	  | A.Leq     -> L.build_icmp L.Icmp.Sle
         	  | A.Greater -> L.build_icmp L.Icmp.Sgt
         	  | A.Geq     -> L.build_icmp L.Icmp.Sge *)
       	    ) e1' e2' "tmp" builder
      | (A.Void, SCall("print", [sx])) ->
          let sx' = sxpr builder sx in
          L.build_call printf_func [| str_format_str ; sx' |] "printf" builder
      | _ -> raise (Failure "sxpr codegen type not implemented yet.")
  in

  let rec ssched_spec builder = function
    SNamed(kind, sx_opt, sx) ->
      (* Create a global string constant for the name. *)
      let name_ptr = sxpr builder sx in
      (* Allocate space on the stack for the name. *)
      let sched = L.build_alloca sched_t "" builder in
      (* Store value of name_ptr to the name field of this Schedule. *)
      let sname_ptr = L.build_struct_gep sched 0 "" builder in
      let store = L.build_store name_ptr sname_ptr builder in
      (* Store value of start date info (if given) *)
      let date_ptr =
        (match sx_opt with
          Some(dt_sx) -> sxpr builder dt_sx
        | _ -> sxpr builder (A.String, SStrLit "None"))
      in
      let sdate_ptr = L.build_struct_gep sched 1 "" builder in
      L.build_store date_ptr sdate_ptr builder
  | _ -> raise (Failure "ssched_spec case not implemented yet.")
  in

  let rec scstmt builder = function
    SSchedule(spec) -> ignore (ssched_spec builder spec); builder
  | _ -> raise (Failure "screate_stmt case not implemented yet.")
  in

  let rec sstmt builder = function
      SExpr(sx) -> ignore (sxpr builder sx); builder
    | SCS(cs) -> ignore (scstmt builder cs); builder
    | _ -> raise (Failure "sstmt codegen type not implemented yet.")
  in

  List.iter (fun stmt -> ignore(sstmt the_state.b stmt)) sprogram;
  ignore(L.build_ret (L.const_int i32_t 0) the_state.b);
  the_module
