open Why3
open Michelson
open Adt
open Ptree
open Number

let mk_id id_str =
  { id_str; id_ats = []; id_loc = Loc.dummy_position }

let mk_expr ?(expr_loc = Loc.dummy_position) expr_desc =
  { expr_desc; expr_loc }

let mk_pat pat_desc =
  { pat_desc; pat_loc = Loc.dummy_position }

let id_stack = mk_id "__stack__"

let id_fuel = mk_id "__fuel__"

let q_fuel = Qident id_fuel

let q_stack = Qident id_stack

let e_stack = mk_expr (Eident q_stack)

let e_fuel = mk_expr (Eident q_fuel)

let id_stack_t = mk_id "stack_t"

let stack_ty = PTtyapp (Qident id_stack_t, [])

let id_int = mk_id "int"

let int_ty = PTtyapp (Qident id_int, [])

let stack_binder = Loc.dummy_position, Some id_stack, false, Some stack_ty

let fuel_binder = Loc.dummy_position, Some id_fuel, false, Some int_ty

let empty_spec = {
  sp_pre     = [];
  sp_post    = [];
  sp_xpost   = [];
  sp_reads   = [];
  sp_writes  = [];
  sp_alias   = [];
  sp_variant = [];
  sp_checkrw = false;
  sp_diverge = false;
  sp_partial = false;
}

let stack_fuel_args = [e_stack; e_fuel]

let use_axiomatic = let axiomatic_sem = mk_id "AxiomaticSem" in
  let q_axiomatic = Qdot (Qident (mk_id "axiomatic"), axiomatic_sem) in
  Duseimport (Loc.dummy_position, false, [q_axiomatic, None])

let use_data_types = let data_types = mk_id "DataTypes" in
  let q_data_types = Qdot (Qident (mk_id "dataTypes"), data_types) in
  Duseimport (Loc.dummy_position, false, [q_data_types, None])

let data = function
  | D_int n | D_nat n -> let n = Z.to_string n in
      let n = int_literal ILitDec ~neg:false (Lexlib.remove_underscores n) in
      mk_expr (Econst (Constant.ConstInt n))
  | _ -> assert false

let rec inst = function
  | I_drop ->
      mk_expr (Eidapp (Qident (mk_id "drop"), stack_fuel_args))
  | I_drop_n n -> let n = Z.to_string n in
      let n = int_literal ILitDec ~neg:false (Lexlib.remove_underscores n) in
      let n = mk_expr (Econst (Constant.ConstInt n)) in
      mk_expr (Eidapp (Qident (mk_id "drop_n"), stack_fuel_args @ [n]))
  | I_swap ->
      mk_expr (Eidapp (Qident (mk_id "swap"), stack_fuel_args))
  | I_push ({data = Type t}, d) -> begin match t with
      | T_comparable {data = Comparable_type t} -> begin match t with
          | T_int -> let data = node d in
              let id_int = mk_id "Int" in
              let int_app = mk_expr (Eidapp (Qident id_int, [data])) in
              let id_cmp = mk_id "Comparable" in
              let cmp_app = mk_expr (Eidapp (Qident id_cmp, [int_app])) in
              let args = stack_fuel_args @ [cmp_app] in
              mk_expr (Eidapp (Qident (mk_id "push"), args))
          | T_nat -> let data = node d in
              let id_nat = mk_id "Nat" in
              let nat_app = mk_expr (Eidapp (Qident id_nat, [data])) in
              let id_cmp = mk_id "Comparable" in
              let cmp_app = mk_expr (Eidapp (Qident id_cmp, [nat_app])) in
              let args = stack_fuel_args @ [cmp_app] in
              mk_expr (Eidapp (Qident (mk_id "push"), args))
          | _ -> assert false (* TODO *) end
      | _ -> assert false (* TODO *) end
  | I_seq (i1, i2) ->
      mk_expr (Elet (id_stack, false, Expr.RKnone, node i1, node i2))
  | _ -> assert false (* TODO *)

and node : type a. a node -> expr = fun n ->
  match n.data with
  | Inst i -> inst i
  | Data d -> data d
  | Type x -> assert false (* TODO *)
  | Comparable_type _ -> assert false (* TODO *)

let program {code} =
  let code = node code in
  let kind = Expr.RKnone in
  let i_let = mk_expr (Elet (id_stack, false, kind, code, e_stack)) in
  let pat = mk_pat (Pvar id_stack_t) in
  let mask = Ity.MaskVisible in
  let pty = Some stack_ty in
  let binders = [stack_binder; fuel_binder] in
  let f_exp = Efun (binders, pty, pat, mask, empty_spec, i_let) in
  let f_exp = mk_expr f_exp in
  let decl = Dlet (mk_id "test", false, kind, f_exp) in
  [use_axiomatic; use_data_types; decl]
