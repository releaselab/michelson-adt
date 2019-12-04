open Why3
open Michelson
open Adt
open Ptree
open Number

let mk_id id_str =
  { id_str; id_ats = []; id_loc = Loc.dummy_position }

let mk_expr expr_desc =
  { expr_desc; expr_loc = Loc.dummy_position }

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

let inst = function
  | I_drop     -> Eidapp (Qident (mk_id "drop"), stack_fuel_args)
  | I_drop_n n -> let n = Z.to_string n in
      let n = int_literal ILitDec ~neg:false (Lexlib.remove_underscores n) in
      let n = mk_expr (Econst (Constant.ConstInt n)) in
      Eidapp (Qident (mk_id "drop_n"), stack_fuel_args @ [n])
  | I_swap -> Eidapp (Qident (mk_id "swap"), stack_fuel_args)
  | _ -> assert false (* TODO *)

let node : type a. a node -> decl = fun n ->
  match n.data with
  | Inst i -> let i_app = mk_expr (inst i) in
      let kind = Expr.RKnone in
      let i_let = mk_expr (Elet (id_stack, false, kind, i_app, e_stack)) in
      let pat = mk_pat (Pvar id_stack_t) in
      let mask = Ity.MaskVisible in
      let pty = Some stack_ty in
      let binders = [stack_binder; fuel_binder] in
      let f_exp = Efun (binders, pty, pat, mask, empty_spec, i_let) in
      let f_exp = mk_expr f_exp in
      Dlet (mk_id "test", false, kind, f_exp)
  | Data _ -> assert false (* TODO *)
  | Type x -> assert false (* TODO *)
  | Comparable_type _ -> assert false (* TODO *)

let program {code} =
  [use_axiomatic; node code]
