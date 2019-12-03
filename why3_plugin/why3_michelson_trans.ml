open Why3
open Michelson
open Adt
open Ptree
open Number

let mk_id id_str =
  { id_str; id_ats = []; id_loc = Loc.dummy_position }

let mk_expr expr_desc =
  { expr_desc; expr_loc = Loc.dummy_position }

let q_stack = Qident (mk_id "__stack__")

let e_stack = mk_expr (Eident q_stack)

let inst = function
  | I_drop     -> Eidapp (Qident (mk_id "drop"), [e_stack])
  | I_drop_n n -> let n = Z.to_string n in
      let n = int_literal ILitDec ~neg:false (Lexlib.remove_underscores n) in
      let n = mk_expr (Econst (Constant.ConstInt n)) in
      Eidapp (Qident (mk_id "drop_n"), [e_stack; n])
  | I_swap -> Eidapp (Qident (mk_id "swap"), [e_stack])
  | _ -> assert false (* TODO *)
