open Why3
open Ptree
open Michelson
open Adt
open Why3_michelson_trans

let () = ignore (id)

let () =
  ignore (Qident {id_str = "ola"; id_ats = []; id_loc = Loc.dummy_position})

let () = ignore (t_int)

let () = ignore (T_int)
