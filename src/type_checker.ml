(* open Adt
open Types


exception Typing_error

let rec data_type d =
  match get_node_data d with
  | D_int _ -> T_comparable (T_simple T_int)
  | D_nat _ -> T_comparable (T_simple T_nat)
  | D_string _ -> T_comparable (T_simple T_string)
  | D_timestamp _ -> T_comparable (T_simple T_timestamp)
  | D_signature _ -> T_signature
  | D_key _ -> T_key
  | D_key_hash _ -> T_comparable (T_simple T_key_hash)
  | D_mutez _ -> T_comparable (T_simple T_mutez)
  (* | D_contract _ -> T_contract  *)
  | D_unit -> T_unit
  | D_bool _ -> T_comparable (T_simple T_bool)
  | D_pair (l, r) -> (
      match (data_type l, data_type r) with
      | T_comparable (T_simple l_t), T_comparable r_t ->
          T_comparable (T_comparable_pair (l_t, r_t))
      | l_t, r_t -> T_pair (l_t, r_t) )
  | D_left o -> (
      match data_type o with T_or (l, _) -> l | _ -> raise Typing_error )
  | D_right o -> (
      match data_type o with T_or (_, r) -> r | _ -> raise Typing_error )
  | D_some d -> T_option (data_type d)
  (* | D_none ->  *)
  | D_list l -> assert (List.for_all (fun e -> data_type)) *)
