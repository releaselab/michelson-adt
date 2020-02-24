open Adt

exception Typing_error

let rec data_type = function
  | D_int _ -> T_comparable (T_simple_comparable_type T_int)
  | D_nat _ -> T_comparable (T_simple_comparable_type T_nat)
  | D_string _ -> T_comparable (T_simple_comparable_type T_string)
  | D_timestamp _ -> T_comparable (T_simple_comparable_type T_timestamp)
  | D_signature _ -> T_signature
  | D_key _ -> T_key
  | D_key_hash _ -> T_comparable (T_simple_comparable_type T_key_hash)
  | D_mutez _ -> T_comparable (T_simple_comparable_type T_mutez)
  | D_unit -> T_unit
  | D_bool _ -> T_comparable (T_simple_comparable_type T_bool)
  | D_pair (l, r) -> (
      match (data_type l, data_type r) with
      | T_comparable (T_simple_comparable_type l_t), T_comparable r_t ->
          T_comparable (T_comparable_pair ((l_t, None), (r_t, None)))
      | l_t, r_t -> T_pair ((l_t, None), (r_t, None)) )
  | D_left o -> (
      match data_type o with T_or ((l, _), _) -> l | _ -> raise Typing_error )
  | D_right o -> (
      match data_type o with T_or (_, (r, _)) -> r | _ -> raise Typing_error )
  | D_some d -> T_option (data_type d, None)
  | D_none t -> T_option t
  | D_list (t, _) -> T_list t
  | D_address _ -> T_comparable (T_simple_comparable_type T_address)
  | D_set (t, _) -> T_set t
  | D_map ((t_k, t_v), _) -> T_map (t_k, t_v)
  | D_bytes _ -> T_comparable (T_simple_comparable_type T_bytes)
