open Base
open Adt

type stack = Failed | S of typ Common.Stack.t [@@deriving eq, sexp_of]

let rec is_comparable t =
  match t.value with
  | T_unit | T_key | T_signature | T_chain_id | T_int | T_nat | T_string
  | T_bytes | T_mutez | T_bool | T_key_hash | T_timestamp | T_address ->
      true
  | T_option t -> is_comparable t
  | T_or (t_1, t_2) | T_pair (t_1, t_2) ->
      is_comparable t_1 && is_comparable t_2
  | T_operation | T_list _ | T_set _ | T_contract _ | T_lambda _ | T_map _
  | T_big_map _ ->
      false

let typecheck p =
  let exception Type_error of (inst * stack) in
  let t t = make_node t in
  let entrypoints =
    match p.param.value with
    | T_or _ ->
        let rec et entrypoints t =
          match t.value with
          | T_or (t_1, t_2) -> et (et entrypoints t_1) t_2
          | _ -> (
              match
                List.find_map t.annots ~f:(function
                  | A_field s -> Some s
                  | _ -> None)
              with
              | None -> entrypoints
              | Some a -> (a, t) :: entrypoints)
        in
        et [] p.param
    | _ -> []
  in
  let rec t_aux stack inst =
    let type_error = Type_error (inst, stack) in
    match stack with
    | Failed -> Failed
    | S s -> (
        match (inst.value, s) with
        | I_noop, _ -> S s
        | I_abs, { value = T_int; _ } :: s' -> S (t T_nat :: s')
        | I_failwith, _ -> Failed
        | I_exec, a :: { value = T_lambda (a', b); _ } :: s' when equal_typ a a'
          ->
            S (b :: s')
        | ( I_apply,
            a
            :: { value = T_lambda ({ value = T_pair (a', b); _ }, c); _ } :: s'
          )
          when equal_typ a a' ->
            S (t (T_lambda (b, c)) :: s')
        | I_drop, _ :: s' -> S s'
        | I_dup, t :: s' -> S (t :: t :: s')
        | I_swap, t_1 :: t_2 :: s' -> S (t_2 :: t_1 :: s')
        | I_unit, s -> S (t T_unit :: s)
        | I_eq, { value = T_int; _ } :: s' -> S (t T_bool :: s')
        | I_neq, { value = T_int; _ } :: s' -> S (t T_bool :: s')
        | I_lt, { value = T_int; _ } :: s' -> S (t T_bool :: s')
        | I_gt, { value = T_int; _ } :: s' -> S (t T_bool :: s')
        | I_le, { value = T_int; _ } :: s' -> S (t T_bool :: s')
        | I_ge, { value = T_int; _ } :: s' -> S (t T_bool :: s')
        | I_or, { value = T_bool; _ } :: { value = T_bool; _ } :: s' ->
            S (t T_bool :: s')
        | I_or, { value = T_nat; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_nat :: s')
        | I_and, { value = T_bool; _ } :: { value = T_bool; _ } :: s' ->
            S (t T_bool :: s')
        | I_and, { value = T_nat; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_nat :: s')
        | I_and, { value = T_int; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_nat :: s')
        | I_xor, { value = T_bool; _ } :: { value = T_bool; _ } :: s' ->
            S (t T_bool :: s')
        | I_xor, { value = T_nat; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_nat :: s')
        | I_not, { value = T_bool; _ } :: s' -> S (t T_bool :: s')
        | I_not, { value = T_nat; _ } :: s' -> S (t T_int :: s')
        | I_not, { value = T_int; _ } :: s' -> S (t T_int :: s')
        | I_neg, { value = T_nat; _ } :: s' -> S (t T_int :: s')
        | I_neg, { value = T_int; _ } :: s' -> S (t T_int :: s')
        | I_isnat, { value = T_int; _ } :: s' ->
            S (t (T_option (t T_nat)) :: s')
        | I_int, { value = T_nat; _ } :: s' -> S (t T_int :: s')
        | I_add, { value = T_int; _ } :: { value = T_int; _ } :: s' ->
            S (t T_int :: s')
        | I_add, { value = T_int; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_int :: s')
        | I_add, { value = T_nat; _ } :: { value = T_int; _ } :: s' ->
            S (t T_int :: s')
        | I_add, { value = T_nat; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_nat :: s')
        | I_add, { value = T_timestamp; _ } :: { value = T_int; _ } :: s' ->
            S (t T_timestamp :: s')
        | I_add, { value = T_int; _ } :: { value = T_timestamp; _ } :: s' ->
            S (t T_timestamp :: s')
        | I_add, { value = T_mutez; _ } :: { value = T_mutez; _ } :: s' ->
            S (t T_mutez :: s')
        | ( I_sub,
            { value = T_int | T_nat; _ } :: { value = T_int | T_nat; _ } :: s' )
          ->
            S (t T_int :: s')
        | I_sub, { value = T_timestamp; _ } :: { value = T_int; _ } :: s' ->
            S (t T_timestamp :: s')
        | I_sub, { value = T_timestamp; _ } :: { value = T_timestamp; _ } :: s'
          ->
            S (t T_int :: s')
        | I_sub, { value = T_mutez; _ } :: { value = T_mutez; _ } :: s' ->
            S (t T_mutez :: s')
        | I_mul, { value = T_int | T_nat; _ } :: { value = T_int; _ } :: s' ->
            S (t T_int :: s')
        | I_mul, { value = T_nat; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_nat :: s')
        | I_mul, { value = T_int; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_int :: s')
        | I_mul, { value = T_mutez; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_mutez :: s')
        | I_mul, { value = T_nat; _ } :: { value = T_mutez; _ } :: s' ->
            S (t T_mutez :: s')
        | I_ediv, { value = T_int | T_nat; _ } :: { value = T_int; _ } :: s' ->
            S (t (T_option (t (T_pair (t T_int, t T_nat)))) :: s')
        | I_ediv, { value = T_int; _ } :: { value = T_nat; _ } :: s' ->
            S (t (T_option (t (T_pair (t T_int, t T_nat)))) :: s')
        | I_ediv, { value = T_nat; _ } :: { value = T_nat; _ } :: s' ->
            S (t (T_option (t (T_pair (t T_nat, t T_nat)))) :: s')
        | I_ediv, { value = T_mutez; _ } :: { value = T_nat; _ } :: s' ->
            S (t (T_option (t (T_pair (t T_mutez, t T_mutez)))) :: s')
        | I_ediv, { value = T_mutez; _ } :: { value = T_mutez; _ } :: s' ->
            S (t (T_option (t (T_pair (t T_nat, t T_mutez)))) :: s')
        | I_lsl, { value = T_nat; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_nat :: s')
        | I_lsr, { value = T_nat; _ } :: { value = T_nat; _ } :: s' ->
            S (t T_nat :: s')
        | I_compare, t_1 :: t_2 :: s'
          when equal_typ t_1 t_2 && is_comparable t_1 ->
            S (t T_int :: s')
        | ( I_compare,
            { value = T_pair (t_1, t_2); _ }
            :: { value = T_pair (t_1', t_2'); _ } :: s' ) ->
            if equal_typ t_1 t_1' && equal_typ t_2 t_2' then S (t T_int :: s')
            else raise type_error
        | I_concat, { value = T_string; _ } :: { value = T_string; _ } :: s' ->
            S (t T_string :: s')
        | I_concat, { value = T_bytes; _ } :: { value = T_bytes; _ } :: s' ->
            S (t T_bytes :: s')
        | ( I_concat,
            { value = T_list { value = (T_string | T_bytes) as t'; _ }; _ }
            :: s' ) ->
            S (t t' :: s')
        | ( I_size,
            { value = T_string | T_set _ | T_map _ | T_list _ | T_bytes; _ }
            :: s' ) ->
            S (t T_nat :: s')
        | ( I_slice,
            { value = T_nat; _ }
            :: { value = T_nat; _ }
               :: ({ value = T_string | T_bytes; _ } as t') :: s' ) ->
            S (t (T_option t') :: s')
        | I_pair, t_1 :: t_2 :: s' -> S (t (T_pair (t_1, t_2)) :: s')
        | I_car, { value = T_pair (t, _); _ } :: s'
        | I_cdr, { value = T_pair (_, t); _ } :: s' ->
            S (t :: s')
        | I_empty_set t', s -> S (t (T_set t') :: s)
        | I_mem, t' :: { value = T_set t''; _ } :: s'
        | I_mem, t' :: { value = T_map (t'', _); _ } :: s'
        | I_mem, t' :: { value = T_big_map (t'', _); _ } :: s'
          when equal_typ t' t'' ->
            S (t T_bool :: s')
        | ( I_update,
            t_1
            :: { value = T_bool; _ } :: ({ value = T_set t_1'; _ } as t') :: s'
          )
          when equal_typ t_1 t_1' ->
            S (t' :: s')
        | ( I_update,
            t_1
            :: { value = T_option t_2; _ }
               :: ({ value = T_map (t_1', t_2') | T_big_map (t_1', t_2'); _ } as
                  t')
                  :: s' )
          when equal_typ t_1 t_1' && equal_typ t_2 t_2' ->
            S (t' :: s')
        | I_iter b, { value = T_list t_1 | T_set t_1; _ } :: s' ->
            let s'' = t_aux (S (t_1 :: s')) b in
            if equal_stack (S s') s'' then S s' else raise type_error
        | I_iter b, { value = T_map (t_1, t_2); _ } :: s' ->
            let s'' = t_aux (S (t (T_pair (t_1, t_2)) :: s')) b in
            if equal_stack (S s') s'' then S s' else raise type_error
        | I_empty_map (t_1, t_2), s' -> S (t (T_map (t_1, t_2)) :: s')
        | ( I_get,
            t_1
            :: { value = T_map (t_1', t_2) | T_big_map (t_1', t_2); _ } :: s' )
          ->
            if equal_typ t_1 t_1' then S (t (T_option t_2) :: s')
            else raise type_error
        | I_map b, { value = T_list t'; _ } :: s' -> (
            match t_aux (S (t' :: s')) b with
            | S (b :: s'') when equal_stack (S s') (S s'') ->
                S (t (T_list b) :: s')
            | S _ | Failed -> raise type_error)
        | I_map b, { value = T_map (t_1, t_2); _ } :: a' -> (
            match t_aux (S (t (T_pair (t_1, t_2)) :: a')) b with
            | S (b :: a'') when equal_stack (S a') (S a'') ->
                S (t (T_map (t_1, b)) :: a')
            | S _ | Failed -> raise type_error)
        | I_some, t' :: s' -> S (t (T_option t') :: s')
        | I_cons, t' :: { value = T_list t''; _ } :: s' when equal_typ t' t'' ->
            S (t (T_list t') :: s')
        | ( I_transfer_tokens,
            p :: { value = T_mutez; _ } :: { value = T_contract p'; _ } :: s' )
          ->
            if equal_typ p p' then S (t T_operation :: s') else raise type_error
        | ( I_set_delegate,
            { value = T_option { value = T_key_hash; _ }; _ } :: s' ) ->
            S (t T_operation :: s')
        | I_balance, s' -> S (t T_mutez :: s')
        | I_address, { value = T_contract _; _ } :: s' -> S (t T_address :: s')
        | I_source, s' -> S (t T_address :: s')
        | I_sender, s' -> S (t T_address :: s')
        | I_self, s' -> (
            match
              List.find_map inst.annots ~f:(function
                | A_field a -> Some a
                | _ -> None)
            with
            | Some a -> (
                match List.Assoc.find entrypoints ~equal:String.equal a with
                | Some t' -> S (t (T_contract t') :: s')
                | None -> S (t (T_contract p.param) :: s'))
            | None -> S (t (T_contract p.param) :: s'))
        | I_amount, s' -> S (t T_mutez :: s')
        | I_implicit_account, { value = T_key_hash; _ } :: s' ->
            S (t (T_contract (t T_unit)) :: s')
        | I_now, s' -> S (t T_timestamp :: s')
        | I_chain_id, s' -> S (t T_chain_id :: s')
        | I_pack, _ :: s' -> S (t T_bytes :: s')
        | I_hash_key, { value = T_key; _ } :: s' -> S (t T_key_hash :: s')
        | I_blake2b, { value = T_bytes; _ } :: s' -> S (t T_bytes :: s')
        | I_sha256, { value = T_bytes; _ } :: s' -> S (t T_bytes :: s')
        | I_sha512, { value = T_bytes; _ } :: s' -> S (t T_bytes :: s')
        | ( I_check_signature,
            { value = T_key; _ }
            :: { value = T_signature; _ } :: { value = T_bytes; _ } :: s' ) ->
            S (t T_bool :: s')
        | I_unpair, { value = T_pair (t_1, t_2); _ } :: s' ->
            S (t_1 :: t_2 :: s')
        | I_rename, s' -> S s'
        | I_seq i_l, s' -> List.fold_left i_l ~init:(S s') ~f:t_aux
        | I_if (i_t, i_f), { value = T_bool; _ } :: s' -> (
            let s_t = t_aux (S s') i_t in
            let s_f = t_aux (S s') i_f in
            match (s_t, s_f) with
            | Failed, s | s, Failed -> s
            | S _, S _ when equal_stack s_t s_f -> s_t
            | _ -> raise type_error)
        | I_loop b, { value = T_bool; _ } :: s' ->
            if equal_stack (t_aux (S s') b) (S s) then S s'
            else raise type_error
        | I_loop_left b, ({ value = T_or (t_1, t_2); _ } as t') :: s' ->
            if equal_stack (t_aux (S (t_1 :: s')) b) (S (t' :: s')) then
              S (t_2 :: s')
            else raise type_error
        | I_dip c, t' :: s' -> (
            match t_aux (S s') c with
            | Failed -> raise type_error
            | S s -> S (t' :: s))
        | I_dip_n (n, c), s -> (
            let n = Bigint.to_int_exn n in
            match Common.Stack.pop_n s n with
            | None -> raise type_error
            | Some (s_h, s_t) -> (
                match t_aux (S s_t) c with
                | Failed -> raise type_error
                | S s' -> S (s_h @ s')))
        | I_drop, s -> (
            match Common.Stack.drop s with
            | None -> raise type_error
            | Some s -> S s)
        | I_drop_n n, s -> (
            let n = Bigint.to_int_exn n in
            match Common.Stack.drop_n s n with
            | None -> raise type_error
            | Some s -> S s)
        | I_dig n, s -> (
            let n = Bigint.to_int_exn n in
            match Common.Stack.dig s n with
            | None -> raise type_error
            | Some s -> S s)
        | I_dug n, s -> (
            let n = Bigint.to_int_exn n in
            match Common.Stack.dug s n with
            | None -> raise type_error
            | Some s -> S s)
        | I_push (t', _), s -> S (t' :: s)
        | I_lambda (t_1, t_2, _), s -> S (t (T_lambda (t_1, t_2)) :: s)
        | I_empty_big_map (t_1, t_2), s -> S (t (T_big_map (t_1, t_2)) :: s)
        | I_none t', s -> S (t (T_option t') :: s)
        | I_if_none (i_t, i_f), { value = T_option t'; _ } :: s -> (
            let s_t = t_aux (S s) i_t in
            let s_f = t_aux (S (t' :: s)) i_f in
            match (s_t, s_f) with
            | Failed, s | s, Failed -> s
            | S _, S _ -> if equal_stack s_t s_f then s_t else raise type_error)
        | I_left t_2, t_1 :: s' -> S (t (T_or (t_1, t_2)) :: s')
        | I_right t_1, t_2 :: s' -> S (t (T_or (t_1, t_2)) :: s')
        | I_if_left (i_t, i_f), { value = T_or (t_1, t_2); _ } :: s' -> (
            let s_t = t_aux (S (t_1 :: s')) i_t in
            let s_f = t_aux (S (t_2 :: s')) i_f in
            match (s_t, s_f) with
            | Failed, s | s, Failed -> s
            | S _, S _ when equal_stack s_t s_f -> s_t
            | _ -> raise type_error)
        | I_nil t', s -> S (t (T_list t') :: s)
        | I_if_cons (i_t, i_f), ({ value = T_list t'; _ } as t'') :: s' -> (
            let s_t = t_aux (S (t' :: t'' :: s')) i_t in
            let s_f = t_aux (S s') i_f in
            match (s_t, s_f) with
            | Failed, s | s, Failed -> s
            | S _, S _ -> if equal_stack s_t s_f then s_t else raise type_error)
        | ( I_create_contract p,
            { value = T_option { value = T_key_hash; _ }; _ }
            :: { value = T_mutez; _ } :: g :: s' ) ->
            if equal_typ p.storage g then S (t T_operation :: t T_address :: s')
            else raise type_error
        | I_contract t', { value = T_address; _ } :: s' ->
            S (t (T_option (t (T_contract t'))) :: s')
        | I_unpack t', { value = T_bytes; _ } :: s' -> S (t (T_option t') :: s')
        | I_cast _, s' -> S s'
        | _ -> raise type_error)
  in
  try
    match t_aux (S [ t (T_pair (p.param, p.storage)) ]) p.code with
    | Failed -> true
    | S [ { value = T_pair (t_1, t_2); _ } ]
      when equal_typ t_1 (t (T_list (t T_operation))) && equal_typ t_2 p.storage
      ->
        true
    | S _ as s ->
        Core_kernel.Debug.ams [%here] "Invalid end stack" s sexp_of_stack;
        false
  with Type_error (i, s) ->
    Core_kernel.Debug.ams [%here] "Type error" (i, s) (fun (i, s) ->
        Sexp.List [ sexp_of_inst i; sexp_of_stack s ]);
    false
