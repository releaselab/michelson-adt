open Base

type 'a t = 'a list [@@deriving eq, ord, sexp]

let push s a = a :: s

let%test _ = equal Int.equal (push [ 2 ] 1) [ 1; 2 ]

let pop = function [] -> None | h :: t -> Some (h, t)

let%test _ = Option.is_none (pop [])

let%test _ =
  let h, s = Option.value_exn (pop [ 1; 2 ]) in
  h = 1 && equal Int.equal s [ 2 ]

let pop_n s n = if List.length s < n then None else Some (List.split_n s n)

let%test _ = Option.is_none (pop_n [ 1; 2 ] 3)

let%test _ =
  let h, t = Option.value_exn (pop_n [ 1; 2; 3; 4 ] 2) in
  equal Int.equal h [ 1; 2 ] && equal Int.equal t [ 3; 4 ]

let peek = function [] -> None | h :: _ -> Some h

let%test _ = Option.is_none (peek [])

let%test _ = Option.value_exn (peek [ 1; 2 ]) = 1

let drop = function [] -> None | _ :: t -> Some t

let%test _ = Option.is_none (drop [])

let%test _ = equal Int.equal (Option.value_exn (drop [ 1; 2 ])) [ 2 ]

let drop_n s n = if List.length s < n then None else Some (List.drop s n)

let%test _ = Option.is_none (drop_n [] 1)

let%test _ = equal Int.equal (Option.value_exn (drop_n [ 1; 2; 3 ] 2)) [ 3 ]

let swap = function h :: h' :: s -> Some (h' :: h :: s) | [ _ ] | [] -> None

let%test _ =
  equal Int.equal
    (Option.value_exn (swap [ 1; 2; 3; 4; 5; 6; 7; 8 ]))
    [ 2; 1; 3; 4; 5; 6; 7; 8 ]

let%test _ = Option.is_none (swap [])

let%test _ = Option.is_none (swap [ 1 ])

let dig s n =
  if n = 0 then Some s
  else if List.length s < n + 1 then None
  else
    let s_h, s_t = List.split_n s n in
    match s_t with h :: s_t -> Some (h :: (s_h @ s_t)) | _ -> assert false

let%test _ =
  equal Int.equal
    (Option.value_exn (dig [ 1; 2; 3; 4; 5; 6; 7; 8 ] 0))
    [ 1; 2; 3; 4; 5; 6; 7; 8 ]

let%test _ =
  equal Int.equal
    (Option.value_exn (dig [ 1; 2; 3; 4; 5; 6; 7; 8 ] 1))
    [ 2; 1; 3; 4; 5; 6; 7; 8 ]

let%test _ =
  equal Int.equal
    (Option.value_exn (dig [ 1; 2; 3; 4; 5; 6; 7; 8 ] 5))
    [ 6; 1; 2; 3; 4; 5; 7; 8 ]

let%test _ = Option.is_none (dig [ 1; 2; 3; 4; 5; 6; 7; 8 ] 8)

let dug s n =
  if n = 0 then Some s
  else if List.length s < n + 1 then None
  else
    match List.split_n s (n + 1) with
    | h :: s_h, s_t -> Some (s_h @ h :: s_t)
    | _ -> assert false

let%test _ =
  equal Int.equal
    (Option.value_exn (dug [ 1; 2; 3; 4; 5; 6; 7; 8 ] 0))
    [ 1; 2; 3; 4; 5; 6; 7; 8 ]

let%test _ =
  equal Int.equal
    (Option.value_exn (dug [ 1; 2; 3; 4; 5; 6; 7; 8 ] 1))
    [ 2; 1; 3; 4; 5; 6; 7; 8 ]

let%test _ =
  equal Int.equal
    (Option.value_exn (dug [ 1; 2; 3; 4; 5; 6; 7; 8 ] 5))
    [ 2; 3; 4; 5; 6; 1; 7; 8 ]

let%test _ = Option.is_none (dug [ 1; 2; 3; 4; 5; 6; 7; 8 ] 8)

let dup = function [] -> None | h :: t -> Some (h :: h :: t)

let%test _ = Option.is_none (dup [])

let%test _ = equal Int.equal (Option.value_exn (dup [ 1; 2 ])) [ 1; 1; 2 ]
