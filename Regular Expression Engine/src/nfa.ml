open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let matching_transitions =
    List.filter
      (fun (q, symbol, _) ->
        match symbol with
        | Some sym -> s = Some sym
        | None -> s = None
      )
      (List.fold_left
        (fun acc q ->
          List.append
            acc
            (List.filter
              (fun (state, _, _) -> state = q)
              nfa.delta)
        )
        []
        qs
      )
  in
  let compare_q q1 q2 = 
    if q1 = q2 then 0
    else if q1 < q2 then -1
    else 1
  in
  List.sort_uniq compare_q (List.map (fun (_, _, q) -> q) matching_transitions)

let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let epsilon_moves = List.concat (List.map (fun q -> move nfa [q] None) qs) in
  let next_states = List.fold_left
    (fun acc q ->
      if List.mem q acc then acc
      else e_closure nfa (q::acc)
    )
    qs
    epsilon_moves
  in
  next_states

let accept (nfa: ('q, char) nfa_t) (s: string) : bool =
  let rec accept_helper (qs: 'q list) (chars: char list) : bool =
    match chars with
    | [] ->
      List.exists (fun q -> List.mem q nfa.fs) (e_closure nfa qs)
    | c::rest ->
      let next_states = move nfa qs (Some c) in
      let epsilon_states = e_closure nfa next_states in
      accept_helper epsilon_states rest
  in
  accept_helper [nfa.q0] (explode s)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q, 's) nfa_t) (qs: 'q list) : 'q list list =
  let init = e_closure nfa qs in List.fold_left (fun acc x -> acc @ [e_closure nfa (move nfa init (Some x))]) [] nfa.sigma
  
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun x ele -> x @ [qs, Some ele, e_closure nfa (move nfa qs (Some ele))]) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (List.length (intersection qs nfa.fs) > 0) then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q, 's) nfa_t) : ('q list, 's) nfa_t =
  let init_state = e_closure nfa [nfa.q0] in
  let rec helper (unmarked: 'q list list) (marked: 'q list list) (trans: ('q list, 's) transition list) : ('q list, 's) nfa_t =
    match unmarked with
    | [] -> {sigma = nfa.sigma; qs = marked; q0 = init_state; fs = List.filter (fun qs -> List.exists (fun q -> List.mem q nfa.fs) qs) marked; delta = trans}
    | qs :: rest ->
      let new_trans = new_trans nfa qs in
      let marked' = qs :: marked in
      let unmarked' = List.fold_left (fun acc x -> if List.mem x acc || List.mem x marked' then acc else x :: acc) rest (new_states nfa qs) in
      let trans' = trans @ new_trans in
      helper unmarked' marked' trans'
  in
  helper [init_state] [] []