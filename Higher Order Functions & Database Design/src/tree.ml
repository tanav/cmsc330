type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree = match tree with
  | Leaf -> init
  | Node (a, b, c) -> f (tree_fold f init a) b (tree_fold f init c)

let map tree f =
  tree_fold (fun a b c -> Node (a, f b, c)) Leaf tree

let mirror tree =
  tree_fold (fun a b c -> Node (c, b, a)) Leaf tree

let in_order tree =
  let folder a b c =
    a @ [b] @ c in
  tree_fold folder [] tree

let pre_order tree =
  let folder a b c =
    b :: (a @ c) in
  tree_fold folder [] tree

let compose tree = failwith "unimplemented"

let depth tree =
  let f a _ c = 1 + max a c in
  tree_fold f 0 tree

  let trim tree n =
    let depth = ref 0 in
    let f left_subtree value right_subtree =
      let trimmed_left_subtree = tree_fold f Leaf left_subtree in
      let trimmed_right_subtree = tree_fold f Leaf right_subtree in
      if !depth < n then begin
        depth := !depth + 1;
        Node (trimmed_left_subtree, value, trimmed_right_subtree)
      end
      else
        Leaf
    in
    tree_fold f Leaf tree

let rec split lst v = match lst with
  | [] -> ([], [])
  | x::xs when x = v -> ([], xs)
  | x::xs ->
      let (before, after) = split xs v in
      (x::before, after)

let rec from_pre_in pre in_ord =
  match pre with
  | [] -> Leaf
  | h::t ->
      let (in_ord_left, in_ord_right) = split in_ord h in
      let pre_left = List.filter (fun x -> List.mem x in_ord_left) t in
      let pre_right = List.filter (fun x -> List.mem x in_ord_right) t in
      let left_subtree = from_pre_in pre_left in_ord_left in
      let right_subtree = from_pre_in pre_right in_ord_right in
      Node(left_subtree, h, right_subtree)