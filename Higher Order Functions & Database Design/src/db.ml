type person = { name: string;
                age: int;
                hobbies: string list }

type db = person list

let newDatabase : db = []

let insert (p:person) (db:db) : db = p::db

let remove (name:string) (db:db) : db = List.filter (fun p -> p.name <> name) db

type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

let rec query (condition : condition) (db : db) : person list =
  match condition with
  | True -> db
  | False -> []
  | Age f -> List.filter (fun p -> f p.age) db
  | Name f -> List.filter (fun p -> f p.name) db
  | Hobbies f -> List.filter (fun p -> f p.hobbies) db
  | And (c1, c2) -> List.filter (fun p -> List.mem p (query c1 db) && List.mem p (query c2 db)) db
  | Or (c1, c2) -> List.concat [(query c1 db); (query c2 db)]
  | Not c -> List.filter (fun p -> not (List.mem p (query c db))) db
  | If (c1, c2, c3) -> if (query c1 db) = [] then (query c3 db) else (query c2 db)

type comparator = person -> person -> int

let sort (comparator : person -> person -> int) (db:db) : person list = List.sort comparator db

let rec queryBy condition db comparator =
  let filtered_db = List.filter (fun person -> match_condition condition person) db in
  List.sort comparator filtered_db and match_condition condition person = match condition with
  | True -> true
  | False -> false
  | Age(f) -> f person.age
  | Name(f) -> f person.name
  | Hobbies(f) -> f person.hobbies
  | And(c1, c2) -> (match_condition c1 person) && (match_condition c2 person)
  | Or(c1, c2) -> (match_condition c1 person) || (match_condition c2 person)
  | Not(c) -> not (match_condition c person)
  | If(c1, c2, c3) -> if match_condition c1 person then match_condition c2 person else match_condition c3 person

let rec checkCondition (condition : condition) (person : person) : bool =
  match condition with
  | True -> true
  | False -> false
  | Age f -> f person.age
  | Name f -> f person.name
  | Hobbies f -> f person.hobbies
  | And (c1, c2) -> (checkCondition c1 person) && (checkCondition c2 person)
  | Or (c1, c2) -> (checkCondition c1 person) || (checkCondition c2 person)
  | Not c -> not (checkCondition c person)
  | If (c1, c2, c3) -> if checkCondition c1 person then checkCondition c2 person else checkCondition c3 person

let update condition db personData =
  let rec loop db' = function
    | [] -> db'
    | person::rest ->
      if checkCondition condition person then
        let updatedPerson = personData person in
        loop (updatedPerson::db') rest
      else
        loop (person::db') rest
  in loop [] db |> List.rev

let deleteAll (condition:condition) (db:db) : db =
  List.filter (fun p -> not (match_condition condition p)) db
