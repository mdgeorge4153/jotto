(** minimum by sum of squares: reset / terse / ester ... *)
(** minimum by max bucket: tiers *)

(** for jotto dict: squares: raved; minmax: wader *)

type stats = int list
type dict  = (string * stats) list
type comparison_result = Gt | Eq | Lt


let letters = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
               'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']

let rec count x l = match l with
  | []               -> 0
  | y::tl when x = y -> 1 + count x tl
  | _::tl            -> count x tl

let compare l1 l2 =
  List.fold_left (+) 0 (List.map2 min l1 l2)

let list_of_string s = [s.[0]; s.[1]; s.[2]; s.[3]; s.[4]]

let convert (s:string) : stats =
  let l = list_of_string (String.lowercase s) in
  List.map (fun x -> count x l) letters

let words filename =
  let ch = open_in filename in
  let rec loop acc =
    let line = try Some (input_line ch) with End_of_file -> None in
    match line with
      | Some l when String.length l = 5 -> loop (l::acc)
      | Some _ -> loop acc
      | None   -> acc
  in loop []

let dict : dict =
  List.rev_map (fun s -> s,convert s) (words "words")

let split (words : dict) guess =
  let split = Array.make 6 0 in
  List.iter (fun (s,w) ->
    let count = compare w guess in
    split.(count) <- split.(count) + 1
  ) words;
  split

let filter_guess (words : dict) (guess : stats) n : dict =
  List.filter (fun (s,w) -> compare w guess = n) words

let best_answer (words : dict) (guess : stats) =
  let split = Array.mapi (fun i x -> (x,i)) (split words guess) in
  let (n,i) = Array.fold_left max (-1,-1) split in
  i

let mine   = ref dict
let theirs = ref dict
let replies = ref []

let reset () =
  mine    := dict;
  theirs  := dict;
  replies := [];
  ()

let rec take n l = match n,l with
  | 0, _  -> []
  | _, [] -> []
  | n, h::tl -> h::take (pred n) tl

let answer (word : string) =
  let word = convert word in
  let n = best_answer !mine word in
  mine := filter_guess !mine word n;
  List.iter (fun (s,_) -> print_endline s) (take 10 !mine);
  n

(** returns the goodness of a guess.  Smaller is better *)
let minmax (dict : dict) (word : stats) : int =
  let split = split dict word in
  Array.fold_left (fun a n -> max a n) 0 split

let squares (dict : dict) (word : stats) : int =
  let split = split !theirs word in
  Array.fold_left (fun a n -> a + n*n) 0 split

let cmp a b = if a < b then Lt else if a = b then Eq else Gt

let guesses score (possible : dict) =
  let _,r = List.fold_left (fun (best,ws) (s,w) ->
    Printf.printf "%s%!" s;
    let score = score possible w in
    Printf.printf "\b\b\b\b\b%!";
    match cmp score best with
      | Lt -> Printf.printf "%s: %i\n%!" s score; (score,[s,w])
      | Eq -> Printf.printf "%s\n%!" s;           (score, (s,w)::ws)
      | Gt -> (best,ws)
  ) (max_int, []) dict
  in List.map fst r

let reply s n =
  replies := (s,n)::!replies;
  let w = convert s in
  theirs := List.filter (fun (s,w') -> compare w w' = n) !theirs;
  List.map fst !theirs

(** given a dictionary, return the necessary characters *)
let musthave (dict : dict) =
  let max  = List.map (fun _ -> 6) letters in
  let mins = List.fold_left (fun acc (_,ls) -> List.map2 min acc ls) max dict in
  let lets = List.combine letters mins in
  List.filter (fun (c,n) -> n > 0) lets

let canthave (dict : dict) =
  let min   = List.map (fun _ -> 0) letters in
  let maxes = List.fold_left (fun acc (_,ls) -> List.map2 max acc ls) min dict in
  let lets  = List.combine letters maxes in
  List.map fst (List.filter (fun (c,n) -> n = 0) lets)

(*
module type JottoMonad = sig
  type 'a t

  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a t

  val guess : string -> int t
end

module StandardJotto = struct
  type 'a t = 'a * string

*)

(*
squares game

raved
 | 0 -> snits
   | 0 ->
   | 1 ->
   | 2 -> solos
     | 0 ->
     | 1 -> pucks
     | 2 ->
     | 3 ->
     | 4 ->
     | 5 ->
   | 3 ->
   | 4 ->
   | 5 ->
 | 1 ->
 | 2 ->
 | 3 ->
 | 4 ->
 | 5 -> raved
*)
