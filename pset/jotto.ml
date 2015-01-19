
let compare s1 s2 : int = failwith "TODO"

module type Oracle = sig

  type 'a t

  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val guess  : string -> int t
  val random : int    -> int t

end

module Word : Oracle = struct

  type 'a t = string -> 'a

  let (>>=)  v f word = f v
  let return x   word = x

  let guess guess word = compare guess word
end

module 

