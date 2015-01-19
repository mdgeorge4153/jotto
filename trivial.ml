let choices = ["blue"; "green"; "orange"; "yellow"; "pink"; "brown"]

let color () =
  let num = Random.int 7 in
  if num = 6 then "roll again" else
  let color = List.nth choices num in
  let wedge = if Random.int 6 = 0 then " wedge" else "" in
  color ^ wedge

let roll () =
  color (), color ()

let flip () =
  if Random.bool () then "heads" else "tails"

