open Tools
   
type 'a machine = string IMap.t * (int * 'a * int) list * ISet.t * ISet.t

type 'b tape = 'b list * 'b list

exception Left
exception Right

let move_left : 'b tape -> 'b tape = function
  | (a::l,r) -> (l,a::r)
  | _ -> raise Left
let move_right : 'b tape -> 'b tape = function
  | (l,a::r) -> (a::l,r)
  | _ -> raise Right

let read : 'b tape -> 'b option = function
  | (_,a::r) -> Some a
  | _ -> None

let write b : 'b tape -> 'b tape = function
  | (l,a::r) -> (l,b::r)
  | _ -> raise Right

let insert b : 'b tape -> 'b tape = function
  | (l,r) -> (l,b::r)

type ('a,'b) transition = 'a machine -> int -> 'b tape -> int * 'b tape

let accept (_,_,_,f : 'a machine) q = ISet.mem q f

type choice = (string * int) list
              
  
