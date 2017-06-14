open Tools

type tape = string list * string list

type stack = string list

type generic_state =
  {
    state : int;
    tapes : tape list;
    stacks : stack list;
    registers : int list
  }
             
exception Left
exception Right
exception Empty
exception Stuck
exception Finished
        
let move_left : tape -> tape = function
  | (a::l,r) -> (l,a::r)
  | _ -> raise Left
let move_right : tape -> tape = function
  | (l,a::r) -> (a::l,r)
  | _ -> raise Right

let read : tape -> string = function
  | (_,a::r) -> a
  | _ -> raise Right

let write b : tape -> tape = function
  | (l,a::r) -> (l,b::r)
  | _ -> raise Right

let insert b : tape -> tape = function
  | (l,r) -> (l,b::r)

let push a (c : stack) : stack = a::c
let pop : stack -> stack = function
  | _::c -> c
  | _ -> raise Empty
let top : stack -> string = function
  | a::_ -> a
  | _ -> raise Empty

type choice_fun = (string * int) list -> int

module type Det =
  sig
    type t
    type state
    val print_state : state -> generic_state
    val nodes : t -> string IMap.t
    val edges : t -> (int * string * int) list
    val accept : t -> state -> bool
    val init : t -> string list -> state
    val step : t -> state -> state
    val initial_state : t -> int
    val final_states : t -> int list
  end

let det_run (type a) (type b)
            (module A : Det with type t=a and type state = b)
            (m : A.t) (w : string list) =
  let rec aux q =
    try aux (A.step m q)
    with Finished -> (A.accept m q,A.print_state q)
       | Stuck -> (false,A.print_state q)
  in aux (A.init m w)

module type NDet =
  sig
    type t
    type state
    val print_state : state -> generic_state
    val nodes : t -> string IMap.t
    val edges : t -> (int * string * int) list
    val accept : t -> state -> bool
    val init : t -> string list -> state list
    val step : t -> state -> state list
    val initial_states : t -> int list
    val final_states : t -> int list
  end

 
let ndet_run (type a) (type b)
             (module A : NDet with type t=a and type state = b)
            (m : A.t) (w : string list) =
  let rec aux acc = function
    | [] -> acc
    | lst ->
       let acc',lst' =
         List.fold_left
           (fun (acc',lst') q ->
                   try acc',(A.step m q)@lst'
                   with Finished -> (A.accept m q,A.print_state q)::acc',lst'
                       | Stuck -> (false,A.print_state q)::acc',lst')
           (acc,[]) lst
       in aux acc' lst'
  in aux [] (A.init m w)

module DFA : Det = struct
  type t = string IMap.t * (int SMap.t) IMap.t * int * ISet.t
  type state = int * tape
  let print_state (q,t) =
    { state = q;
      tapes = [t];
      stacks = [];
      registers = [];
    }
  let nodes (n,_,_,_) = n
  let edges (_,e,_,_ : t) =
    bind
      (fun (p,t) ->
        List.map (fun (a,q) -> (p,a,q))
                 (SMap.bindings t))
      (IMap.bindings e)
  let accept (_,_,_,f) = function
    | _,(_,_::_) -> false
    | q,(_,[]) -> ISet.mem q f 
  let init (_,_,q0,_) w = (q0,([],w))
  let step (_,e,_,_) = function
    | _,(_,[]) -> raise Finished
    | q,(p,a::w) -> try (SMap.find a (IMap.find q e),(a::p,w))
                    with Not_found -> raise Stuck
  let initial_state (_,_,q0,_) = q0
  let final_states (_,_,_,f) = ISet.elements f
end
                 
module NFA : NDet = struct
  type t = string IMap.t * (ISet.t SMap.t) IMap.t * ISet.t * ISet.t
  type state = int * tape
  let print_state (q,t) =
    { state = q;
      tapes = [t];
      stacks = [];
      registers = [];
    }
  let nodes (n,_,_,_) = n
  let edges (_,e,_,_ : t) =
    bind
      (fun (p,t) ->
        bind (fun (a,s) ->
            List.map (fun q -> (p,a,q))
                     (ISet.elements s))
             (SMap.bindings t))
      (IMap.bindings e)
  let accept (_,_,_,f) = function
    | _,(_,_::_) -> false
    | q,(_,[]) -> ISet.mem q f 
  let init (_,_,q0,_) w = List.map (fun q -> q,([],w)) (ISet.elements q0)
  let step (_,e,_,_) = function
    | _,(_,[]) -> raise Finished
    | q,(p,a::w) ->
       try
         ISet.elements (SMap.find a (IMap.find q e))
         |> List.map (fun q -> q,(a::p,w))
       with Not_found -> raise Stuck
  let initial_states (_,_,q0,_) = ISet.elements q0
  let final_states (_,_,_,f) = ISet.elements f
         
end
let to_tape : string list -> tape = function
  | [] -> ([],[])
  | a::w -> (w,[a])
          
module DT : Det = struct
  type t = string IMap.t * ((string list *int) SMap.t) IMap.t * int * ISet.t
  type state = int * tape * string list
  let print_state (q,i,o) =
    { state = q;
      tapes = [i;to_tape o];
      stacks = [];
      registers = [];
    }
  let nodes (n,_,_,_) = n
  let edges (_,e,_,_ : t) =
    bind
      (fun (p,t) ->
        List.map (fun (a,(b,q)) -> (p,(a^" / "^(String.concat " " b)),q))
                 (SMap.bindings t))
      (IMap.bindings e)
  let accept (_,_,_,f) = function
    | _,(_,_::_),_ -> false
    | q,(_,[]),_ -> ISet.mem q f 
  let init (_,_,q0,_) w = (q0,([],w),[])
  let step (_,e,_,_) = function
    | _,(_,[]),_ -> raise Finished
    | p,(past,a::w),o ->
       try
         let (o',q) = SMap.find a (IMap.find p e) in
         (q,(a::past,w),(List.rev o')@o)
       with Not_found -> raise Stuck
  let initial_state (_,_,q0,_) = q0
  let final_states (_,_,_,f) = ISet.elements f
         
end
module NT : NDet = struct
  type t = string IMap.t * (((string list * int) list) SMap.t) IMap.t * ISet.t * ISet.t
  type state = int * tape * string list
  let print_state (q,i,o) =
    { state = q;
      tapes = [i;to_tape o];
      stacks = [];
      registers = [];
    }
  let nodes (n,_,_,_) = n
  let edges (_,e,_,_ : t) =
    bind
      (fun (p,t) ->
        bind (fun (a,o) ->
            List.map (fun (b,q) ->(p,(a^" / "^(String.concat " " b)),q)) o)
                  
                 (SMap.bindings t))
      (IMap.bindings e)
  let accept (_,_,_,f) = function
    | _,(_,_::_),_ -> false
    | q,(_,[]),_ -> ISet.mem q f 
  let init (_,_,q0,_) w = List.map (fun q -> q,([],w),[]) (ISet.elements q0)
                        
  let step (_,e,_,_) = function
    | _,(_,[]),_ -> raise Finished
    | p,(past,a::w),o ->
       try
         List.map (fun (o',q) -> q,(a::past,w),(List.rev o')@o)
             (SMap.find a (IMap.find p e))
       with Not_found -> raise Stuck
  let initial_states (_,_,q0,_) = ISet.elements q0
  let final_states (_,_,_,f) = ISet.elements f
         
end
module DPA : Det = struct
  type t = string IMap.t * (((string list * string list * int) list) SMap.t) IMap.t * int * ISet.t
  type state = int * tape * stack
  let print_state (q,t,s) =
    { state = q;
      tapes = [t];
      stacks = [s];
      registers = [];
    }
  let nodes (n,_,_,_) = n
  let edges (_,e,_,_ : t) =
    bind
      (fun (p,t) ->
        bind
          (fun (a,q) ->
            List.map
              (fun (i,o,q) ->
                (p,a^" , "^(String.concat " " i)^" / "^(String.concat " " o),q))
           q)
          (SMap.bindings t))
      (IMap.bindings e)
  let accept (_,_,_,f) = function
    | _,(_,_::_),_ | _,_,_::_ -> false
    | q,(_,[]),[] -> ISet.mem q f 
  let init (_,_,q0,_) w = (q0,([],w),[])

  let rec match_stack = function
    | ([],l) -> Some l
    | (_,[]) -> None
    | (a::_,b::_) when a <> b -> None
    | (_::l,_::m) -> match_stack (l,m)
                   
  let step (_,e,_,_) = function
    | _,(_,[]),_ -> raise Finished
    | q,(p,a::w),s ->
       try
         List.fold_left
           (function
            | Some w -> fun _ -> Some w
            | None ->
               fun (i,o,q) ->
               match match_stack (i,s) with
               | None -> None
               | Some s' -> Some (q,(a::p,w),o@s'))
           None
           (SMap.find a (IMap.find q e))
         |> (function None -> raise Stuck
                    | Some q -> q)
       with Not_found -> raise Stuck

  let initial_state (_,_,q0,_) = q0
  let final_states (_,_,_,f) = ISet.elements f         
end
                 
module PA : NDet = struct
  type t = string IMap.t * (((string list * string list * int) list) SMap.t) IMap.t * ISet.t * ISet.t
  type state = int * tape * stack
  let print_state (q,t,s) =
    { state = q;
      tapes = [t];
      stacks = [s];
      registers = [];
    }
  let nodes (n,_,_,_) = n
  let edges (_,e,_,_ : t) =
    bind
      (fun (p,t) ->
        bind
          (fun (a,q) ->
            List.map
              (fun (i,o,q) ->
                (p,a^" , "^(String.concat " " i)^" / "^(String.concat " " o),q))
           q)
          (SMap.bindings t))
      (IMap.bindings e)
  let accept (_,_,_,f) = function
    | _,(_,_::_),_ | _,_,_::_ -> false
    | q,(_,[]),[] -> ISet.mem q f 
  let init (_,_,q0,_ : t) w : state list = List.map (fun q0 -> q0,([],w),[]) (ISet.elements q0)

  let rec match_stack = function
    | ([],l) -> Some l
    | (_,[]) -> None
    | (a::_,b::_) when a <> b -> None
    | (_::l,_::m) -> match_stack (l,m)
                   
  let step (_,e,_,_) = function
    | _,(_,[]),_ -> raise Finished
    | q,(p,a::w),s ->
       try
         List.fold_left
           (fun acc (i,o,q) ->
             match match_stack (i,s) with
             | None -> acc
             | Some s' -> (q,(a::p,w),o@s')::acc)
           []
           (SMap.find a (IMap.find q e))
       with Not_found -> raise Stuck
  let initial_states (_,_,q0,_) = ISet.elements q0
  let final_states (_,_,_,f) = ISet.elements f
         
end
   
