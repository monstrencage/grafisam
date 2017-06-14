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

type choice_fun = (string * int) list -> int

module type Det =
  sig
    type t
    type state
    val print_state : state -> generic_state
    val nodes : t -> string Tools.IMap.t
    val edges : t -> (int * string * int) list
    val accept : t -> state -> bool
    val init : t -> string list -> state
    val step : t -> state -> state
    val initial_state : t -> int
    val final_states : t -> int list
  end

val det_run : (module Det with type t = 'a and type state = 'b) ->
              'a -> string list -> bool * generic_state
  
module type NDet =
  sig
    type t
    type state
    val print_state : state -> generic_state
    val nodes : t -> string Tools.IMap.t
    val edges : t -> (int * string * int) list
    val accept : t -> state -> bool
    val init : t -> string list -> state list
    val step : t -> state -> state list
    val initial_states : t -> int list
    val final_states : t -> int list
  end

val ndet_run : (module NDet with type t = 'a and type state = 'b) ->
               'a -> string list -> (bool * generic_state) list
  
module DFA : Det
module DT : Det
module DPA : Det
module NFA : NDet
module NT : NDet
module PA : NDet
