(* Copyright (C) 2016 Paul Brunet
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.*)

exception UnsupportedOperation

module Int = struct type t = int let compare = compare end
module ISet = Set.Make(Int)
module IMap = Map.Make(Int)
module ISSet = Set.Make(ISet)

module IPair = struct
  type t = int * int
  let compare = Pervasives.compare
end
module IPSet = Set.Make(IPair)
		      
module SSet = Set.Make(String)
module SMap = Map.Make(String)

module SPair = struct
  type t = string * string
  let compare = Pervasives.compare
end
module SPSet = Set.Make(SPair)
	

let string_of_ipair (a,b) = 
  Printf.sprintf "(%d,%d)" a b

let string_of_spair (a,b) = 
  Printf.sprintf "(%s,%s)" a b

let string_of_iset s =
  ISet.elements s
  |> List.map string_of_int
  |> String.concat "; "
  |> Printf.sprintf "{%s}"

let string_of_sset s =
  SSet.elements s
  |> String.concat "; "
  |> Printf.sprintf "{%s}"

let string_of_ipset s =
  IPSet.elements s
  |> List.map string_of_ipair
  |> String.concat "; "
  |> Printf.sprintf "{%s}"

let string_of_spset s =
  SPSet.elements s
  |> List.map string_of_spair
  |> String.concat "; "
  |> Printf.sprintf "{%s}"

let string_of_imap f s =
  IMap.bindings s
  |> List.map (fun (i,x) -> Printf.sprintf "(%d→%s)" i (f x))
  |> String.concat "; "
  |> Printf.sprintf "{%s}"

let string_of_smap f s =
  SMap.bindings s
  |> List.map (fun (i,x) -> Printf.sprintf "(%s→%s)" i (f x))
  |> String.concat "; "
  |> Printf.sprintf "{%s}"

type crel = ISet.t IMap.t

let get_succ p (rel : crel) =
  try IMap.find p rel
  with Not_found -> ISet.empty
		   
let add_pair (p,q) (rel : crel) : crel =
  IMap.add p (ISet.add q (get_succ p rel)) rel

module StrInt = struct
  type t = string * int
  let compare = Pervasives.compare
end

module SISet = Set.Make (StrInt)

let string_of_siset s =
  SISet.fold (fun (s,i) -> Printf.sprintf "(%s,%d)%s" s i) s ""
  |> Printf.sprintf "{%s}"

module TrSet = Set.Make(struct
  type t = int * string * int
  let compare = compare
end)


module Descriptor : UnionFind.Desc with type accumulator = unit = struct
    type descriptor = int
    let default = 0
    type accumulator = unit
    let union d1 d2 acc = (d1+d2,())
  end

type ('a,'b) case = Left of 'a | Right of 'b

module SSCase = 
struct 
  type t = (SSet.t,SSet.t) case
  let compare a b = 
    match a,b with
      | Left a,Left b | Right a,Right b -> SSet.compare a b
      | Left a,Right b -> -1
      | Right a,Left b -> 1
  let equal a b = (compare a b = 0)
  module Map = Map.Make(struct type t = (SSet.t,SSet.t) case
                               let compare : t -> t -> int = compare
                        end)
end
                  
module SSCaseUF =
  struct
    include UnionFind.Make(SSCase)(Descriptor)
    let union a b s = union a b s () |> fst
  end
