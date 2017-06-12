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
(** General tool box for the project. *)

exception UnsupportedOperation

(** {3 Sets. } *)
(** Sets of integers. *)
module ISet : Set.S with type elt = int

(** Sets of sets of integers. *)
module ISSet : Set.S with type elt = ISet.t

(** Sets of pairs of integers. *)
module IPSet : Set.S with type elt = int * int

(** Sets of strings. *)
module SSet : Set.S with type elt = string

(** Sets of pairs of strings. *)
module SPSet : Set.S with type elt = string * string

(** Sets of pairs of a string and an integer. *)
module SISet : Set.S with type elt = string * int

(** Sets of triples of an integer, a string and another integer. *)
module TrSet : Set.S with type elt = int * string * int

(** {3 Maps. } *)
(** Maps indexed with integers. *)
module IMap : Map.S with type key = int
     
(** Maps indexed with strings. *)
module SMap : Map.S with type key = string

(** {3 Conversions to string. } *)    
val string_of_ipair : int * int -> string
val string_of_spair : string * string -> string
val string_of_iset : ISet.t -> string
val string_of_sset : SSet.t -> string
val string_of_ipset : IPSet.t -> string
val string_of_spset : SPSet.t -> string
val string_of_siset : SISet.t -> string
val string_of_imap : ('a -> string) -> 'a IMap.t -> string
val string_of_smap : ('a -> string) -> 'a SMap.t -> string

(** {3 Binary relations over integers. } *)
type crel = ISet.t IMap.t
val get_succ : int -> crel -> ISet.t
val add_pair : int * int -> crel -> crel


(** {3 Union find. } *)

(** Disjoint union of types. *)
type ('a,'b) case = Left of 'a | Right of 'b

(** Simple module for union find structure, where the elements are
    sets of strings of two incomparable kinds. *)
module SSCaseUF :
sig
  type item = (SSet.t,SSet.t) case
  type state
  val initial : state
  val union : item -> item -> state -> state
  val equivalent : item -> item -> state -> bool
end
