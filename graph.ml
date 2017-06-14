(* Copyright (C) 2016 Paul Brunet
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301 USA.*)
open Tools

type io_graph =
  string IMap.t * (int * string option * int) list * ISet.t

let dot_of_graph (m,a,i : io_graph) file =
  let printarc ch (i,l,j) =
    match l with
    | None -> Printf.fprintf ch "%d -> %d;\n" i j
    | Some l ->
       Printf.fprintf ch "%d -> %d [label = \"%s\"];\n" i j l
  in
  let printplace ch i s = 
    Printf.fprintf ch "%d [label=\"%s\";shape=circle];\n" i s
  in
  let printpoint ch i = 
    Printf.fprintf ch "%d [shape=point];\n" i
  in
  let ch = open_out file in
  Printf.fprintf ch "digraph structs {\n";
  ISet.iter (printpoint ch) i;
  IMap.iter (printplace ch) m;
  List.iter (printarc ch) a;
  Printf.fprintf ch " }";
  close_out ch

open Machine
   
let det_to_graph
      (type a) (type b)
      (module A : Det with type t=a and type state = b)
      (m : A.t) : io_graph =
  let v = A.nodes m in
  let n0 = IMap.max_binding v |> fst in
  let edges0 = List.map (fun (i,l,j) -> i,Some l,j) (A.edges m) in
  let p1,n1,edges1 =
    (ISet.singleton (n0+1),n0+2,(n0+1,None,A.initial_state m)::edges0)
  in
  let p2,_,edges2 =
    List.fold_left
      (fun (p,nb,e) qf ->
        (ISet.add nb p,nb+1,(qf,None,nb)::e))
      (p1,n1+1,edges1) (A.final_states m)
  in (v,edges2,p2)

  
let ndet_to_graph
      (type a) (type b)
      (module A : NDet with type t=a and type state = b)
      (m : A.t) : io_graph =
  let v = A.nodes m in
  let n0 = IMap.max_binding v |> fst in
  let edges0 = List.map (fun (i,l,j) -> i,Some l,j) (A.edges m) in
  let p1,n1,edges1 =
    List.fold_left
      (fun (p,nb,e) q0 ->
        (ISet.add nb p,nb+1,(nb,None,q0)::e))
      (ISet.empty,n0+1,edges0)
      (A.initial_states m)
  in
  let p2,_,edges2 =
    List.fold_left
      (fun (p,nb,e) qf ->
        (ISet.add nb p,nb+1,(qf,None,nb)::e))
      (p1,n1+1,edges1) (A.final_states m)
  in (v,edges2,p2)

  
