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
open Tools
open Graph

module Html = Dom_html

let blue = Js.Unsafe.obj [|
  ( "border" , Js.Unsafe.inject ( Js.string "#2B7CE9"));
  ( "background" , Js.Unsafe.inject ( Js.string "#97C2FC"));
  ( "highlight" , Js.Unsafe.inject ( Js.Unsafe.obj [|
    ( "border" , Js.Unsafe.inject ( Js.string "#2B7CE9"));
    ( "background" , Js.Unsafe.inject ( Js.string "#D2E5FF"))
  |]))
|]

let green = Js.Unsafe.obj [| 
  ("border"  , Js.Unsafe.inject ( Js.string "#41A906"));
  ("background"  , Js.Unsafe.inject ( Js.string "#7BE141"));
  ("highlight"  , Js.Unsafe.inject ( Js.Unsafe.obj [|
    ("border"  , Js.Unsafe.inject ( Js.string "#41A906"));
    ("background"  , Js.Unsafe.inject ( Js.string "#A1EC76"))
  |]))
|]

let purple= Js.Unsafe.obj [|
  ("border", Js.Unsafe.inject ( Js.string "#7C29F0"));
  ("background", Js.Unsafe.inject ( Js.string "#AD85E4"));
  ("highlight", Js.Unsafe.inject ( Js.Unsafe.obj [|
    ("border", Js.Unsafe.inject ( Js.string "#7C29F0"));
    ("background", Js.Unsafe.inject ( Js.string "#D3BDF0"))
  |]))
|]


let red= Js.Unsafe.obj [|
  ("border", Js.Unsafe.inject ( Js.string "#8B0000"));
  ("background", Js.Unsafe.inject ( Js.string "#DC143C"));
  ("highlight", Js.Unsafe.inject ( Js.Unsafe.obj [|
    ("border", Js.Unsafe.inject ( Js.string "#8B0000"));
    ("background", Js.Unsafe.inject ( Js.string "#FF4864"))
  |]))
|]

let node_trans i s = 
  Js.Unsafe.obj 
    [| 
      ("id", Js.Unsafe.inject ( Js.string (Printf.sprintf "%d" i))) ;
      ("label", Js.Unsafe.inject ( Js.string (Printf.sprintf "   %s   "  s)));
      ("shape", Js.Unsafe.inject ( Js.string "box" ));
      ("color", Js.Unsafe.inject ( green )); 
      ("fontSize", Js.Unsafe.inject ( Js.number_of_float 25. ))
    |]

let node_place i s = 
  Js.Unsafe.obj 
    [| 
      ("id", Js.Unsafe.inject ( Js.string (Printf.sprintf "%d" i))) ;
      ("label", Js.Unsafe.inject ( Js.string (Printf.sprintf "%s" s)));
      ("shape", Js.Unsafe.inject ( Js.string "circle" ));
      ("color", Js.Unsafe.inject ( blue )); 
      ("fontSize", Js.Unsafe.inject ( Js.number_of_float 27. ))
    |]


let node_point i = 
  Js.Unsafe.obj 
    [| 
      ("id", Js.Unsafe.inject ( Js.string (Printf.sprintf "%d" i))) ;
      ("label", Js.Unsafe.inject ( Js.string (Printf.sprintf "%d" i)));
      ("shape", Js.Unsafe.inject ( Js.string "dot" ));
      ("hidden", Js.Unsafe.inject (Js.bool true))
    |]

let arc i j = function
  | None -> 
     Js.Unsafe.obj 
       [|("from", Js.Unsafe.inject (Js.string (string_of_int i))); 
         ("to", Js.Unsafe.inject (Js.string (string_of_int j))); 
         ("color", Js.Unsafe.inject ( blue )); 
         ("style", Js.Unsafe.inject (Js.string "arrow-center"))|]
  | Some l -> 
     Js.Unsafe.obj 
       [|("from", Js.Unsafe.inject (Js.string (string_of_int i))); 
         ("to", Js.Unsafe.inject (Js.string (string_of_int j))); 
         ("color", Js.Unsafe.inject ( green )); 
         ("label", Js.Unsafe.inject ( Js.string l));
         ("style", Js.Unsafe.inject (Js.string "arrow-center"))|]
  
let vis_of_graph (m,a,i : io_graph) id =
  let nodes =
    IMap.fold (fun i s acc -> (node_place i s)::acc) m []
    |> ISet.fold (fun i acc -> (node_point i)::acc) i
  in
  let edges =
    List.fold_left (fun acc (i,l,j) -> (arc i j l)::acc) [] a
  in
  let conv a =   
    Js.Unsafe.inject 
      (Js.array (Array.of_list (List.map Js.Unsafe.inject a)))
  in
  let grph = (Js.Unsafe.obj [|"nodes",conv nodes;"edges",conv edges|])
  in
  let cmd = 
    Printf.sprintf 
      "new 
         vis.Network(document.getElementById('%s'),
                     dataGraphVis,
                     {edges:{arrows:'to'}})" 
      id
  in
  Js.Unsafe.global##dataGraphVis <- grph;
  Js.Unsafe.eval_string cmd

