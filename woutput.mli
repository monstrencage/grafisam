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
(** Drawing graphs using {{: http://visjs.org/ } vis.js }. *)

(** [vis_of_graph g id] draws the graph [g] inside the html element of
    id [id] using the external javascript library {{:
    http://visjs.org/ } vis.js }. An intermediate javascript global
    variable [dataGraphVis] is used. *)
val vis_of_graph : Graph.io_graph -> string -> unit
