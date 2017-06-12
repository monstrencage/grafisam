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
(** Graphs. *)

(** Type of graphs with three kinds of nodes.  In [(v,a,p)], [v] is
    meant to represent vertices and [p] points. [a] is a list of arcs
    linking two nodes, with optionnal labels. Nodes of kind [v] and
    [t] have labels. In practice, points are not "real" nodes of the
    graph, but rather start or end points of semantic arrows, for
    instance denoting inital/final vertices. *)
type io_graph =
  string Tools.IMap.t * (int * string option * int) list * Tools.ISet.t

(** [dot_of_graph g f] writes a description of [g] in the file [f],
    using the language [DOT]   of {{: http://www.graphviz.org/ }
    Graphviz }. *)
val dot_of_graph : io_graph -> string -> unit
