(*
    Arg.fs - Command Line Argument Parser.
 
    Created by Oghenevwogaga Ebresafe on Wed 8/6/20
    Copyright © 2020 Oghenevwogaga Ebresafe. All rights reserved.
*)

module Arg

type Spec = 
    | Unit of (unit -> unit)
    | Bool of (bool -> unit)
    | Set of bool ref 
    | String of (string -> unit)
    | SetString of string ref
    | Int of (int -> unit)
    | SetInt of int ref 
    | Float of (float -> unit)
    | SetFloat of float ref
    | Tuple of Spec list 
    | Symbol of string list * (string -> unit)
    | Rest of (string -> unit)
    | RestAll of (string list -> unit)
    | Expand of (string -> string array)
