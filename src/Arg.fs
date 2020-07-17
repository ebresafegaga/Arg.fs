(*
    Arg.fs - Command Line Argument Parser.
 
    Created by Oghenevwogaga Ebresafe on Wed 8/6/20
    Copyright © 2020 Oghenevwogaga Ebresafe. All rights reserved.
*)

module Arg

open System.Text // for StringBuilder 

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


// used internally 

exception NotFound

type error = 
    | Unknown of string 
    | Wrong of string * string * string  (* option, actual, expected *)
    | Missing of string 
    | Message of string 

exception Stop of error 

// helpers 

// get the value of a specified option from a spec list 
let rec getValue x spec =
    match spec with
    | [] -> raise NotFound
    | (y1, y2, _) :: _ when y1 = x -> y2
    | _ :: t -> getValue x t

// split string by '='
let split s =
    let len = String.length s 
    let i = s.IndexOf '=' 
    s.Substring (0, i) , s.Substring ((i+1), (len-(i+1)))

// seprate a list of strings with sep and wrap around prefix and suffix 
let makeSymlist prefix sep suffix l =
    match l with
    | [] -> "<none>"
    | h::t -> (List.fold (fun x y -> x + sep + y) (prefix + h) t) + suffix

// does as it name says, literally 
let printSpec buf (key, spec, doc) =
    if String.length doc > 0 then
        match spec with
        | Symbol (l, _) -> Printf.bprintf buf "  %s %s%s\n" key (makeSymlist "{" "|" "}" l) doc
        | Unit | Bool | Set | String | SetString | Int 
        | SetInt | Float | SetFloat | Tuple | Symbol | Rest 
        | RestAll | Expand -> Printf.bprintf buf "  %s %s\n" key doc 


let helpAction () = raise (Stop (Unknown "-help"))

// speclist contains (key, value, doc)
let addHelp speclist =
    let rec add1 =
        try ignore (getValue "-help" speclist); []
        with NotFound ->
            ["-help", Unit helpAction, " Display this list of options"] // NOTE: ',' for tuple
    and add2 =
        try ignore (getValue "--help" speclist); []
        with NotFound ->
            ["--help", Unit helpAction, " Display this list of options"] // NOTE: ',' for tuple
    speclist @ add1 @ add2

let usageWithBuffer buf speclist errmsg =
    Printf.bprintf buf "%s\n" errmsg
    List.iter (printSpec buf) (addHelp speclist)

let usageWithString speclist errmsg =
    let b = StringBuilder (200) 
    usageWithBuffer b speclist errmsg
    b.ToString ()

let usage speclist errmsg =
    eprintf "%s" (usageWithString speclist errmsg)