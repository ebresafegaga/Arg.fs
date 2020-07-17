(*
    Arg.fs - Command Line Argument Parser.
 
    Created by Oghenevwogaga Ebresafe on Wed 8/6/20
    Copyright © 2020 Oghenevwogaga Ebresafe. All rights reserved.
*)

module Arg

open System // for Boolean, why doesn't F# just allow bool. , int. , string. for .NET statics?
open System.Text // for StringBuilder 

// should be exported 
type Key = string 
type Doc = string 
type UsageMsg = string
type AnonFun = string -> unit 

type Spec = 
    | Unit of (unit -> unit)
    | Bool of (bool -> unit)
    | Set of bool ref 
    | Clear of bool ref
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

exception Bad of string
exception Help of string

exception NotFound
exception InvalidArgument of string // TODO: remove later and use .NET's own 

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
        | Unit | Bool | Set | Clear | String | SetString | Int 
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

// wrappers with 'a option
let boolOpt (x : string) =
  try Some (Boolean.Parse x) with _ -> None

let intOpt (x : string) =
  try Some (int x) with _ -> None

let floatOpt x =
  try Some (float x) with _ -> None

// not good to make a global mutable? 
let current = ref 0

//
// meaty stuff 
//

open Printf // for bprintf and family 

// read 'expand' as 'expand?'
let parseAndExpandArgvDynamicAux expand current argv speclist anonfun errmsg =

    let initpos = !current // not global 'current'; local

    (* convert an internal error to a Bad/Help exception
       *or* add the program name as a prefix and the usage message as a suffix
       to an user-raised Bad exception.
    *)
    let convertError error =
        let b = StringBuilder (200) 
        let progname = if initpos < (Array.length !argv) then (!argv).[initpos] else "(?)"
        
        match error with 
        | Unknown "-help" -> ()
        | Unknown "--help" -> ()
        | Unknown s ->
          bprintf b "%s: unknown option '%s'.\n" progname s
        | Missing s ->
          bprintf b "%s: option '%s' needs an argument.\n" progname s
        | Wrong (opt, arg, expected) ->
          bprintf b "%s: wrong argument '%s'; option '%s' expects %s.\n"
                  progname arg opt expected
        | Message s -> (* user error message *)
          bprintf b "%s: %s.\n" progname s

        usageWithBuffer b !speclist errmsg
        if error = Unknown "-help" || error = Unknown "--help"
        then Help (b.ToString ())
        else Bad (b.ToString ())

    incr current // move from name of exe to real CLI arguments 

    while !current < (Array.length !argv) do
        try 
            let s = (!argv).[!current]
            if String.length s >= 1 && s.[0] = '-' then
                let action, follow = 
                    try getValue s !speclist, None
                    with NotFound ->
                        try
                            let keyword, arg = split s 
                            getValue keyword !speclist, Some arg
                        with _ -> raise (Stop (Unknown s))
                let noArg () =
                    match follow with
                    | None -> ()
                    | Some arg -> raise (Stop (Wrong (s, arg, "no argument")))
                let getArg () =
                    match follow with
                    | None ->
                        if !current + 1 < (Array.length !argv) then (!argv).[!current + 1]
                        else raise (Stop (Missing s))
                    | Some arg -> arg
                (* 
                    follow matches (Some arg) when options and arguments are seperated by an '='
                    otherwise, it's in the form -option arg in which case we advance the pointer
                    current by one to consume -option, then current will point to arg, which will
                    also be incremented at the end of the iteration.
                *)
                let consumeArg () =
                    match follow with
                    | None   -> incr current 
                    | Some _ -> () 
                let rec handleAction = function 
                    | Unit f -> noArg (); f ()
                    | Bool f -> 
                        let arg = getArg ()
                        match boolOpt arg with 
                        | None -> raise (Stop (Wrong (s, arg, "a boolean")))
                        | Some s -> f s
                        consumeArg ()
                    | Set r -> noArg (); r := true
                    | Clear r -> noArg (); r := false
                    | String f -> 
                        let arg = getArg ()
                        f arg
                        consumeArg ()
                    | Symbol (symb, f) -> 
                        let arg = getArg ()
                        // symb is a list of possible values for this '-option'
                        // arg is a potential value  
                        if List.contains arg symb then
                            f arg
                            consumeArg ()
                        else raise (Stop (Wrong (s, arg, "one of: " + (makeSymlist "" " " "" symb))))
                    | SetString r -> 
                        r := getArg ()
                        consumeArg ()
                    | Int f ->
                        let arg = getArg ()
                        match intOpt arg with
                        | None -> raise (Stop (Wrong (s, arg, "an integer")))
                        | Some x -> f x
                        consumeArg ()
                    | SetInt r -> 
                        let arg = getArg ()
                        match intOpt arg with
                        | None -> raise (Stop (Wrong (s, arg, "an integer")))
                        | Some x -> r := x
                    | Float f ->
                        let arg = getArg ()
                        match floatOpt arg with
                        | None -> raise (Stop (Wrong (s, arg, "a float")))
                        | Some x -> f x
                        consumeArg ()
                    | SetFloat r -> 
                        let arg = getArg () 
                        match floatOpt arg with
                        | None -> raise (Stop (Wrong (s, arg, "a float")))
                        | Some x -> r := x
                    | Tuple specs -> // ?
                        noArg ()
                        List.iter handleAction specs
                    | Rest f ->
                        noArg () // f will never be called on an empty list. Subtle difference between Rest and RestAll
                        while !current < (Array.length !argv) - 1 do
                          f (!argv).[!current + 1]
                          consumeArg ()
                    | RestAll f ->
                        noArg ()
                        let acc = ref [] // for RestAll f can be called on an empty list 
                        while !current < Array.length !argv - 1 do
                          acc := (!argv).[!current + 1] :: !acc
                          consumeArg ()
                        f (List.rev !acc)
                    | Expand f -> // who needs this anyway?
                        if not expand then // remember 'expand?'
                          raise (InvalidArgument "Arg.Expand is is only allowed with \
                                                   Arg.parse_and_expand_argv_dynamic")
                        let arg = getArg () 
                        let newarg = f arg 
                        consumeArg ()
                        let before = Array.sub !argv 0 (!current + 1)
                        let after = Array.sub !argv (!current + 1) ((Array.length !argv) - !current - 1) 
                        argv := Array.concat [before; newarg; after]

                handleAction action  
            else anonfun s
        with Bad m -> raise (convertError (Message m)) | Stop e -> raise (convertError e)
        incr current // advance 

[<RequireQualifiedAccess>]
module Sys =
    let argv = System.Environment.GetCommandLineArgs ()

// can use Expand here to update argv
let parseAndExpandArgvDynamic current argv speclist anonfun errmsg =
    parseAndExpandArgvDynamicAux true current argv speclist anonfun errmsg

// current should be optional, but I'll just use the global current directly.
let parseArgvDynamic argv speclist anonfun errmsg =
    parseAndExpandArgvDynamicAux false current (ref argv) speclist anonfun errmsg

// current problem just as above. Pun intended.
let parseArgv argv speclist anonfun errmsg =
    parseArgvDynamic argv (ref speclist) anonfun errmsg

let parse l f msg = 
    try 
        parseArgv Sys.argv l f msg
    with
    | Bad msg -> eprintf "%s" msg; exit 2
    | Help msg -> printf "%s" msg; exit 0


let parseDynamic l f msg =
    try
        parseArgvDynamic Sys.argv l f msg
    with
    | Bad msg -> eprintf "%s" msg; exit 2
    | Help msg -> printf "%s" msg; exit 0

let parseExpand l f msg =
    try
        let argv = ref Sys.argv 
        let spec = ref l 
        let current = ref (!current) 
        parseAndExpandArgvDynamic current argv spec f msg
    with
    | Bad msg -> eprintf "%s" msg; exit 2
    | Help msg -> printf "%s" msg; exit 0