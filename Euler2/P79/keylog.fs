///Project Euler Problem 79
///A common security method used for online banking is to ask the user for three random characters 
///from a passcode. For example, if the passcode was 531278, they may asked for the 2nd, 3rd, and 
///5th characters; the expected reply would be: 317.
///
///The text file, keylog.txt, contains fifty successful login attempts.
///
///Given that the three characters are always asked for in order, analyse the file so as to 
///determine the shortest possible secret passcode of unknown length.
///A: 73162890
//-----------------------------------------------------------------------
// <copyright file="keylog.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO
open System.Text.RegularExpressions

let passcodes = File.ReadAllLines("keylog.txt")
let listPasscodes = passcodes |> Array.map(fun s -> s.ToCharArray() |> Array.map(fun c -> int c - int '0') |> Array.toList) |> Array.toList

let removeKey key = List.map(List.filter((<>) key)) >> List.filter(List.isEmpty >> not)

let findKey (list : int list list) = 
    let candidates = list |> List.map(List.head) |> Seq.countBy id|> Seq.toList |> List.sortBy snd |> List.rev
    match candidates with
        | []       -> failwith "Error: this shouldn't happen"
        | [f]      -> fst f
        | f::s::xs -> if snd f <> snd s then
                        fst f
                      else
                        let a = fst f
                        let b = fst s
                        let ab = a.ToString() + b.ToString()
                        let test = passcodes |> Seq.exists(fun s -> let m = Regex.Match(s,ab) in m.Success )
                        if test then
                            a
                        else
                            b
let solvePasscode list = 
    let rec solvePasscode' list acc = 
        let key = list |> findKey
        let list' = removeKey key list
        match list' with
            | [] -> key::acc
            | _  -> solvePasscode' list' (key::acc)
    solvePasscode' list [] |> List.rev
    
solvePasscode listPasscodes |> Seq.fold(fun acc n -> acc + n.ToString()) "" |> printfn "%s"
Console.ReadKey(true) |> ignore