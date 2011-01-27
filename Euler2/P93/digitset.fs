///Project Euler Problem 93
///By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four 
///arithmetic operations (+, , *, /) and brackets/parentheses, it is possible to form different 
///positive integer targets.
///
///For example,
///
///                8 = (4 * (1 + 3)) / 2
///                14 = 4 * (3 + 1 / 2)
///                19 = 4 * (2 + 3) 1
///                36 = 3 * 4 * (2 + 1)
///
///Note that concatenations of the digits, like 12 + 34, are not allowed.
///
///Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 
///is the maximum, and each of the numbers 1 to 28 can be obtained before encountering the first 
///non-expressible number.
///
///Find the set of four distinct digits, a b < c d, for which the longest set of consecutive 
///positive integers, 1 to n, can be obtained, giving your answer as a string: abcd.
//-----------------------------------------------------------------------
// <copyright file="squareseq.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler2

[<CustomComparisonAttribute>]
[<CustomEqualityAttribute>]
type OP =
    | Add
    | Subs
    | Mult
    | Div
    interface IComparable<OP> with
        member op.CompareTo (o :OP) =
            match op with
                | Add -> match o with
                            | Add -> 0
                            | _ -> -1
                | Subs -> match o with 
                            | Add -> 1
                            | Subs -> 0
                            | _ -> -1
                | Mult -> match o with
                            | Mult -> 0
                            | Div -> -1
                            | _ -> 1
                | Div -> match o with
                            | Div -> 0
                            | _ -> -1
    interface IComparable with
        member op.CompareTo (ob :obj) =
            let o = unbox<OP> ob 
            let ic = op :> IComparable<OP>
            ic.CompareTo(o)

    override o.Equals (ob: obj) = 
        if ob = null then
            false
        else
            let ic = o :> IComparable
            ic.CompareTo(ob) = 0
    
    override op.GetHashCode () = 
            match op with
                | Add -> 1
                | Subs -> 2
                | Mult -> 3
                | Div -> 4

    member o.toOp =
        match o with
            | Add -> (+)
            | Subs -> (-)
            | Mult -> (*)
            | Div -> (/)
    
let rules = [Add;Subs;Mult;Div]

let operations r = 
    let rec pairs list = 
        seq {
            match list with
                | [] -> ()
                | h::t ->
                    for i in t do
                        yield [h;i]
                    yield! pairs t
        }
    let operations' (r :'a list) = 
        let toBinArray (n :int) = 
            let str = Convert.ToString(n,2)
            if str.Length = 3 then
                str.ToCharArray() |> Array.map(fun c -> int c - int '0')
            elif str.Length = 2 then
                ("0" + str).ToCharArray() |> Array.map(fun c -> int c - int '0')
            else
                ("00" + str).ToCharArray() |> Array.map(fun c -> int c - int '0')
                
        seq {
            for n in 0 .. 7 do
                let b = toBinArray n
                yield [r.[b.[0]];r.[b.[1]];r.[b.[2]]]
        }
    seq {
        for p in pairs r do
            yield! operations' p
        for p in combinations r 3 do
            yield! permutations p
    } |> Set.ofSeq //remove duplicates
 
let eval numbers (ops :OP list) =
    let rec eval' numbers (ops :OP list) acc = 
        match numbers,ops with
            | [],[] -> acc
            | [n],[o] -> o.toOp acc n
            | n::ns, o::os -> let acc' = o.toOp acc n
                              eval' ns os acc'
            | _ -> failwith "shouldn't match"
    eval' (List.tail numbers) ops (List.head numbers)

let eval2 numbers (ops :OP list) =
    let rec eval' numbers (ops :OP list) acc = 
        match numbers,ops with
            | [],[] -> acc
            | [n],[o] -> o.toOp n acc
            | n::ns, o::os -> let acc' = o.toOp n acc
                              eval' ns os acc'
            | _ -> failwith "shouldn't match"
    eval' (List.tail numbers) ops (List.head numbers)

let buildSet numbers =
    let ops = operations rules
    let perms = permutations numbers
    let isInteger f = 
        let diff = f - float ( int f)
        diff = 0.0
    seq {
        for ns in perms do
            for o in ops do
                let result = eval ns o
                if result > 0.0 && result |> isInteger  then
                    yield result
                let result2 = eval2 ns o
                if result2 > 0.0 && result2 |> isInteger  then
                    yield result2
    } |> Set.ofSeq //remove duplicates

let getMax list =
    list |> Seq.fold(fun acc n -> if acc = n - 1. then n else acc) 0.
(*
let test = [1.;2.;3.;4.;]
    
let rr = buildSet test
rr |> Seq.iter(printfn "%f")
rr |> Seq.length |> printfn "%d"
rr |> getMax |> printfn "%f"
*)
let solutions = 
    let digits = [1.;2.;3.;4.;5.;6.;7.;8.;9.]
    let digiCombs = combinations digits 4
    seq {
        for dig in digiCombs do
            let result = buildSet dig
            yield dig, result |> getMax
    }

//solutions |> Seq.iter(printfn "%A")
let solution = solutions |> Seq.maxBy snd |> fst |> Seq.map(sprintf "%.0f") |> String.Concat
solution |> printfn "solution: %s"

Console.ReadKey(true) |> ignore

