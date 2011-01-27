///Project Euler Problem 78
///Let p(n) represent the number of different ways in which n coins can be separated into piles. 
///For example, five coins can separated into piles in exactly seven different ways, so p(5)=7.
///
///                OOOOO
///               OOOO   O
///              OOO   OO
///             OOO   O   O
///            OO   OO   O
///           OO   O   O   O
///          O   O   O   O   O
///
///Find the least value of n for which p(n) is divisible by one million.
//-----------------------------------------------------------------------
// <copyright file="coins.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic
open Euler2

///see http://mathworld.wolfram.com/PartitionFunctionP.html equation (11)
#nowarn "40"
let rec P = memoize(fun n ->
    let even n = if n &&& 1L = 0L then true else false
    let p' n = 1L 
               |> Seq.unfold(fun k -> 
                    let k1 = n - (k*(3L*k - 1L)/2L)
                    let k2 = n - (k*(3L*k + 1L)/2L)
                    let p1 = P k1
                    let p2 = P k2
                    if k1 < 0L && k2 < 0L then
                        None
                    else
                        if even (k+1L) then
                            Some(p1+p2,k+1L) 
                        else
                            Some(-p1 - p2,k+1L))
    match n with
        | _ when n < 0L -> 0L
        | 0L            -> 1L
        | 1L            -> 1L
        | _             -> (Seq.sum (p' n) ) % 1000000L)

let pSeq = 1L |> Seq.unfold(fun n -> Some((n,P n),n+1L))
let solution = pSeq |> Seq.find(fun (n,pn) -> pn = 0L) |> fst
solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore
