///Project Euler Problem 62
///The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 
///66430125 (4053). In fact, 41063625 is the smallest cube which has exactly three permutations of 
///its digits which are also cube.
///
///Find the smallest cube for which exactly five permutations of its digits are cube.
//-----------------------------------------------------------------------
// <copyright file="cubes.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic

let cubes = 1L |> Seq.unfold(fun n -> Some(pown n 3, n+1L))

let key (n :int64) = new String(n.ToString().ToCharArray() |> Array.sort)

let solution = 
    (new Dictionary<_,_>(), cubes) 
        ||> Seq.scan(fun table cube -> 
                let k = key cube
                let ok,numbers = table.TryGetValue(k)
                if ok then
                    table.Remove(k) |> ignore
                    table.Add(k,cube::numbers)
                else
                    table.Add(k,[cube])
                table) 
        |> Seq.choose(fun table -> table.Values |> Seq.tryFind(List.length >> (=) 5))
        |> Seq.head 
        |> List.min

solution |> printfn "solution: %d"
