///Project Euler Problem 39
///If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly 
///three solutions for p = 120.
///
///                                 {20,48,52}, {24,45,51}, {30,40,50}
///
///For which value of p 1000, is the number of solutions maximised?
//-----------------------------------------------------------------------
// <copyright file="perimeter.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic

type solution = int * (int * int * int)

let calcB a p = 
        if p < a then
            None
        else
            let n = p*p - (2*p*a)
            let d = 2*(p-a)
            if d <> 0 && n % d = 0 then
                let b = n/d
                if b > 0 then
                    Some (n/d)
                else
                    None
            else
                None
let triangle a p = calcB a p |> Option.map(fun b -> (a,b,p-a-b))

let solutions =
    seq {
        let table = new HashSet<solution>()
        
        for p in 1 .. 1000 do
            for a in 1 .. p do //a < p
                match triangle a p with
                    | Some s -> let (a1,b1,c1) = s
                                let s1 = (b1,a1,c1)
                                let found = table.Contains((p,s1))
                                if not found then
                                    yield (p,s)
                                table.Add((p,s)) |> ignore
                    | None   -> ()
    }

let s = solutions |> Seq.countBy fst |> Seq.maxBy snd |> fst

printfn "solution: %d" s
Console.ReadKey(true) |> ignore