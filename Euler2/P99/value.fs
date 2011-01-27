///Project Euler Problem 99
///Comparing two numbers written in index form like 2^11 and 3^7 is not difficult, as any calculator 
///would confirm that 2^11 = 2048  3^7 = 2187.
///
///However, confirming that 632382^518061 519432^525806 would be much more difficult, as both 
///numbers contain over three million digits.
///
///Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file containing one 
///thousand lines with a base/exponent pair on each line, determine which line number has the 
///greatest numerical value.
///
///NOTE: The first two lines in the file represent the numbers in the example given above.
//-----------------------------------------------------------------------
// <copyright file="amicable.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO

let numbers = File.ReadAllLines("base_exp.txt")
              |> Array.mapi(fun line s -> 
                        let numbers = s.Split ','
                        let n = float (numbers.[0])
                        let exp = float (numbers.[1])
                        (line+1,(n,exp)))

let solution = numbers |> Seq.maxBy(fun (_,(n, exp)) -> exp * Math.Log n ) |> fst

solution |> printfn "solution %d"
Console.ReadKey(true) |> ignore