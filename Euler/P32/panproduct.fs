///Project Euler Problem 32
///We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly 
///once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
///
///The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing multiplicand, multiplier, 
///and product is 1 through 9 pandigital.
///
///Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 
///through 9 pandigital.
///HINT: Some products can be obtained in more than one way so be sure to only include it once in 
///your sum.
//-----------------------------------------------------------------------
// <copyright file="panproduct.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Diagnostics
open System.Collections.Generic
let sw = new Stopwatch()
sw.Start()

let sort (n : String) = new String(n.ToCharArray() |> Array.sort)

let isPandigital (n : String) =
                            let str = "123456789"
                            if str.Length <> n.Length then
                                false
                            else
                                str = sort n

let toStr (a :int) (b : int) (c :int) = a.ToString() + b.ToString() + c.ToString()

let products = 
    seq {
        let known = new HashSet<int>()
        
        for i in 2 .. 9998 do
            for j in i+1 .. 9999 do
                let product = i * j
                let result = toStr product i j
                let found = known.Contains(product)
                if not found && (result |> isPandigital) then
                    do known.Add(product) |> ignore
                    yield product //(product,i,j)
    }

//products |> Seq.iter(printfn "%A")

let result = products |> Seq.sum

result |> printfn "solution: %d"
sw.Stop()
sw.Elapsed |> printfn "solution: %A"
Console.ReadKey(true) |> ignore