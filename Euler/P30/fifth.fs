///Project Euler Problem 30
///Surprisingly there are only three numbers that can be written as the sum of fourth powers of 
///their digits:
///
///    1634 = 1^4 + 6^4 + 3^4 + 4^4
///    8208 = 8^4 + 2^4 + 0^4 + 8^4
///    9474 = 9^4 + 4^4 + 7^4 + 4^4
///
///As 1 = 1^4 is not a sum it is not included.
///
///The sum of these numbers is 1634 + 8208 + 9474 = 19316.
///
///Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
//-----------------------------------------------------------------------
// <copyright file="fifth.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

//let digits' n = n.ToString().ToCharArray() |> Seq.map (fun ch -> int ch - int '0')
let digits n =
    let rec td' n = seq {
        let d = n % 10
        yield d
        let next = n / 10
        if next > 0 then
           yield! td' next
    }
    td' n

let pow = 
    let flip f a b = f b a
    digits >> Seq.sumBy (flip pown 5)

let fifth = 
        seq {
            for n in 10 .. 999999 do
                if n = pow n then
                    yield n
        }
// fifth |> Seq.length |> printfn "%d"
fifth |> Seq.sum |> printfn "solution: %d"
Console.ReadKey(true) |> ignore
