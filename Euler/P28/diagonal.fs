///Project Euler Problem 39
///Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is 
///formed as follows:
///
///                                 (21) 22 23 24 (25)
///                                 20 ( 7 ) (8 ) 9 10
///                                 19  6 ( 1 ) 2 11
///                                 18  (5)  4 ( 3 )12
///                                 (17) 16 15 14 (13)
///
///It can be verified that the sum of both diagonals is 101.
///
///What is the sum of both diagonals in a 1001 by 1001 spiral formed in the same way?
//-----------------------------------------------------------------------
// <copyright file="diagonal.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let diag n = 
            let n2 = n*n
            let n1 = (n-1)
            (n2,n2-n1,n2-(2*n1),n2-(3*n1))

let diagonals n = 
        seq {
            yield (1,0,0,0)
            for i in 3 .. 2 .. n do
                yield diag i
        }
        
let soultion = diagonals 1001 |> Seq.sumBy (fun (a,b,c,d) -> a+b+c+d)
//diag 9 |> printfn "%A"
soultion |> printfn "solution: %d"
Console.ReadKey(true) |> ignore
