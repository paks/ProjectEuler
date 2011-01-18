///Project Euler Problem 38
///Take the number 192 and multiply it by each of 1, 2, and 3:
///
///    192 1 = 192
///    192 2 = 384
///    192 3 = 576
///
///By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 
///the concatenated product of 192 and (1,2,3)
///
///The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the 
///pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
///
///What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated 
///product of an integer with (1,2, ... , n) where n > 1?
//-----------------------------------------------------------------------
// <copyright file="concat.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let sort (n : String) = new String(n.ToCharArray() |> Array.sort)

let isPandigital (n : String) =
                            let str = "123456789"
                            if str.Length <> n.Length then
                                false
                            else
                                str = sort n

let toStr (a :int) (b : int) (c :int) = a.ToString() + b.ToString() + c.ToString()
let toStr2 (a :int) (b : int) = a.ToString() + b.ToString()

let pandigitals = 
    seq {
        for i in 192 .. 333 do
            let result = toStr i (i*2) (i*3)
            if result |> isPandigital then
                yield Convert.ToInt32(result)
        for i in 5000 .. 9999 do
            let result = toStr2 i (i*2) 
            if result |> isPandigital then
                yield Convert.ToInt32(result)
    }
    
pandigitals |> Seq.max |> printfn "solution: %d"

Console.ReadKey(true) |> ignore