///Project Euler Problem 37
///The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the 
///digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
///
///Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
///
///    * d2d3d4=406 is divisible by 2
///    * d3d4d5=063 is divisible by 3
///    * d4d5d6=635 is divisible by 5
///    * d5d6d7=357 is divisible by 7
///    * d6d7d8=572 is divisible by 11
///    * d7d8d9=728 is divisible by 13
///    * d8d9d10=289 is divisible by 17
///
///Find the sum of all 0 to 9 pandigital numbers with this property.
//-----------------------------------------------------------------------
// <copyright file="pandiun.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic

let rec fact x = if x <= 0 then 1 else x * fact (x - 1)

let rec LexoPermutation (digits:List<int>) target =
    if digits.Count = 1 then
        [digits.[0]]
    else
        let bucketSize = (fact digits.Count) / digits.Count
        let digitToSplitIdx = target / bucketSize
        let digitSplit = digits.[digitToSplitIdx]
        digits.RemoveAt(digitToSplitIdx)
        digitSplit :: (LexoPermutation digits (target - bucketSize * digitToSplitIdx))

let isSpecial (l :int list) =
                let digits = l |> List.toArray
                if digits.[3] % 2 <> 0 then
                    false
                else
                    if digits.[5] <> 0 && digits.[5] <> 5 then
                        false
                    else
                        let num = digits.[2]*100+digits.[3]*10+digits.[4]
                        if num % 3 <> 0 then
                            false
                        else
                            let num = digits.[4]*100+digits.[5]*10+digits.[6]
                            if num % 7 <> 0 then
                                false
                            else
                                let num = digits.[5]*100+digits.[6]*10+digits.[7]
                                if num % 11 <> 0 then
                                    false
                                else
                                    let num = digits.[6]*100+digits.[7]*10+digits.[8]
                                    if num % 13 <> 0 then
                                        false
                                    else
                                        let num = digits.[7]*100+digits.[8]*10+digits.[9]
                                        num % 17 = 0
                
// [1;2;3;4;5;6;7;8;9;0] |> isSpecial |> printfn "%A"
// [1;4;0;6;3;5;7;2;8;9] |> isSpecial |> printfn "%A"
let pandigitals = 
    seq {
        for i in fact 9 .. fact 10 - 1 do
            let digits = new List<int>([0..9])
            let pandi = LexoPermutation digits i
            if isSpecial pandi then
                yield pandi
    } |> Seq.cache

let toInt64 = List.map int64 >> List.fold(fun acc d -> acc*10L + d) 0L
// pandigitals |> Seq.iter(printfn "%A")
pandigitals |> Seq.map toInt64 |> Seq.sum |> printfn "solution: %d"

Console.ReadKey(true) |> ignore

