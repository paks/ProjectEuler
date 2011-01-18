///Project Euler Problem 33
///The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to 
///simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling 
///the 9s.
///
///We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
///
///There are exactly four non-trivial examples of this type of fraction, less than one in value, 
///and containing two digits in the numerator and denominator.
///
///If the product of these four fractions is given in its lowest common terms, find the value of the 
///denominator.
//-----------------------------------------------------------------------
// <copyright file="fraction.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let conv (n : int) = 
                [|
                    yield float (n / 10)
                    yield float (n % 10)
                |]

// conv 93 |> printfn "%A"
// conv 16 |> printfn "%A"

let eq n d = let narr = conv n
             let darr = conv d
             let nf = float n
             let df = float d
             let nd = nf / df
             if (narr.[0] = darr.[0]) && (darr.[1] <> 0.0) then
                nd = (narr.[1] / darr.[1])
             else 
                if (narr.[0] = darr.[1]) && (darr.[0] <> 0.0) then
                    nd = (narr.[1] / darr.[0])
                else
                    if (narr.[1] = darr.[0]) && (darr.[1] <> 0.0) then
                        nd = (narr.[0] / darr.[1])
                    else
                        if (narr.[1] = darr.[1]) && (darr.[0] <> 0.0) then
                            nd = (narr.[0] / darr.[0])
                        else
                            false

let fractions =
        seq {
            for n in 10 .. 98 do
                for d in n+1 .. 99 do
                    if (n % 10 <> 0) && (d % 10 <> 0) then
                        if eq n d then
                            yield (n,d)
        }

let rec gcd a b = 
    if b = 0 then
        a
    else
        gcd b (a % b)

let result = fractions |> Seq.fold (fun (ac,bc) (a,b) -> ac*a,bc*b) (1,1) |> (fun (n,d) -> d/(gcd n d))

result |> printfn "solution: %d"
Console.ReadKey(true) |> ignore