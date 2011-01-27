///Project Euler Problem 57
///It is possible to show that the square root of two can be expressed as an infinite continued 
///fraction.
///
///                      sqrt 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
///
///By expanding this for the first four iterations, we get:
///
///1 + 1/2 = 3/2 = 1.5
///1 + 1/(2 + 1/2) = 7/5 = 1.4
///1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
///1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
///
///The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, 
///is the first example where the number of digits in the numerator exceeds the number of digits in 
///the denominator.
///
///In the first one-thousand expansions, how many fractions contain a numerator with more digits 
///than denominator?
//-----------------------------------------------------------------------
// <copyright file="sqr2.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

type Fraction(n :bigint, d:bigint) = 
    member f.N = n
    member f.D = d
    
    //[<OverloadID("addFractionFraction")>]
    static member (+) (a :Fraction, b :Fraction) = 
                                Fraction(a.N*b.D+b.N*a.D,a.D*b.D)

    //[<OverloadID("addBigIntFraction")>]
    static member (+) (a :bigint, b :Fraction) = Fraction(a,1I) + b

    //[<OverloadID("divideFractionFraction")>]
    static member (/) (a :Fraction, b :Fraction) = 
                                Fraction(a.N*b.D,a.D*b.N)

    //[<OverloadID("divideBigIntFraction")>]
    static member (/) (a :bigint, b :Fraction) = Fraction(a,1I) / b


//1 ++ 1/((2,1) + (1,2)) |> printfn "%A"

let isNumDigitsBiggerThanDeno (a :Fraction) = let (n,d) = (a.N,a.D)
                                              n.ToString().Length > d.ToString().Length

let expansion n = 
    if n = 0 then
        Fraction(3I,2I)
    else
        let rec expansion' next (acc : Fraction) = 
                    match next with
                        | 0 -> 2I + (1I / acc)
                        | _ -> let acc' = 2I + (1I / acc)
                               expansion' (next - 1) acc'
        1I + (1I/(expansion' (n - 1) (Fraction(2I,1I))))
        
let expansions = 0 |> Seq.unfold(fun n -> Some(expansion n, n+1) )
///expansions |> Seq.take 1000 |> Seq.iter(printfn "%A")
expansions |> Seq.take 1000 |> Seq.filter(isNumDigitsBiggerThanDeno) |> Seq.length |> printfn "solucion: %d"
Console.ReadKey(true) |> ignore

                                   