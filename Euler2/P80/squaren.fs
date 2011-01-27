///Project Euler Problem 80
///It is well known that if the square root of a natural number is not an integer, then it 
///is irrational. The decimal expansion of such square roots is infinite without any 
///repeating pattern at all.
///
///The square root of two is 1.41421356237309504880..., and the digital sum of the first 
///one hundred decimal digits is 475.
///
///For the first one hundred natural numbers, find the total of the digital sums of the 
///first one hundred decimal digits for all the irrational square roots.
//-----------------------------------------------------------------------
// <copyright file="squaren.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics

let digits (n :float) = 
    let digitArr = n.ToString().Split '.' 
    let integral = if digitArr.[0].Length % 2 = 0 then
                        digitArr.[0]
                   else
                        "0"+digitArr.[0]
    let fractional = if digitArr.Length > 1 then
                        if digitArr.[1].Length % 2 = 0 then
                           digitArr.[1]
                        else
                           digitArr.[1]+"0"
                     else
                        "00"
    let buildList (s :string) = [|
                                    let arr = s.ToCharArray() |> Array.map(fun c -> int c - int '0')
                                    for i in 0 .. 2 .. arr.Length - 1 do
                                        yield arr.[i]*10 + arr.[i + 1]
                                |]
    let digi = Array.concat [buildList integral; buildList fractional]
    let index = ref 0        
    seq {
        while true do
            if !index < digi.Length then
                yield new BigInteger(digi.[!index])
                do index := !index + 1
            else
                yield 0I
    }
//digits 152.2756 |> Seq.take 10 |> Seq.iter(printfn "%A")

let getX c p =
    let rec calcUp x y = 
        let x' = x + 1I
        if x' > 9I then
            (x,y)
        else
            let y' = ((20I * p) + x')  * x';
            if y' > c then
                (x,y)
            else
                calcUp x' y'

    let rec calcDow x y = 
        let x' = x - 1I
        if x' < 0I then
            (x,y)
        else
            let y' = ((20I * p) + x')  * x';
            if y' < c then
                (x',y')
            else
                calcDow x' y'
    
    let initX = 
        let x' = c / (20I - p)
        if x' < 0I then
            0I
        elif x' > 9I then
            9I
        else
            x'
    
    let x = initX
    let y = ((20I * p) + x)  * x;
    let (x',c') = if y > c then
                     calcDow x y
                  else
                     calcUp x y
    (x', c - c')

let sqrtN n decimals = 
    let digi = digits n |> Seq.take decimals |> Seq.toList
    let acc = []
    let rec sqrt' digits p r acc =
        match digits with
            | [] -> acc
            | (d::ds) -> let c = 100I*r + d
                         if c + p = 0I then
                            acc
                         else
                            let (x,r') = getX c p
                            sqrt' ds (10I * p + x) r' (x::acc)
    sqrt' digi 0I 0I acc

//sqrtN 2.0 100 |> Seq.sum |> printfn "%A"

let isNotSquare  =
    let table = 
            let arr = Array.create 101 true
            [1 .. 10] |> Seq.iter(fun n -> arr.[n*n] <- false)
            arr
    fun n -> table.[n]

let solution = [2 .. 100] |> List.filter(isNotSquare) |> Seq.map(fun n -> sqrtN (float n) 100 |> Seq.sum) |> Seq.sum |> int
solution |> printfn "solution: %d"
Console.ReadKey(true) |> ignore