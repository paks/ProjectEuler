///Project Euler Problem 5
///2520 is the smallest number that can be divided by each of the numbers 
///from 1 to 10 without any remainder.
///
///What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
// This solution came from here: http://geekswithblogs.net/Erik/archive/2008/02/19/119734.aspx
open System

// The function primeFactors has been a workhorse for solving other euler problems.
// Thanks Erik.
let primeFactors (num:float) =
    let get num2  =
        let sq = Math.Sqrt (num2)
        let div = ref 2.0 
        while( (not(num2 % !div = 0.0)) && (!div < sq) ) do
            if (!div = 2.0) then
                div  := !div + 1.0
            else
                div  := !div + 2.0
        div
    let divSeq = num |> Seq.unfold (fun x ->
        let sq = Math.Sqrt (x) 
        let divisor = get x 
        if (x = 1.0) then
            None
        else if (sq < !divisor) then
            Some (x, 1.0)  // x is prime!
        else
            Some(!divisor, x/(!divisor))
    ) 
    divSeq

let pFactors = seq { 2 .. 20 } |> Seq.map (float >> primeFactors >> Seq.map int)

let all =   pFactors 
            |> Seq.map (Seq.countBy id)  
            |> Seq.concat

let uncurry f (a,b) = f a b

let solution = all 
               |> Seq.groupBy fst
               |> Seq.map (snd >> Seq.maxBy snd >> uncurry pown)
               |> Seq.fold ( * ) 1

printfn "solution: %d" (int solution)

Console.ReadKey(true) |> ignore
