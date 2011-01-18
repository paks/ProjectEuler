///Project Euler Problem 27
///Euler published the remarkable quadratic formula:
///
///n² + n + 41
///
///It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. 
///However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 
///41, 41² + 41 + 41 is clearly divisible by 41.
///
///Using computers, the incredible formula  n² 79n + 1601 was discovered, which produces 80 
///primes for the consecutive values n = 0 to 79. The product of the coefficients, 79 and 1601, is 
///126479.
///
///Considering quadratics of the form:
///
///    n² + an + b, where |a| 1000 and |b| 1000
///
///    where |n| is the modulus/absolute value of n
///    e.g. |11| = 11 and |4| = 4
///
///Find the product of the coefficients, a and b, for the quadratic expression that produces the 
///maximum number of primes for consecutive values of n, starting with n = 0.
///
/// A: (71, -61I, 971I)
//-----------------------------------------------------------------------
// <copyright file="quadratic.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic

let quadratic n a b = (pown n 2) + n * a + b

let isPrime =
    let primes = 
       seq {
            // First prime
            yield 2
        
            let knownComposites = new HashSet<_>()

            // Loop through all odd numbers; evens can't be prime
            for i in 3 .. 2 .. 100000 do
        
                // Check if its in our list, if not, its prime
                let found = knownComposites.Contains(i)
                if not found then
                    yield i
                // Add all multiples of i to our sieve, starting
                // at i and irecementing by i.
                do for j in i .. i .. 100000 do
                    knownComposites.Add(j) |> ignore
            
       }
    let primeTable = Array.create 100000 false
    primes |> Seq.iter (fun p -> primeTable.[ int p] <- true)
    (fun (n: int64) -> primeTable.[int n])

let seqQuadraticPrime a b = (0L,0L) |> Seq.unfold (fun (n,x) -> 
                                                        let v = quadratic n a b
                                                        if isPrime(abs v) then
                                                            Some (v,(n+1L,0L))
                                                        else
                                                            None
                                                 )

let (a,b) =
        seq {
            for a in -999L .. 999L do
                for b in -999L .. 999L do
                    let length = seqQuadraticPrime a b |> Seq.length
                    yield ( length , (a , b))
        } |> Seq.maxBy fst |> snd


printfn "solution: %d" (a*b)
Console.ReadKey(true) |> ignore
