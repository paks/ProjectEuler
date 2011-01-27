///Project EuIer ProbIem 69
///Euler's Totient function, phi(n) [sometimes called the phi function], is used to determine the 
///number of numbers less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 
///8, are all less than nine and relatively prime to nine, phi(9)=6.
///
///                     n 	Relatively Prime 	phi(n) 	n/phi(n)
///                     2 	1                   1       2
///                     3 	1,2                 2       1.5
///                     4 	1,3                 2       2
///                     5 	1,2,3,4             4       1.25
///                     6 	1,5                 2       3
///                     7 	1,2,3,4,5,6         6       1.1666...
///                     8 	1,3,5,7             4       2
///                     9 	1,2,4,5,7,8         6       1.5
///                     10 	1,3,7,9             4       2.5
///
///It can be seen that n=6 produces a maximum n/phi(n) for n 10.
///
///Find the value of n 1,000,000 for which n/phi(n) is a maximum.
//-----------------------------------------------------------------------
// <copyright file="phin.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler2

let primes = primes |> Seq.map int

// n/phi(n) would be maximal for minimal phi(n)

let solution = 
    primes 
    |> Seq.scan(fun (n,_) p -> n * p,n) (1,0) 
    |> Seq.find(fst >> (<) 1000000) 
    |> snd

solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore
