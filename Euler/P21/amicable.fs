///Project Euler Problem 21
///Let d(n) be defined as the sum of proper divisors of n (numbers less 
///than n which divide evenly into n).
///If d(a) = b and d(b) = a, where a b, then a and b are an amicable 
///pair and each of a and b are called amicable numbers.
///
///For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 
/// 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors 
///of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
///
//Evaluate the sum of all the amicable numbers under 10000.
//-----------------------------------------------------------------------
// <copyright file="amicable.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let divisors n =
        seq {
            yield 1L
            for i in 2L ..  n - 1L do
                if n % i = 0L  then
                    yield i
        } 

let sumDiv n =  divisors n |> Seq.sum

let amicables =
    seq {
        for n in 2L .. 10000L do
            let dn = sumDiv n
            if dn <> n  && dn > n then //if dn is less than n, we already calculated that pair
                let ami = sumDiv dn
                if(n = ami) then
                    yield (n, dn)
    }

amicables |> Seq.sumBy (fun (a,b) -> a + b) |> printfn "solution: %d"
Console.ReadKey(true) |> ignore