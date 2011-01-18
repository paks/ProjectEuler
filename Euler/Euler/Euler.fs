///Project Euler http://projecteuler.net
///Solutions to problems 01 to 50
//-----------------------------------------------------------------------
// <copyright file="Euler.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

module Euler

open System
open System.Collections.Generic
open Primegen

let primes = 
    seq {
        let primes = new Primes()
        primes.skipto(2UL)
        while true do
            yield primes.next()
    }

//this function came from here: http://geekswithblogs.net/Erik/archive/2008/02/19/119734.aspx
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
        divSeq |> Seq.countBy id

let memoize f =
    let cache = Dictionary<_, _>()
    fun x ->
        let ok,res = cache.TryGetValue(x)
        if ok then 
            res
        else 
            let res = f x
            cache.[x] <- res
            res
