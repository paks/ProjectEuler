///Project Euler http://projecteuler.net
///Solutions to problems 51 to 50
//-----------------------------------------------------------------------
// <copyright file="euler2.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
module Euler2
open System
open System.Collections.Generic
open System.Numerics
open Primegen

let version = "1.0.0.0"

let primes = 
    seq {
        let primes = new Primes()
        primes.skipto(2UL)
        while true do
            yield primes.next()
    }

///Miller-Rabin comes from http://www.haskell.org/haskellwiki/Prime_numbers
let private quotRem (a :bigint) b = 
    let quote = a / b
    let rem = a - (quote * b)
    (quote,rem)

let private find2km (n : bigint) = 
    let rec f k m = 
        let (q,r) = quotRem m 2I
        match r with
            | _ when r = 1I -> (k,m)
            | _            -> f (k+1I) q
    f 0I n

let private pow' mul sq x' n' = 
    let rec f x n y = 
        if n = 1I then
            mul x y
        else
            let (q,r) = quotRem n 2I
            let x2 = sq x
            if r = 0I then
                f x2 q y
            else
                f x2 q (mul x y)
    f x' n' 1I
        
let mulMod (a :bigint) b c = (b * c) % a
let squareMod (a :bigint) b = (b * b) % a
let powMod m = pow' (mulMod m) (squareMod m)

let private millerRabinPrimality (n :bigint) a =
    let n' = n - 1I
    let (k,m) = find2km n'
    let b0 = powMod n a m
    let iterate f x = (x,0) |> Seq.unfold(fun (x',_) -> let fx = f x'
                                                        Some(fx,(fx,0)))
    let b = iterate (squareMod n) b0 |> Seq.take (int (k - 1I)) |> Seq.toList
    let rec iter l = 
        match l with
            | [] -> false
            | (x::xs) -> match x with
                            | _ when x = 1I -> false
                            | _ when x = n' -> true
                            | _      -> iter xs
    match (a,n) with
        | _ when a <= 1I && a >= n - 1I -> failwith (sprintf "millerRabinPrimality: a out of range (%A for %A)" a n)
        | _ when n < 2I -> false
        | _ when n % 2I = 0I -> false
        | _ when b0 = 1I || b0 = n' -> true
        | _  -> iter b

let public isPrime = function
    | n when n < 2I -> false
    | n when n = 2I -> true
    | n when n = 3I -> true
    | n when n % 2I = 0I -> false
    | n when n = 3I -> true
    | n             -> let result = millerRabinPrimality n 2I 
                       if result then
                            millerRabinPrimality n 3I
                       else
                            false
(*
let memoizeM f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None     ->
                    let res = f x
                    cache := (!cache).Add(x,res)
                    res
*)
let memoize f =
    let cache = Dictionary<_, _>()
    fun x ->
        let ok,res = cache.TryGetValue(x)
        if ok then res
        else let res = f x
             cache.[x] <- res
             res

let rec permutations =function 
            | [] -> [[]]
            | xs -> let r = new List<'a list>()
                    let h = new List<'a>()
                    for x in xs do
                        if not (h.Contains(x)) then
                            let ts = xs |> List.filter(fun a -> a <> x)
                            for p in permutations(ts) do
                                r.Add(x::p) |> ignore
                        h.Add(x) |> ignore
                    r |> Seq.toList

let combinations list (r: int) =
    let rec factorial = function
        | n when n = 0I -> 1I
        | n -> n * factorial (n-1I)
    let d = list |> List.toArray
    let ri = bigint r
    let ni = bigint(list.Length)
    let total = (factorial ni) / ((factorial ri) * (factorial (ni - ri))) |> int
    let n = int ni
    let r = int ri
    let builder (d :array<_>) a =
        let rec builder' a acc =
            match a with
                | [] -> acc
                | n::ns -> let acc' = d.[n] :: acc
                           builder' ns acc'
        builder' a [] |> List.rev
    let a = [|0..r-1|];
    seq {
        yield d.[0..r-1] |> List.ofArray
        for x in 1 .. total-1 do
            let i = ref (r - 1)
            while a.[!i] = (n - r + !i) do
                i := !i - 1
            a.[!i] <- a.[!i] + 1
            let j = ref (!i + 1)
            for j in (!i + 1) .. r - 1 do
                a.[j] <- a.[!i] + j - !i
            yield builder d (a |> Array.toList)
    }


// this function came from here: http://geekswithblogs.net/Erik/archive/2008/02/19/119734.aspx
let primeFactorsSeq (num:float) =
        let div = ref 2.0
        let get num2  =
            let sq = sqrt (num2)
            while( (not(num2 % !div = 0.0)) && (!div < sq) ) do
                if (!div = 2.0) then
                    div  := !div + 1.0
                else
                    div  := !div + 2.0
            div
        let divSeq = num |> Seq.unfold (fun x ->
            let sq = sqrt (x)
            let divisor = get x
            if (x = 1.0) then
                None
            else if (sq < !divisor) then
                Some (x, 1.0)  // x is prime!
            else
                Some(!divisor, x/(!divisor))
        )
        divSeq

let primeFactors = primeFactorsSeq >> Seq.countBy id

let rec factorial = function
    | n when n = 0I -> 1I
    | n -> n * factorial (n-1I)
    
// Some useful Haskell functions
let uncurry f (a,b) = f a b
let curry f a b = f(a,b)
let flip f a b = f b a 