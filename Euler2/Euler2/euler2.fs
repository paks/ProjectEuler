///Project Euler http://projecteuler.net
///Solutions to problems 51 to 100
//-----------------------------------------------------------------------
// <copyright file="euler2.fs" >
// Copyright Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
module Euler2
open System
open System.Collections.Generic
open System.Numerics
open Primegen

let version = "1.0.0.1"

// Some useful Haskell functions
let uncurry f (a,b) = f a b
let curry f a b = f(a,b)
let flip f a b = f b a 
let iterate f = Seq.unfold(fun x -> let fx = f x in Some(x,fx))

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

let private millerRabinPrimality n a =
    let n' = n - 1I
    let (k,m) = find2km n'
    let b0 = powMod n a m
    let probe = Seq.truncate(int (k-1I)) >> Seq.tryPick(fun x -> if x = 1I then Some(false) elif x = n' then Some(true) else None)
            
    match (a,n) with
        | _ when a <= 1I && a >= n' -> failwith (sprintf "millerRabinPrimality: a out of range (%A for %A)" a n)
        | _ when b0 = 1I || b0 = n' -> true
        | _  -> b0 
                 |> iterate (squareMod n) 
                 |> Seq.skip 1 
                 |> probe 
                 |> Option.exists id 
                

let isPrimeW witnesses = function
    | n when n < 2I -> false
    | n when n = 2I -> true
    | n when n = 3I -> true
    | n when n % 2I = 0I -> false
    | n             -> witnesses |> Seq.forall(millerRabinPrimality n)

let isPrime = isPrimeW [2I;3I]

let memoize f =
    let cache = Dictionary<_, _>()
    fun x ->
        let ok,res = cache.TryGetValue(x)
        if ok then res
        else let res = f x
             cache.[x] <- res
             res

//These functions came form the Book "Introduction to Functional Programming" by Richard Bird and Philip Wadler.
    
let rec subs = function
    | [] -> [[]]
    | x::xs -> [ for ys in subs xs do
                    yield! [ys;x::ys] ]

let rec interleave x = function
    | [] -> [[x]]
    | y::ys -> 
        [ yield x::y::ys
          yield! interleave x ys |> List.map (curry List.Cons y)]

let rec perms = function
    | [] -> [[]]
    | x::xs -> List.concat (List.map (interleave x) (perms xs))

let choices xs = List.concat (List.map perms (subs xs))
    
let rec parts = function
    | []    -> [[]]
    | [x]   -> [[[x]]]
    | x::x'::xs -> List.map (glue x) (parts (x'::xs)) 
                    @ List.map (curry List.Cons [x]) (parts (x'::xs))

and glue x xss = (x :: List.head xss):: List.tail xss

//
let internal fact a b = [a .. b] |> Seq.fold(( * )) 1I

let binomial n r =
    (fact (r+1I) n) / (fact 1I (n - r))

let permu n r = (fact (n-r+1I) n)

let combinations list (r: int) =
    let d = list |> Seq.toArray
    let n = d.Length
    let total = (bigint n, bigint r) ||> binomial |> int
    let builder (d :array<_>) a =
        let rec builder' a acc =
            match a with
                | [] -> acc
                | n::ns -> let acc' = d.[n] :: acc
                           builder' ns acc'
        builder' a [] |> List.rev
    seq {
        let a = [|0..r-1|];
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
    

