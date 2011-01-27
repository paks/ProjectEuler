///Project Euler Problem 64
///All square roots are periodic when written as continued fractions and can be written in the form:
///sqrt(N)= a0 + ________________1_____________________
///               a1 + 	____________1__________________
///                     a2 + _________1________________
///                           a3 + ...
///
///For example, let us consider sqrt 23:
///sqrt(23)= 4 + sqrt 23 — 4 = 4 + __1________ = 4 + _____1_________
///                                __1________       1 + sqrt(23) -3
///                                sqrt(23)-4            -----------
///                                                           7
///If we continue we would get the following expansion:
///sqrt(23) = 4 + ___1_______________
///               1 +____1___________
///                   3 + ___1_______
///                       1 + __1____
///                           8 + ...
///
///The process can be summarised as follows:
///a0 = 4,_____1____ = sqrt(23)+4   1 + sqrt(23) - 3
///       sqrt(23)—4   ---------- =     -------------
///                        7                  7
///
///a1 = 1,_____7____ = 7(sqrt(23)+3) = 3 + sqrt(23) -3
///       sqrt(23)—3   -------------       -----------
///                         14                  2
///
///a2 = 3, 	 2        = 2(sqrt(23)+3)  = 1 + sqrt(23) - 4
///       -----------   -------------        ------------
///        sqrt(23)—3       14                    7
///
///It can be seen that the sequence is repeating. For conciseness, we use the notation 
///sqrt(23) = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.
///
///The first ten continued fraction representations of (irrational) square roots are:
///
///sqrt( 2)=[1;(2)], period=1
///sqrt( 3)=[1;(1,2)], period=2
///sqrt( 5)=[2;(4)], period=1
///sqrt( 6)=[2;(2,4)], period=2
///sqrt( 7)=[2;(1,1,1,4)], period=4
///sqrt( 8)=[2;(1,4)], period=2
///sqrt(10)=[3;(6)], period=1
///sqrt(11)=[3;(3,6)], period=2
///sqrt(12)= [3;(2,6)], period=2
///sqrt(13)=[3;(1,1,1,1,6)], period=5
///
///Exactly four continued fractions, for N <= 13, have an odd period.
///
///How many continued fractions for N <= 10000 have an odd period?
//-----------------------------------------------------------------------
// <copyright file="squarefractions.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System

///see http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
///Continued fraction for sqtr(n)
let sqrtn = float >> sqrt >> Math.Floor >> int

let cf n = 
        let a0 = sqrtn n
        let m0 = 0
        let d0 = 1
        (n,(a0,m0,d0,a0)) 
        |> Seq.unfold(fun (s,(a,m,d,a0)) ->
            if a0 = -1 then
                None
            else
                let mn = d*a-m
                let dn = (s - (mn*mn)) / d
                let an = (a0+mn)/dn
                if an <> 2*a0 then
                    Some(an,(s,(an,mn,dn,a0)))
                else
                    Some(an,(s,(an,mn,dn,-1))))

let limit = 10000
let isNotSquare = 
    let table = let arr = Array.create (limit+1) true
                let sqrtLimit = sqrtn limit
                for n in 2 .. sqrtLimit do
                    arr.[n*n] <- false
                arr
    fun n -> table.[n]


let oddPeriods = 
    seq {
        for n in 2 .. limit do
            if isNotSquare n then
                let period = cf n |> Seq.length
                if period &&& 1 = 1 then
                    yield period
    }

let solution = oddPeriods |> Seq.length
solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore
