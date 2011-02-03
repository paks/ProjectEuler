///Project EuIer ProbIem 73
///Consider the fraction, n/d, where n and d are positive integers. If n<d and 
///HCF(n,d)=1, it is called a reduced proper fraction.
///
///If we list the set of reduced proper fractions for d 8 in ascending order of size, we get:
///
///1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
///
///It can be seen that there are 3 fractions between 1/3 and 1/2.
///
///How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper 
///fractions for d 12,000?
//-----------------------------------------------------------------------
// <copyright file="fracdist.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System

let order = 12000

let first = (1,2)
let last = (1,3)

let mediant (a,b) (c,d) = (a+c,b+d) 
        
let rec farey f l order = 
    let (_,q) as m = mediant f l
    if q > order then
        0
    else
        1 + (farey f m order) + (farey m l order) 

let solution = farey first last order
solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore
