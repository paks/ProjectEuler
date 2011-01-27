///Project EuIer ProbIem 68
///Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to 
///nine.
///
///                                      (4)
///                                         \
///                                         (3)
///                                        /   \
///                                      (1)---(2)---(6)
///                                     /
///                                   (5)
///
///Working _clockwise_, and starting from the group of three with the numerically lowest external 
///node (4,3,2 in this example), each solution can be described uniquely. For example, the above 
///solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.
///
///It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight 
///solutions in total.
///Total	Solution Set
///9	4,2,3; 5,3,1; 6,1,2
///9	4,3,2; 6,2,1; 5,1,3
///10	2,3,5; 4,5,1; 6,1,3
///10	2,5,3; 6,3,1; 4,1,5
///11	1,4,6; 3,6,2; 5,2,4
///11	1,6,4; 5,4,2; 3,2,6
///12	1,5,6; 2,6,4; 3,4,5
///12	1,6,5; 3,5,4; 2,4,6
///
///By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon 
///ring is 432621513.
///
///Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit 
///strings. What is the maximum 16-digit string for a "magic" 5-gon ring?
///
///                            ( )
///                               \
///                               ( )     ( )
///                             /     \   /
///                           ( )      ( )
///                          /  \      /
///                       ( )   ( )--( )--( )
///                               \
///                               ( )
///
//-----------------------------------------------------------------------
// <copyright file="magicgon.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic

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

//If we have any solutions with 1-5 in the inner ring and 6-10 in the outher ring
//that family solutions would be max according to the problem statement
let perms15 = permutations [1..5]
let perms710 = permutations [7..10]
let isSolution (a : int array) = 
    (a.[0]+a.[1]+a.[2]) = (a.[6]+a.[2]+a.[3]) && (a.[6]+a.[2]+a.[3]) = (a.[7]+a.[3]+a.[4]) && (a.[7]+a.[3]+a.[4]) = (a.[8]+a.[4]+a.[5]) && (a.[8]+a.[4]+a.[5]) = (a.[9]+a.[5]+a.[1])
let buildGon (a : int array) = 
    let gon = Array.create 15 0
    gon.[0] <- a.[0]
    gon.[1] <- a.[1]
    gon.[2] <- a.[2]
    gon.[3] <- a.[6]
    gon.[4] <- a.[2]
    gon.[5] <- a.[3]
    gon.[6] <- a.[7]
    gon.[7] <- a.[3]
    gon.[8] <- a.[4]
    gon.[9] <- a.[8]
    gon.[10] <- a.[4]
    gon.[11] <- a.[5]
    gon.[12] <- a.[9]
    gon.[13] <- a.[5]
    gon.[14] <- a.[1]
    gon |> Array.map(fun n -> n.ToString()) |> Seq.fold (+) "" 
    
let candidates = 
    [
        for p in perms15 do
            let g = Array.create 10 0
            g.[0] <- 6
            p |> List.iteri(fun i n -> g.[i+1] <- n)
            for ps in perms710 do
                ps |> List.iteri(fun i n -> g.[i+6] <- n)
                yield g
    ]    
    
let solution = candidates |> List.filter isSolution |> List.map buildGon |> List.sortWith(fun a b -> compare b a) |> List.head
solution |> printfn "solution: %s"
Console.ReadKey(true) |> ignore