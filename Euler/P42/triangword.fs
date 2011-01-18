///Project Euler Problem 42
///The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten 
//triangle numbers are:
///
///1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
///
///By converting each letter in a word to a number corresponding to its alphabetical position 
///and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 
///25 = 55 = t10. If the word value is a triangle number then we shall call the word a 
///triangle word.
///
///Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly 
///two-thousand common English words, how many are triangle words?
//-----------------------------------------------------------------------
// <copyright file="triangword.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO

let wordStr = File.ReadAllText("words.txt").Replace("\"","")

let words = wordStr.Split ','

let score (s : string) = s.ToCharArray() |> Array.fold (fun acc c -> (int c) - (int 'A') + 1 + acc) 0

let scores = words |> Array.map (score)

let maxScore = scores |> Seq.max

let isTriangular =
    let triangular n = (n * (n + 1))/2
    let triangulars = 1 |> Seq.unfold(fun n -> Some(triangular n, n+1)) |> Seq.takeWhile(fun t -> t <= maxScore) |> Set.ofSeq
    fun n -> triangulars.Contains(n)

let triangularWords = scores |> Seq.filter (isTriangular)

triangularWords |> Seq.length |> printfn "solution: %d" 

Console.ReadKey(true) |> ignore
