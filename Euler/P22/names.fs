///Project Euler Problem 22
///Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over 
///five-thousand first names, begin by sorting it into alphabetical order. Then working out the 
///alphabetical value for each name, multiply this value by its alphabetical position in the list to 
///obtain a name score.
///
///For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 
///9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 53 = 49714.
///
///What is the total of all the name scores in the file?
//-----------------------------------------------------------------------
// <copyright file="names.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO

let nameStr = File.ReadAllText("names.txt").Replace("\"","");
let names = nameStr.Split ',' |> Seq.sort

let score (s : string) = s |> Seq.sumBy (fun c -> (int c) - (int 'A') + 1)

let scores = names |> Seq.mapi (fun i s -> (i+1) * (score s)) |> Seq.sum

printfn "%d" scores
Console.ReadKey(true) |> ignore

