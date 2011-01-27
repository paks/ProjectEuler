///Project Euler Problem 98
///By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively, we form a 
///square number: 1296 = 362. What is remarkable is that, by using the same digital substitutions, 
///the anagram, RACE, also forms a square number: 9216 = 962. We shall call CARE (and RACE) a 
///square anagram word pair and specify further that leading zeroes are not permitted, neither may 
///a different letter have the same digital value as another letter.
///
///Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly 
///two-thousand common English words, find all the square anagram word pairs (a palindromic 
///word is NOT considered to be an anagram of itself).
///
///What is the largest square number formed by any member of such a pair?
///
///NOTE: All anagrams formed must be contained in the given text file.
//-----------------------------------------------------------------------
// <copyright file="amicable.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO
open System.Collections.Generic


let words = File.ReadAllLines("words.txt").[0].Split ',' 
            |> Array.map (fun s -> s.Substring(1, (s.Length - 2))) 
            |> Array.sort

let anagrams = 
    let anagram (dict: Dictionary<_,_>) (s: string) = 
        let key = new String(s.ToCharArray() |> Array.sort)
        let ok,words = dict.TryGetValue(key)
        if ok then
            dict.Remove(key) |> ignore
            dict.Add(key,s::words)
        else
            dict.Add(key,[s])
        dict
    (new Dictionary<string,string list>(), words) 
    ||> Seq.fold(anagram) 
    |> Seq.map(fun kvp -> kvp.Value)
    |> Seq.filter(fun l -> l.Length > 1)

let maxLength = anagrams |> Seq.map(List.head >> String.length) |> Seq.max

let maxSquare = pown 10 maxLength

let squares = 
    1 |> Seq.unfold(fun n -> 
                    let square = n*n
                    if square < maxSquare then
                        Some(square,n+1)
                    else
                        None) |> List.ofSeq
let isSquare = 
    let squareTable = squares |> Set.ofList
    fun n -> squareTable.Contains(n)

//anagrams |>Seq.toList |> List.sortBy(fun a -> a|> List.head |> String.length) |> Seq.iter(printfn "%A")
//anagrams |> Seq.filter(fun l -> l.Length > 2) |> Seq.iter(printfn "%A")
//maxLength |> printfn "max length %A"


//zipWord "CARA" 1296 |> mapAnagram "RACA" |> printfn "%d"
//zipWord "CARA" 9216 |> mapAnagram "RACA" |> printfn "%d"
let isSquareAnagram w1 w2 sq = 
    let zipWord (word :string) (square :int)  = 
        let wl = word.ToCharArray()
        let sl = square.ToString().ToCharArray()
        Array.zip wl sl
    let mapAnagram (w :string) (zw : (char * char) array) =
        let list = new List<char * char>()
        zw |> Seq.iter(fun e -> list.Add(e))
        let find c l = 
                let q = list.Find(fun (ch,n) -> ch = c)
                list.Remove(q) |> ignore
                q
        w.ToCharArray() 
        |> Array.map(fun ch -> find ch zw |> snd) 
        |> String.Concat 
        |> int

    let isValidCase (word :string) (square :int) =
        let numberSet = square.ToString().ToCharArray() |> Set.ofArray
        let combined = zipWord word square |> Set.ofArray
        numberSet.Count = combined.Count
    let zipped = zipWord w1 sq
    let number = mapAnagram w2 zipped
    if isValidCase w1 sq then
        if(sq.ToString().Length = number.ToString().Length) then
            (isSquare number, sq, number)
        else
            (false, sq, number)
     else
        (false, sq, number)

//isSquareAnagram "CARE" "RACE" 1296 |> printfn "%A"
let sqs size = squares |> Seq.filter(fun s -> s.ToString().Length = size) |> Seq.toArray
//sqs 4 |> Seq.iter(printfn "%d")
let squareAnagrams = 
        [
            let twoWords = anagrams |> Seq.filter(fun l -> l.Length = 2) |> Seq.map(Array.ofList)
            for words in twoWords do
                let fw = words.[0]
                let sw = words.[1]
                for sq in sqs fw.Length do
                    let isSq,sq,number = isSquareAnagram fw sw sq
                    if isSq then
                        yield (sq,number)
        ]

squareAnagrams 
    |> Seq.map(fun (sq,number) -> max sq number)
    |> Seq.max
    |> printfn "solution: %d"
Console.ReadKey(true) |> ignore


              
