///Project EuIer ProbIem 69
///The rules for writing Roman numerals allow for many ways of writing each number (see FAQ: 
///Roman Numerals). However, there is always a "best" way of writing a particular number.
///
///For example, the following represent all of the legitimate ways of writing the number sixteen:
///
///            IIIIIIIIIIIIIIII
///            VIIIIIIIIIII
///            VVIIIIII
///            XIIIIII
///            VVVI
///            XVI
///
///The last example being considered the most efficient, as it uses the least number of numerals.
///
///The 11K text file, roman.txt (right click and 'Save Link/Target As...'), contains one thousand 
///numbers written in valid, but not necessarily minimal, Roman numerals; that is, they are arranged in 
///descending units and obey the subtractive pair rule (see FAQ for the definitive rules for this problem).
///
///Find the number of characters saved by writing each of these in their minimal form.
///
///Note: You can assume that all the Roman numerals in the file contain no more than four consecutive 
///identical units.
//-----------------------------------------------------------------------
// <copyright file="roman.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO

let m = Map.ofList ['I',1;'V',5;'X',10;'L',50;'C',100;'D',500;'M',1000;'O',4;'P',9;'Q',40;'R',90;'S',400;'T',900]

let parse (s :string) = 
        let preprocess = s.Replace("IV","O").Replace("IX","P").Replace("XL","Q").Replace("XC","R").Replace("CD","S").Replace("CM","T")
        let value = preprocess.ToCharArray() |> Seq.map(fun x -> m.[x]) |> Seq.sum
        value

//parse "MMDCCCXCVI" |> printfn "%d"
let ones = function
    | 1 -> "I"
    | 2 -> "II"
    | 3 -> "III"
    | 4 -> "IV"
    | 5 -> "V"
    | 6 -> "VI"
    | 7 -> "VII"
    | 8 -> "VIII"
    | 9 -> "IX"
    | e -> failwith "error: parsing " + e.ToString()
    
let rec dec = function
    | 10 -> "X"
    | 20 -> "XX"
    | 30 -> "XXX"
    | 40 -> "XL"
    | 50 -> "L"
    | 60 -> "LX"
    | 70 -> "LXX"
    | 80 -> "LXXX"
    | 90 -> "XC"
    | n when n < 10 -> ones n
    | n -> dec ((n/10)*10) + ones (n % 10)

let rec hundred = function
    | 100 -> "C"
    | 200 -> "CC"
    | 300 -> "CCC"
    | 400 -> "CD"
    | 500 -> "D"
    | 600 -> "DC"
    | 700 -> "DCC"
    | 800 -> "DCCC"
    | 900 -> "CM"
    | n when n < 100 -> dec n
    | n -> hundred ((n/100)*100) + dec (n % 100)

let rec thousand = function
    | 1000 -> "M"
    | 2000 -> "MM"
    | 3000 -> "MMM"
    | 4000 -> "MMMM"
    | 5000 -> "MMMMM"
    | 6000 -> "MMMMMM"
    | 7000 -> "MMMMMMM"
    | 8000 -> "MMMMMMMM"
    | 9000 -> "MMMMMMMMM"
    | n -> thousand ((n/1000)*1000) + "" + hundred (n % 1000)

let show n = 
    match n with
        | _ when n > 999 -> thousand n
        | _ when n > 99  -> hundred n
        | _ when n > 9   -> dec n
        | _              -> ones n


let numbers = File.ReadAllLines("roman.txt")
              |> Array.map(fun s -> let n = parse s in (s, show n))

let solution = numbers |> Seq.sumBy(fun (a,b) -> a.Length-b.Length)
solution |> printfn "solution: %d"
Console.ReadKey(true) |> ignore

