///Project Euler Problem 17
///If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
///then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
///
///If all the numbers from 1 to 1000 (one thousand) inclusive were written out in 
///words, how many letters would be used?
///
///NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
///contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use 
//of "and" when writing out numbers is in compliance with British usage.
//-----------------------------------------------------------------------
// <copyright file="words.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

#light
open System

let ones = function
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | _ -> ""
    
let rec dec = function
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | 20 -> "twenty"
    | 30 -> "thirty"
    | 40 -> "forty"
    | 50 -> "fifty"
    | 60 -> "sixty"
    | 70 -> "seventy"
    | 80 -> "eighty"
    | 90 -> "ninety"
    | n when n < 10 -> ones n
    | n -> dec ((n/10)*10) + "-" + ones (n - (n/10)*10)

let rec hundred = function
    | 100 -> "one hundred"
    | 200 -> "two hundred"
    | 300 -> "three hundred"
    | 400 -> "four hundred"
    | 500 -> "five hundred"
    | 600 -> "six hundred"
    | 700 -> "seven hundred"
    | 800 -> "eight hundred"
    | 900 -> "nine hundred"
    | n -> hundred ((n/100)*100) + " and " + dec (n - (n/100)*100)

let rec thousand = function
    | 1000 -> "one thousand"
    | 2000 -> "two thousand"
    | 3000 -> "three thousand"
    | 4000 -> "four thousand"
    | 5000 -> "five thousand"
    | 6000 -> "six thousand"
    | 7000 -> "seven thousand"
    | 8000 -> "eight thousand"
    | 9000 -> "nine thousand"
    | n -> thousand ((n/1000)*1000) + " " + hundred (n - (n/1000)*1000)

let show n = 
    match n with
        | _ when n > 999 -> thousand n
        | _ when n > 99  -> hundred n
        | _ when n > 9   -> dec n
        | _              -> ones n

let size (s: string)  = s.Replace(" " ,"").Replace("-","").Length

[1..1000] |> Seq.map show |> Seq.map size|> Seq.sum |> printfn "solution: %d"

Console.ReadKey(true) |> ignore
