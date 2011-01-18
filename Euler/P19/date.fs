///Project Euler Problem 19
///You are given the following information, but you may prefer to do some research for yourself.
///
/// * 1 Jan 1900 was a Monday.
/// * Thirty days has September,
///   April, June and November.
///   All the rest have thirty-one,
///   Saving February alone,
///   Which has twenty-eight, rain or shine.
///   And on leap years, twenty-nine.
/// * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is 
///    divisible by 400.
///
///How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 
///to 31 Dec 2000)?
//-----------------------------------------------------------------------
// <copyright file="date.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let firstOfMonth20thCentury = 
    seq {
        for year in 1901 .. 1 .. 2000 do
            for month in 1 .. 12 do
                yield new DateTime(year,month,1)
    }

let daysInSunday = firstOfMonth20thCentury |> Seq.filter (fun d -> d.DayOfWeek = DayOfWeek.Sunday)

daysInSunday |> Seq.length |> printfn "solution: %d"

Console.ReadKey(true) |> ignore
