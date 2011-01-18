///Project Euler Problem 39
///A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with 
///denominators 2 to 10 are given:
///
///    1/2	= 	0.5
///    1/3	= 	0.(3)
///    1/4	= 	0.25
///    1/5	= 	0.2
///    1/6	= 	0.1(6)
///    1/7	= 	0.(142857)
///    1/8	= 	0.125
///    1/9	= 	0.(1)
///    1/10	= 	0.1
///
///Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has 
///a 6-digit recurring cycle.
///
///Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal 
///fraction part.
//-----------------------------------------------------------------------
// <copyright file="fraction.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Collections.Generic


let cicle d = 
        let table = new HashSet<int>()
        let rec cicle' n =
                match n with 
                | _ when n < 10       -> let comp = (n*10) % d
                                         if comp = 0 then
                                            0
                                         else 
                                            if table.Contains(comp) then
                                                0
                                            else
                                                table.Add(comp) |> ignore
                                                1 + cicle' (comp)
                | nn when n < 100    -> let comp = (nn*100) % d
                                        if comp = 0 then
                                            0
                                        else 
                                            if table.Contains(comp) then
                                                0
                                            else
                                                table.Add(comp) |> ignore
                                                1 + cicle' (comp)
                | nnn when n < 1000 -> let comp = (nnn*1000) % d
                                       if comp = 0 then
                                            0
                                       else 
                                            if table.Contains(comp) then
                                                0
                                            else
                                                table.Add(comp) |> ignore
                                                1 + cicle' (comp)
                | _                   ->  0

        match d with 
        | _ when d < 10       -> if 10 % d = 0 then
                                    0
                                 else cicle' (10-d)
        | dd when d < 100    -> if 100 % dd = 0 then
                                    0
                                 else cicle' (100-dd)
        | ddd when d < 1000 -> if 1000 % ddd = 0 then
                                    0
                                 else cicle' (100-ddd)
        | _                   ->  0

//cicle 2 |> printfn "1/2: %d"
//cicle 3 |> printfn "1/3: %d"
//cicle 6 |> printfn "1/6: %d"
//cicle 7 |> printfn "1/7: %d"
//cicle 8 |> printfn "1/8: %d"
//cicle 9 |> printfn "1/9: %d"

//cicle 71 |> printfn "%d"
//cicle 20 |> printfn "%d"

//cicle 101 |> printfn "%d"
//cicle 200 |> printfn "%d"

let result = [2..999] 
             |> List.map(fun n -> (n,cicle n))
             |> List.maxBy snd 
             |> fst

result |> printfn "solution: %d"

Console.ReadKey(true) |> ignore