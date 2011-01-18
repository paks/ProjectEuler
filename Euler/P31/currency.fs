///Project Euler Problem 31
///In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
///
///    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
///
///It is possible to make £2 in the following way:
///
///    1£1 + 150p + 220p + 15p + 12p + 31p
///
///How many different ways can £2 be made using any number of coins?
//-----------------------------------------------------------------------
// <copyright file="currency.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let value = [| 200;100;50;20;10;5;2;1 |]

let from2fto1f n = 2*n
let from1fto50p n = 2*n
let from50pto20p n = (2*n,n)
let from20pto10p n = 2*n
let from10pto5p n = 2*n
let from5pto2p n = (2*n,n)
let from2pto1p n = 2*n


let combinations = 
    seq {
        let start = [| 1;0;0;0;0;0;0;0 |]
        yield start |> Array.toList
        while (start.[0] > 0) do
            do start.[0] <- start.[0] - 1
               start.[1] <- start.[1] + (from2fto1f 1)
            yield start |> Array.toList
            while (start.[1] > 0) do
                do start.[1] <- start.[1] - 1
                   start.[2] <- start.[2] + (from1fto50p 1)
                yield start |> Array.toList
                let (tmpS2,tmpS3,tmpS4) = (start.[2],start.[3],start.[4])
                while (start.[2] > 0) do
                    do start.[2] <- start.[2] - 1
                    let (v20p,v10p) = from50pto20p 1
                    do start.[3] <- start.[3] + v20p
                       start.[4] <- start.[4] + v10p
                    if start.[4] % 2 = 0 then
                        do start.[3] <- start.[3] + 1
                        do start.[4] <- 0                       
                    yield start |> Array.toList
                    let (tmp1,tmp2) = (start.[3],start.[4])
                    while(start.[3] > -1) do
                        let tmp1 = start.[4]
                        while(start.[4] > 0) do
                            do start.[4] <- start.[4] - 1
                               start.[5] <- start.[5] + (from10pto5p 1)
                            yield start |> Array.toList
                            let tmp1 = start.[5]
                            while (start.[5] > 0) do
                                do start.[5] <- start.[5] - 1
                                let (v2p,v1p) = from5pto2p 1
                                do start.[6] <- start.[6] + v2p
                                   start.[7] <- start.[7] + v1p
                                if start.[7] % 2 = 0 then
                                    do start.[6] <- start.[6] + 1
                                    do start.[7] <- 0                       
                                yield start |> Array.toList
                                let (tmp1,tmp2) = (start.[6],start.[7])
                                while(start.[6] > 0) do
                                    do start.[6] <- start.[6] - 1
                                       start.[7] <- start.[7] + (from2pto1p 1)
                                    yield start |> Array.toList
                                do start.[6] <- tmp1
                                   start.[7] <- tmp2
                            do start.[5] <- tmp1
                               start.[6] <- 0
                               start.[7] <- 0
                        do start.[4] <- tmp1
                           start.[5] <- 0
                        do start.[3] <- start.[3] - 1
                           start.[4] <- start.[4] + (from20pto10p 1)
                        if start.[3] > -1 then
                            yield start |> Array.toList
                    do start.[3] <- tmp1
                       start.[4] <- tmp2                       
                do start.[2] <- tmpS2
                   start.[3] <- tmpS3
                   start.[4] <- tmpS4
    }

(* combinations |> Seq.iter(fun s ->
                            let sum = float (s |> Seq.mapi(fun i n -> n * value.[i]) |> Seq.fold(+) 0)/ 100.0
                            s |> Seq.iter(printf "%3d,")
                            printfn " %2.2f" sum) *)

combinations |> Seq.length |> printfn "combinations: %d"

//Console.ReadKey(true)