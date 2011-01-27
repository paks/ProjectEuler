///Project EuIer ProbIem 65
///The square root of 2 can be written as an infinite continued fraction.
///
///The infinite continued fraction can be written, 2 = [1;(2)], (2) indicates that 2 repeats ad 
///infinitum. In a simiIar way, 23 = [4;(1,3,1,8)].
///
///It turns out that the sequence of partiaI vaIues of continued fractions for square roots provide 
///the best rationaI approximations. let us consider the convergents for 2.
///
///Hence the sequence of the first ten convergents for 2 are:
///1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
///
///What is most surprising is that the important mathematicaI constant,
///e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
///
///The first ten terms in the sequence of convergents for e are:
///2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
///
///The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
///
///Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
//-----------------------------------------------------------------------
// <copyright file="convergent.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics

let cde = (-2,0I) |> Seq.unfold(fun (n,s) -> 
                                match n with 
                                    | -2              -> Some(2I,(n+1,0I))
                                    | -1              -> Some(1I,(n+1,0I))
                                    | _  when n%3 = 0 -> Some(s+2I,(n+1,s+2I))
                                    | _               -> Some(1I,(n+1,s)))

let fre = ((0I,0I),(0,0I,0I)) |> Seq.unfold(fun ((pn1,qn1),(next,pn2,qn2)) -> 
                                    match next with
                                        | 0 -> let a0 = cde |> Seq.head
                                               let p0 = a0
                                               let q0 = 1I
                                               Some((p0,q0),((p0,q0),(next+1,0I,0I)))
                                        | 1 -> let a0 = cde |> Seq.head
                                               let a1 = cde |> Seq.nth 1
                                               let p1 = a0*a1+1I
                                               let q1 = a1
                                               Some((p1,q1),((p1,q1),(next+1,pn1,qn1)))
                                        | _ -> let an = cde |> Seq.nth next
                                               let pn = an*pn1 + pn2
                                               let qn = an*qn1 + qn2
                                               Some((pn,qn),((pn,qn),(next+1,pn1,qn1)))
                                        )

let numerator = fre |> Seq.nth 99 |> fst
let solution = numerator.ToString().ToCharArray() |> Array.map(fun c -> int c - int '0') |> Array.sum
solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore