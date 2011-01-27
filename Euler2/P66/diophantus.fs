///Project Euler Problem 66
///Consider quadratic Diophantine equations of the form:
///
///                       x^2 - Dy^2 = 1
///
///For example, when D=13, the minimal solution in x is 6492 – 131802 = 1.
///
///It can be assumed that there are no solutions in positive integers when D is square.
///
///By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
///
///3^2 - 2x2^2 = 1
///2^2 - 3x1^2 = 1
///9^2 - 5x4^2 = 1
///5^2 - 6x2^2 = 1
///8^2 - 7x3^2 = 1
///
///Hence, by considering minimal solutions in x for D 7, the largest x is obtained when D=5.
///
///Find the value of D 1000 in minimal solutions of x for which the largest value of x is obtained.
//-----------------------------------------------------------------------
// <copyright file="diophantus.fs" >
// Copyright © 2011 Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics

///cf n = sqrt(n) = [|a0;a1;a2;....;2a0|]
///see: see http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
let cf (n :int) = 
        (bigint n,(0I,0I,0I,0)) 
            |> Seq.unfold(fun (s,(a,m,d,a0)) ->
                            match a0 with
                                | -1 -> None
                                | 0 ->  let a01 = bigint(int (sqrt (float n)))
                                        let m0 = 0I
                                        let d0 = 1I
                                        Some(a01,(s,(a01,m0,d0,int a01)))
                                | _ ->  let mn = d*a-m
                                        let ab0 = bigint a0
                                        let dn = (s - (mn*mn)) / d
                                        let an = (ab0+mn)/dn
                                        if an <> 2I*ab0 then
                                            Some(an,(s,(an,mn,dn,a0)))
                                        else
                                            Some(an,(s,(an,mn,dn,-1)))) |> Seq.toArray

///see: http://mathworld.wolfram.com/PellEquation.html equations (8) to (14)
let fre (f :bigint array) = 
        ((0I,0I),(0,0I,0I)) 
               |> Seq.unfold(fun ((pn1,qn1),(next,pn2,qn2)) -> 
                                match next with
                                    | 0 -> let a0 = f.[0]
                                           let p0 = a0
                                           let q0 = 1I
                                           Some((p0,q0),((p0,q0),(next+1,0I,0I)))
                                    | 1 -> let a0 = f.[0]
                                           let a1 = f.[1]
                                           let p1 = a0*a1+1I
                                           let q1 = a1
                                           Some((p1,q1),((p1,q1),(next+1,pn1,qn1)))
                                    | _ -> 
                                           let length = f |> Array.length 
                                           let nxt = (next - 1) % (length - 1)
                                           let an = f.[nxt + 1]
                                           let pn = an*pn1 + pn2
                                           let qn = an*qn1 + qn2
                                           Some((pn,qn),((pn,qn),(next+1,pn1,qn1)))
                                    )

let limit = 1000
let isNotSquare =
    let table = let arr = Array.create (limit+1) true
                let sqrtLimit = int (sqrt (float limit))
                for n in 2 .. sqrtLimit do
                    arr.[n*n] <- false
                arr
    fun n -> table.[n]

let isOdd n = n &&& 1 = 1

let solutions =
    let candidates = seq { 2 .. limit } |> Seq.filter isNotSquare
    seq {
        for d in candidates do
            let r = (cf d |> Array.length) - 2
            if r |> isOdd then 
                yield (d,fre (cf d) |> Seq.nth r)
            else
                yield (d,fre (cf d) |> Seq.nth (2*r+1))
    }

let solution = solutions |> Seq.maxBy(snd >> fst) |> fst
solution |> printfn "solution: %d"
Console.ReadKey(true) |> ignore