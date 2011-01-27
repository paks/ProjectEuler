#r "Primegen.dll"
#load "euler2.fs"

open Euler2

let phi (n :int) =
    let factors = primeFactorsSeq (float n) |> Seq.distinct |> Seq.map int
    let nu,de = factors |> Seq.fold(fun (an,ad) p -> (an*(p-1),ad*p)) (1,1)
    (n * nu)/de
