///Project Euler Problem 18
///By starting at the top of the triangle below and moving to adjacent numbers on the row below, 
///the maximum total from top to bottom is 23.
///
///                                                   3'
///                                                  7' 5
///                                                 2 4' 6
///                                                8 5 9' 3
///
///That is, 3 + 7 + 4 + 9 = 23.
///
///Find the maximum total from top to bottom of the triangle below:
///
///                       75
///                       95 64
///                       17 47 82
///                       18 35 87 10
///                       20 04 82 47 65
///                       19 01 23 75 03 34
///                       88 02 77 73 07 63 67
///                       99 65 04 28 06 16 70 92
///                       41 41 26 56 83 40 80 70 33
///                       41 48 72 33 47 32 37 16 94 29
///                       53 71 44 65 25 43 91 52 97 51 14
///                       70 11 33 28 77 73 17 78 39 68 17 57
///                       91 71 52 38 17 14 91 43 58 50 27 29 48
///                       63 66 04 68 89 53 67 30 73 16 69 87 40 31
///                       04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
///
///NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. 
///However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot 
//be solved by brute force, and requires a clever method! ;o)
//-----------------------------------------------------------------------
// <copyright file="triangle.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

let triangle = [| [| 75|];
                  [| 95;64|];
                  [| 17;47;82|];
                  [| 18;35;87;10|];
                  [| 20;04;82;47;65|];
                  [| 19;01;23;75;03;34|];
                  [| 88;02;77;73;07;63;67|];
                  [| 99;65;04;28;06;16;70;92|];
                  [| 41;41;26;56;83;40;80;70;33|];
                  [| 41;48;72;33;47;32;37;16;94;29|];
                  [| 53;71;44;65;25;43;91;52;97;51;14|];
                  [| 70;11;33;28;77;73;17;78;39;68;17;57|];
                  [| 91;71;52;38;17;14;91;43;58;50;27;29;48|];
                  [| 63;66;04;68;89;53;67;30;73;16;69;87;40;31|];
                  [| 04;62;98;27;23;09;70;98;73;93;38;53;60;04;23|] |]
(*
let triangle = [| [| 03|];
                  [| 07;05|];
                  [| 02;04;06|];
                  [| 08;05;09;03|] |]
*)

// Brute force solution :)
type Vertex = int * int
type Edge = Vertex * Vertex * int
type Graph = { vertices: Vertex list; edges: Edge list }

let rec searchAll (g: Graph) (src: Vertex) (dst: Vertex) = 
                    let rec search' = function
                        | [] -> []
                        | (u,v,_) :: es -> match src with
                                            | _ when src = u -> List.map (fun t -> u::t ) (searchAll g v dst) @ search' es
                                            | _ -> search' es
                    match src with
                        | _ when src = dst -> [[src]]
                        | _ -> search' g.edges


let vertices :Vertex list = triangle |> Array.fold (fun acc a -> acc @ (a |> Array.toList) ) [] |> List.mapi (fun i v -> (i,v))
let arrVer = vertices |> List.toArray

let dstSrt  = function 
    | 0  -> 0
    | n  -> [0 .. n - 1] |> Seq.fold (fun acc n -> triangle.[n].Length + acc) 0

let edges : Edge list = 
    [
        for i in 0 .. triangle.Length - 2 do
            let row = triangle.[i]
            for j in 0 .. row.Length - 1 do
                let n = dstSrt i + j
                let d = dstSrt (i+1) + j
                let src = arrVer.[n]
                let dst1 = arrVer.[d]
                let dst2 = arrVer.[d + 1]
                yield [(src,dst1);(src,dst2)]
    ] |> List.fold (fun acc l -> acc @ l ) [] |> List.mapi (fun i (v1,v2) -> (v1,v2,i))
    
    
let graph = { vertices= vertices; edges = edges }

let max (l : Vertex list list) = l |> Seq.map (fun l -> (l |> Seq.fold (fun acc (_,id) -> acc + id) 0 ), l) |> Seq.fold (fun (a, al) (n,l) -> if n > a then (n,l) else (a,al)) (0,[])

//let src = (0,3)
//let destinations = [(6,08);(7,05);(8,09);(9,03)]
let src = (0,75)
let destinations = [(105,04);(106,62);(107,98);(108,27);(109,23);(110,09);(111,70);(112,98);(113,73);(114,93);(115,38);(116,53);(117,60);(118,04);(119,23)]



let result = destinations |> Seq.map (fun dst -> searchAll graph src dst |> max) |> Seq.maxBy fst

result |> fst |> printfn "solution: %d"

Console.ReadKey(true) |> ignore