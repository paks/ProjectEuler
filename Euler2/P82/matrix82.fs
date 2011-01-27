///Project Euler Problem 82
///NOTE: This problem is a more challenging version of Problem 81.
///
///The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the left column and 
///finishing in any cell in the right column, and only moving up, down, and right, is indicated in 
///red; the sum is equal to 994.
///
///                             131  673  (234) (103)  (18)
///                            (201) (96) (342)  965   150
///                             630  803   746   422   111
///                             537  699   497   121   956
///                             805  732   524    37   331 
///	
///Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'), a 31K text file 
///containing a 80 by 80 matrix, from the left column to the right column.
//-----------------------------------------------------------------------
// <copyright file="matrix82.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO
open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Algorithms.Observers
open QuickGraph.Algorithms.ShortestPath

let M = 
    let reader = File.ReadAllLines("matrix.txt") |> Seq.map(fun line -> line.Split ',' |> Seq.map float)
    matrix reader

let directions = [(1,0);(-1,0);(0,1)]

let XcostMatrix = matrix [[ 131.; 673.; 234.; 103.;  18. ];
                         [ 201.;  96.; 342.; 965.; 150. ];
                         [ 630.; 803.; 746.; 422.; 111. ];
                         [ 537.; 699.; 497.; 121.; 956. ];
                         [ 805.; 732.; 524.;  37.; 331. ]; ]


let edgeCost (m: matrix) (e :Edge<int * int>) =
    let l,w = m.Dimensions
    let isOutOfRange (a,b) = a < 0 || b < 0 || a = l || b = w
    if  e.Target |> isOutOfRange then
        Double.PositiveInfinity
    else
        let a,b = e.Target
        m.[a,b]

let pathCost (m: matrix) (e :Edge<int * int>) =
    let l,w = m.Dimensions
    let a,b = e.Source
    m.[a,b]

let buildGraph (m: matrix) = 
    let add (a,b) (c,d) = (a+c,b+d)
    let graph = new AdjacencyGraph<int * int, Edge<int * int>>()
    let l,w = m.Dimensions
    for i in 0 .. l - 1 do
        for j in 0 .. w - 1 do
            let s = i,j
            let dest = directions |> Seq.map(add s)
            dest |> Seq.iter(fun d -> let e = new Edge<int * int>(s,d)
                                      graph.AddVerticesAndEdge(e) |> ignore )
    graph
                


let distance (m: matrix) (path: #Edge<int * int> list) = 
    let rec distance' (path: #Edge<int * int> list) d =
        match path with
            | [] -> d
            | [e] ->
                    //printfn "end: %A" e.Target
                    d + pathCost m e + (let a,b = e.Target in m.[a,b])
            | e::es -> 
                    let d' = d + pathCost m e
                    distance' es d'
    //printf "start: %A" (path |> List.head).Source
    distance' path 0.


let paths (m: matrix) = 
    let graph = buildGraph m
    let l,w = m.Dimensions
    let ec = new Func<Edge<int * int>, float>(edgeCost m)
    seq {
        for i in 0 .. l - 1 do
            let dijkstra = new DijkstraShortestPathAlgorithm<_,_>(graph,ec)
            let predecessorObserver = new VertexPredecessorRecorderObserver<int * int,Edge<int * int>>()
            using (ObserverScope.Create<ITreeBuilderAlgorithm<_, _>>(dijkstra,predecessorObserver)) (fun n -> dijkstra.Compute((i,0)))
            for ii in 0 .. l - 1 do
                let path = predecessorObserver.Path(ii,w - 1) |> List.ofSeq
                let dist = distance m path
                //printfn "source: %d,%d, destination: %d, %d , value: %f" i 0 ii (w - 1) dist
                yield dist,path

            
    }

let solution, path = paths M |> Seq.minBy fst
solution |> printfn "solution: %.0f"
Console.ReadKey(true) |> ignore