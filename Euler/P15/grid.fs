///Project Euler Problem 11
///Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.
///
///            0 1 2
///            3 4 5
///            6 7 8
///
///How many routes are there through a 20x20 grid?
//-----------------------------------------------------------------------
// <copyright file="grid.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics

let gridSize = 20I

let factorial n = [2I .. n] |> List.fold ( * ) 1I
    
let teoNumRoutes n = factorial (2I*n) / (factorial n * factorial n)

teoNumRoutes gridSize |> printfn "solution: %A"

Console.ReadKey(true) |> ignore

//type Vertex = int * string
//type Edge = Vertex * Vertex * string
//type Graph = { vertices: Vertex list; edges: Edge list }
//
//let simpleGraph = { vertices = [(0,"a");(1,"b");(2,"c");(3,"d")]; edges = [((0,"a"),(1,"b"),"l");((0,"a"),(2,"c"),"m");((1,"b"),(3,"d"),"n");((2,"c"),(3,"d"),"o");] }
//let a = (0,"a")
//let b = (1,"b")
//let c = (2,"c")
//let d = (3,"d")
//let e = (4,"e")
//let f = (5,"f")
//let g = (6,"g")
//let h = (7,"h")
//let i = (8,"i")
//let grid2x2 = { vertices = [a;b;c;d;e;f;g;h;i ]; edges = 
//                    [
//                        (a,b,"0");
//                        (a,d,"1");
//                        (b,c,"2");
//                        (b,e,"3");
//                        (c,f,"4");
//                        (d,e,"5");
//                        (d,g,"6");
//                        (e,f,"7");
//                        (e,h,"8");
//                        (f,i,"9");
//                        (g,h,"10");
//                        (h,i,"11");
//                    ]}
//
//let cols = gridSize + 1
//let rows = gridSize + 1
//let size = rows * cols
//
//let leftVertice (n : int , id : string) = if n % cols <> (cols - 1) then Some ((n+1),(n+1).ToString()) else None
//let downVertice (n : int , id : string) = if n < (size - cols) then Some ((n+rows),(n+rows).ToString()) else None
//
//let getEdges v = 
//            let left = leftVertice v
//            let down = downVertice v
//            match left with
//                | Some l -> match down with
//                                | Some d -> [(v,l,(snd v) + (snd l)); (v,d,(snd v) + (snd d))]
//                                | None   -> [(v,l,(snd v) + (snd l))]
//                | None   -> match down with
//                                | Some d -> [(v,d,(snd v) + (snd d))]
//                                | None   -> []
//
//let vertices = List.map (fun (n: int) -> (n , n.ToString())) [0 .. size - 1]
//
//let edges = List.fold (fun acc v -> 
//                    let edges = getEdges v
//                    edges @ acc
//                        ) [] vertices
//
//let grid = { vertices = vertices ; edges = edges }
//
//
//let rec search (g: Graph) (src: Vertex) (dst: Vertex) = 
//                    let rec search' = function
//                        | [] -> None
//                        | (u,v,_) :: es -> match src with
//                                            | _ when src = u -> 
//                                                match search g v dst with
//                                                    | Some p -> Some (u::p)
//                                                    | None   -> search' es
//                                            | _ -> search' es
//                    match src with
//                        | _ when src = dst -> Some ([src])
//                        | _ -> search' g.edges
//                         
//let rec searchAll (g: Graph) (src: Vertex) (dst: Vertex) = 
//                    let rec search' = function
//                        | [] -> []
//                        | (u,v,_) :: es -> match src with
//                                            | _ when src = u -> List.map (fun t -> u::t ) (searchAll g v dst) @ search' es
//                                            | _ -> search' es
//                    match src with
//                        | _ when src = dst -> [[src]]
//                        | _ -> search' g.edges
//
//
//
//let src = (0,"0")
//let dst = (size - 1 ,(size - 1).ToString())
//
////search simpleGraph src dst |> printfn "%A"
//if gridSize < 11 then
//    let routes = searchAll grid src dst 
//    //routes |> printfn "%A"
//    let numRoutes = routes.Length
//    numRoutes |> printfn "number of routes %d"
