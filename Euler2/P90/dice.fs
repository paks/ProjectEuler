///Project Euler Problem 85
///Each of the six faces on a cube has a different digit (0 to 9) written on it; the same is done to a 
///second cube. By placing the two cubes side-by-side in different positions we can form a variety 
///of 2-digit numbers.
///
///For example, the square number 64 could be formed:
///
///                         --- ---
///                         |6| |4|
///                         --- ---
///
///In fact, by carefully choosing the digits on both cubes it is possible to display all of the square 
///numbers below one-hundred: 01, 04, 09, 16, 25, 36, 49, 64, and 81.
///
///For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9} on one cube and {1, 2, 
///3, 4, 8, 9} on the other cube.
///
///However, for this problem we shall allow the 6 or 9 to be turned upside-down so that an 
///arrangement like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows for all nine square numbers to be 
///displayed; otherwise it would be impossible to obtain 09.
///
///In determining a distinct arrangement we are interested in the digits on each cube, not the order.
///
///            {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}
///            {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}
///
///But because we are allowing 6 and 9 to be reversed, the two distinct sets in the last example 
///both represent the extended set {1, 2, 3, 4, 5, 6, 9} for the purpose of forming 2-digit numbers.
///
///How many distinct arrangements of the two cubes allow for all of the square numbers to be displayed?
//-----------------------------------------------------------------------
// <copyright file="dice.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System


let cube = 
    let n = 10
    let r = 6
    let total = 209
    let digits = [|'0';'1';'2';'3';'4';'5';'P';'7';'8';'P'|]
    let a = [|0..5|];
    [
        yield digits.[0..r-1] |> Set.ofArray
        for x in 1 .. total do
            let i = ref (r - 1)
            while a.[!i] = (n - r + !i) do
                decr i
            a.[!i] <- a.[!i] + 1
            for j in (!i + 1) .. r - 1 do
                a.[j] <- a.[!i] + j - !i
            yield set [digits.[a.[0]];digits.[a.[1]];digits.[a.[2]];digits.[a.[3]];digits.[a.[4]];digits.[a.[5]]]
    ]

//cube |> Seq.map(fun s -> new String(s |> Set.toArray)) |> Seq.iter(printfn "%s")
let dices =
    seq {
        //let dc = cube
        for da in cube do
            for db in cube do
                yield da,db
    }
    
let isSquare ((A: Set<_>) ,(B :Set<_>)) =
    let squares = ['0','1';'0','4';'0','P';'1','P';'2','5';'3','P';'4','P';'P','4';'8','1']
    let rec isSquare' sq = 
        match sq with
            | [] -> true
            | s::ss -> 
                let c,d = s
                if (A.Contains c && B.Contains d) || (A.Contains d && B.Contains c) then
                   isSquare' ss
                else
                    false

                        
    isSquare' squares

//isSquare ((set ['0'; '5'; 'P'; '7'; '8'; '1']), (set ['1'; '2'; '3'; '4'; 'P'; '7'])) |> printfn "%A"

let solution = 
    let total = dices |> Seq.filter(isSquare) |> Seq.length
    total/2 //remove duplicates
            
solution |> printfn "solution: %d"            
Console.ReadKey(true) |> ignore

