///Project Euler Problem 96
///Su Doku (Japanese meaning number place) is the name given to a popular puzzle concept. Its 
///origin is unclear, but credit must be attributed to Leonhard Euler who invented a similar, and 
///much more difficult, puzzle idea called Latin Squares. The objective of Su Doku puzzles, 
///however, is to replace the blanks (or zeros) in a 9 by 9 grid in such that each row, column, and 3 
///by 3 box contains each of the digits 1 to 9. Below is an example of a typical starting puzzle grid 
///and its solution grid.
///
///                    0 0 3  0 2 0  6 0 0         4 8 3  9 2 1  6 5 7
///                    9 0 0  3 0 5  0 0 1         9 6 7  3 4 5  8 2 1
///                    0 0 1  8 0 6  4 0 0         2 5 1  8 7 6  4 9 3
///
///                    0 0 8  1 0 2  9 0 0         5 4 8  1 3 2  9 7 6
///                    7 0 0  0 0 0  0 0 8         7 2 9  5 6 4  1 3 8
///                    0 0 6  7 0 8  2 0 0         1 3 6  7 9 8  2 4 5
///
///                    0 0 2  6 0 9  5 0 0         3 7 2  6 8 9  5 1 4
///                    8 0 0  2 0 3  0 0 9         8 1 4  2 5 3  7 6 9
///                    0 0 5  0 1 0  3 0 0         6 9 5  4 1 7  3 8 2
///
///A well constructed Su Doku puzzle has a unique solution and can be solved by logic, although it 
///may be necessary to employ "guess and test" methods in order to eliminate options (there is 
///much contested opinion over this). The complexity of the search determines the difficulty of 
///the puzzle; the example above is considered easy because it can be solved by straight forward 
///direct deduction.
///
///The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'), contains fifty different Su 
///Doku puzzles ranging in difficulty, but all with unique solutions (the first puzzle in the file is the 
///example above).
///
///By solving all fifty puzzles find the sum of the 3-digit numbers found in the top left corner of 
///each solution grid; for example, 483 is the 3-digit number found in the top left corner of the solution grid above.
// The sudoku solver came from here: http://www.ffconsultancy.com/dotnet/fsharp/sudoku/index.html
open System
open System.IO

let size = 3

let size2 = size * size

// Check if "n" is valid at position "x", "y" on the board "m", starting with "i=0"
let rec invalid (m :int array array) i x y n =
  i < size2 && (m.[y].[i] = n || m.[i].[x] = n ||
      m.[y / size * size + i / size].[x / size * size + i % size] = n || invalid m (i + 1) x y n)

// Recursively search every valid entry at every non-empty puzzle position
let rec search x y f accu (m :int array array) = 
    match x, y with
      | x, y when x = size2 -> search 0 (y + 1) f accu m
      | 0, y when y = size2 -> f accu
      | x, y when m.[y].[x] <> 0 -> search (x + 1) y f accu m
      | x, y ->
          let aux accu n =
            if invalid m 0 x y n then accu else
              (m.[y].[x] <- n;
               let accu = search (x + 1) y f accu m in
               m.[y].[x] <- 0;
               accu) in
          Seq.fold aux accu (Seq.init 9 (fun i -> i + 1))

// Input puzzle
let example = [|[|0;0;3;0;2;0;6;0;0|];
                [|9;0;0;3;0;5;0;0;1|];
                [|0;0;1;8;0;6;4;0;0|];
                [|0;0;8;1;0;2;9;0;0|];
                [|7;0;0;0;0;0;0;0;8|];
                [|0;0;6;7;0;8;2;0;0|];
                [|0;0;2;6;0;9;5;0;0|];
                [|8;0;0;2;0;3;0;0;9|];
                [|0;0;5;0;1;0;3;0;0|]|]


let problems = 
    let flip f x y = f y x
    seq {
        let reader =
            [| use reader = new StreamReader(File.OpenRead("sudoku.txt"))
               while not reader.EndOfStream do
                    let line = reader.ReadLine()
                    if not (line |> String.exists(fun ch -> ch = 'G')) then
                        yield line.ToCharArray() |> Array.map(fun c -> int c - int '0') |]
        for i in 0 .. 9 .. reader.Length - 1 do
            let puzzle = Array.create size2 [||]
            for j in 0 .. 8 do 
                do puzzle.[j] <- reader.[i+j]
            yield puzzle
    }

let puzzle (arr :int array array) = Array.init size2 (fun y -> Array.init size2 (fun x -> try arr.[y].[x] with _ -> 0))
exception Solution of int
let sum = ref 0

for problem in problems do
    let m = puzzle problem
    try
        let marker (m :int array array) = m.[0].[0]*100 + m.[0].[1]*10 + m.[0].[2]
        search 0 0 (fun s -> raise(Solution(marker m)) ) () m;
    with
        | Solution(n) -> sum := !sum + n

!sum |> printfn "solution: %d"
Console.ReadKey(true) |> ignore