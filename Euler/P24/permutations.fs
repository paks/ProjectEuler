///Project Euler Problem 23
///A permutation is an ordered arrangement of objects. For example, 3124 is one possible 
///permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or 
///alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
///
///                            012   021   102   120   201   210
///
///What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
// the solution for this problem came from here: http://blogs.msdn.com/b/chrsmith/archive/2008/03/22/project-euler-problem-24.aspx
open System
open System.Collections.Generic

let rec fact x = if x <= 0 then 1 else x * fact (x - 1)

let rec LexoPermutation (digits:List<int>) target =
    if digits.Count = 1 then
        [digits.[0]]
    else
        let bucketSize = (fact digits.Count) / digits.Count
        let digitToSplitIdx = target / bucketSize
        let digitSplit = digits.[digitToSplitIdx]
        digits.RemoveAt(digitToSplitIdx)
        digitSplit :: (LexoPermutation digits (target - bucketSize * digitToSplitIdx))

let digits = new List<int>([0..9])
let permutation = LexoPermutation digits (1000000 - 1)

let result = new String(permutation |> List.map (fun n -> char (n + 0x30)) |> Array.ofList)

printfn "solution: %s" result
Console.ReadKey(true) |> ignore