///Project Euler Problem 87
///The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. 
///In fact, there are exactly four numbers below fifty that can be expressed in such a way:
///
///                          28 = 2^2 + 2^3 + 2^4
///                          33 = 3^2 + 2^3 + 2^4
///                          49 = 5^2 + 2^3 + 2^4
///                          47 = 2^2 + 3^3 + 2^4
///
///How many numbers below fifty million can be expressed as the sum of a prime square, prime 
///cube, and prime fourth power?
#light

// open some standard namespaces
open System
open System.Collections.Generic

let primes limit =
    [|
        // First prime
        yield 2
        
        let knownComposites = new HashSet<int>()

        // Loop through all odd numbers; evens can't be prime
        for i in 3 .. 2 .. limit do
        
            // Check if its in our list, if not, its prime
            let found = knownComposites.Contains(i)
            if not found then
                yield i
            // Add all multiples of i to our sieve, starting
            // at i and irecementing by i.
            do for j in i .. i .. limit do
                knownComposites.Add(j) |> ignore
            
   |]
printfn "done with primes"
let limit = 50000000

let solution = 
    let table = new HashSet<_>()
    for a in primes 7072 do // limit^(1/4)
        for b in primes 369 do // limit^(1/3)
            for c in primes 84 do // limit^(1/2)
                let num = (a*a) + (b*b*b) + (c*c*c*c)
                if  num < limit && not (table.Contains(num)) then
                    table.Add(num) |> ignore
    table.Count
                    
solution |> printfn "solution: %d"
Console.ReadKey(true) |> ignore
