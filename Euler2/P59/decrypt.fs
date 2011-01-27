///Project Euler Problem 59
///Each character on a computer is assigned a unique code and the preferred standard is ASCII 
///(American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk 
///(*) = 42, and lowercase k = 107.
///
///A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each 
///byte with a given value, taken from a secret key. The advantage with the XOR function is that 
///using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 
///42 = 107, then 107 XOR 42 = 65.
///
///For unbreakable encryption, the key is the same length as the plain text message, and the key is 
///made up of random bytes. The user would keep the encrypted message and the encryption key 
///in different locations, and without both "halves", it is impossible to decrypt the message.
///
///Unfortunately, this method is impractical for most users, so the modified method is to use a 
///password as a key. If the password is shorter than the message, which is likely, the key is 
///repeated cyclically throughout the message. The balance for this method is using a sufficiently 
///long password key for security, but short enough to be memorable.
///
///Your task has been made easy, as the encryption key consists of three lower case characters. 
///Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII 
///codes, and the knowledge that the plain text must contain common English words, decrypt the 
///message and find the sum of the ASCII values in the original text.
//-----------------------------------------------------------------------
// <copyright file="diagonal.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// http://www.kitiara.org
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO
open Euler2

let (^^^) (c :char) (c1 :char) = char (int c ^^^ int c1)

let key = Array.create 3 (char 0)

let cipherText = 
    let text = File.ReadAllText(@"cipher1.txt").Split ',' |> Array.toList
    text |> List.map(int >> char)

let getColumn col = List.mapi(curry id) >> List.filter(fst >> flip (%) 3 >> (=) col) >> List.map snd
let firstcol,secondcol,thirdcol = 
    cipherText 
    |> List.mapi(curry id) 
    |> List.fold(fun (f,s,t) (i,c) -> 
        match i % 3 with
            | 0 -> (c::f,s,t)
            | 1 -> (f,c::s,t)
            | 2 -> (f,s,c::t)
            | _ -> failwith "imposile!") ([],[],[])

// Lets do a frecuency analisys
// http://en.wikipedia.org/wiki/Frequency_analysis
let analisys = Seq.countBy id  >> List.ofSeq >> List.sortBy snd >> List.rev

//The most common character is the space ' '
//with luck it would be the most common in all colunms
//If not use the Letter Frequency table here: http://en.wikipedia.org/wiki/Letter_frequency
let mostCommomCharacter = ' '

let firstCandidate = analisys firstcol |> List.head |> fst
key.[0] <- firstCandidate ^^^ mostCommomCharacter

let secondCandidate = analisys secondcol |> List.head |> fst
key.[1] <- secondCandidate  ^^^ mostCommomCharacter

let thirdCandidate = analisys thirdcol |> List.head |> fst
key.[2] <- thirdCandidate ^^^ mostCommomCharacter

let crypt (text : char list) (k :char array) = 
    let length = k.Length 
    text 
    |> List.mapi(fun i c ->
        let m = i % length
        c ^^^ k.[m])
                                            

let plainTextList = crypt cipherText key

let plainText = new String(plainTextList |>List.toArray)

new String(key) |> printfn "key: %s"
plainText |> printfn "cleartext:\n %s\n"
let solution = plainTextList |> Seq.fold(fun acc c -> acc + int c ) 0
printfn "solution: %d" solution
Console.ReadKey(true) |> ignore
