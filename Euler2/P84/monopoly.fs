///Project Euler Problem 84
///In the game, Monopoly, the standard board is set up in the following way:
///
///         GO  A1  CC1 A2  T1  R1  B1  CH1 B2  B3  JAIL
///         H2                                      C1
///         T2                                      U1
///         H1                                      C2
///         CH3                                     C3
///         R4                                      R2
///         G3                                      D1
///         CC3                                     CC2
///         G2                                      D2
///         G1                                      D3
///         G2J F3  U2  F2  F1  R3  E3  E2  CH2 E1  FP
///
///A player starts on the GO square and adds the scores on two 6-sided dice to determine the 
///number of squares they advance in a clockwise direction. Without any further rules we would 
///expect to visit each square with equal probability: 2.5%. However, landing on G2J (Go To Jail), 
///CC (community chest), and CH (chance) changes this distribution.
///
///In addition to G2J, and one card from each of CC and CH, that orders the player to go to 
///directly jail, if a player rolls three consecutive doubles, they do not advance the result of their 
///3rd roll. Instead they proceed directly to jail.
///
///At the beginning of the game, the CC and CH cards are shuffled. When a player lands on CC or 
///CH they take a card from the top of the respective pile and, after following the instructions, it 
///is returned to the bottom of the pile. There are sixteen cards in each pile, but for the purpose 
///of this problem we are only concerned with cards that order a movement; any instruction not 
///concerned with movement will be ignored and the player will remain on the CC/CH square.
///
///    * Community Chest (2/16 cards):
///         1. Advance to GO
///         2. Go to JAIL
///    * Chance (10/16 cards):
///         1. Advance to GO
///         2. Go to JAIL
///         3. Go to C1
///         4. Go to E3
///         5. Go to H2
///         6. Go to R1
///         7. Go to next R (railway company)
///         8. Go to next R
///         9. Go to next U (utility company)
///        10. Go back 3 squares.
///
///The heart of this problem concerns the likelihood of visiting a particular square. That is, the 
///probability of finishing at that square after a roll. For this reason it should be clear that, with 
///the exception of G2J for which the probability of finishing on it is zero, the CH squares will 
///have the lowest probabilities, as 5/8 request a movement to another square, and it is the final 
///square that the player finishes at on each roll that we are interested in. We shall make no 
///distinction between "Just Visiting" and being sent to JAIL, and we shall also ignore the rule about 
///requiring a double to "get out of jail", assuming that they pay to get out on their next turn.
///
///By starting at GO and numbering the squares sequentially from 00 to 39 we can concatenate these 
///two-digit numbers to produce strings that correspond with sets of squares.
///
///Statistically it can be shown that the three most popular squares, in order, are JAIL (6.24%) = 
///Square 10, E3 (3.18%) = Square 24, and GO (3.09%) = Square 00. So these three most popular 
///squares can be listed with the six-digit modal string: 102400.
///
///If, instead of using two 6-sided dice, two 4-sided dice are used, find the six-digit modal string.
//-----------------------------------------------------------------------
// <copyright file="monopoly.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System

type Board = 
    | GO  | A1 | CC1 | A2  | T1 | R1 | B1  | CH1 | B2 | B3 
    | JAIL | C1 | U1  | C2  | C3 | R2 | D1  | CC2 | D2 | D3 
    | FP   | E1 | CH2 | E2  | E3 | R3 | F1  | F2  | U2 | F3 
    | G2J  | G1 | G2  | CC3 | G3 | R4 | CH3 | H1  | T2 | H2
    static member private array =  
       [| GO  ; A1 ; CC1 ; A2  ; T1 ; R1 ; B1  ; CH1 ; B2 ; B3 
        ; JAIL ; C1 ; U1  ; C2  ; C3 ; R2 ; D1  ; CC2 ; D2 ; D3 
        ; FP   ; E1 ; CH2 ; E2  ; E3 ; R3 ; F1  ; F2  ; U2 ; F3 
        ; G2J  ; G1 ; G2  ; CC3 ; G3 ; R4 ; CH3 ; H1  ; T2 ; H2 |]
    static member ToIntBoard n = 
        Board.array.[n % Board.array.Length]
    static member op_Explicit b = 
        Array.FindIndex (Board.array,(fun n -> n = b))
    static member (+) (b: Board, n :int) = 
        let value = int b
        let i = (value + n) % Board.array.Length
        Board.array.[i]
    member b.ToStringNum = 
        int b |> sprintf "%02d"
        
type Cards =
    | GOTO_GO | GOTO_JAIL | GOTO_C1 | GOTO_E3 | GOTO_H2 | GOTO_R1 | GOTO_NEXTR | GOTO_NEXTU | BACK3 | OTHER

let comunityChestDeck = [GOTO_GO; GOTO_JAIL; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER]

let chanceDeck = [GOTO_GO; GOTO_JAIL; GOTO_C1; GOTO_E3; GOTO_H2; GOTO_R1; GOTO_NEXTR; GOTO_NEXTU; BACK3; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER; OTHER]    

let cardToPosition card position =
    match card,position with
        | GOTO_GO,_ -> GO
        | GOTO_JAIL,_ -> JAIL
        | GOTO_C1,_ -> C1
        | GOTO_E3,_ -> E3
        | GOTO_H2,_ -> H2
        | GOTO_R1,_ -> R1
        | GOTO_NEXTR,CH1 -> R2
        | GOTO_NEXTR,CH2 -> R3
        | GOTO_NEXTR,CH3 -> R1
        | GOTO_NEXTU,CH1 -> U1
        | GOTO_NEXTU,CH2 -> U2
        | GOTO_NEXTU,CH3 -> U1
        | BACK3,CH1      -> T1
        | BACK3,CH2      -> D3
        | BACK3,CH3      -> CC3
        | OTHER,_      -> position
        | _              -> failwith ("option now allowed " + card.ToString() + ", " + position.ToString())
         
let getCard deck = 
    let card = List.head deck
    (card , (List.tail deck) @ [card])
        
type Dice(faces: int) =
    let faces = faces
    let rnd = new Random();
    member d.NextRoll = rnd.Next(faces) + 1
    
type Roll = { Result: int ; IsDouble: bool }

let rolldices (dice: Dice) = 
    let isDouble (a, b) = a = b
    let result = (dice.NextRoll,dice.NextRoll)
    { Result = fst result + snd result ; IsDouble = isDouble(result) }

type GameState =
    {position: Board; ccDeck: Cards list; chDeck: Cards list; doublesInARow: int }
    member gs.nextTurn(roll: Roll) = 
        let doubles = if roll.IsDouble then gs.doublesInARow + 1 else 0
        if doubles = 3 then
            {position = JAIL; ccDeck = gs.ccDeck; chDeck = gs.chDeck; doublesInARow = 0 }
        else
            let pos = gs.position + roll.Result 
            match pos with
                | CC1 | CC2 | CC3 -> 
                         let card,deck = getCard gs.ccDeck
                         let nextPosition = cardToPosition card pos
                         {position = nextPosition; ccDeck = deck; chDeck = gs.chDeck; doublesInARow = doubles }
                | CH1 | CH2 | CH3 -> 
                         let card,deck = getCard gs.chDeck
                         let next = cardToPosition card pos
                         if next = CC3 then //pick another card if we landed on CC3
                            let card,ccDeck = getCard gs.ccDeck
                            let ccPosition = cardToPosition card CC3
                            {position = ccPosition; ccDeck = ccDeck; chDeck = deck; doublesInARow = doubles }                                     
                         else
                            {position = next; ccDeck = gs.ccDeck; chDeck = deck; doublesInARow = doubles }
                | G2J -> {position = JAIL; ccDeck = gs.ccDeck; chDeck = gs.chDeck; doublesInARow = doubles }
                | _   -> {position = pos ; ccDeck = gs.ccDeck; chDeck = gs.chDeck; doublesInARow = doubles }
            
let runGame dice (gs: GameState) attemps = 
    let statistics = Array.create 40 0
    let rec runGame' dice (gs: GameState) attemps = 
        if attemps = 0 then
            List.zip ([0 .. 39] |> List.map(Board.ToIntBoard)) (statistics |> Array.toList) |> List.sortBy snd |> List.rev
        else
            let roll = rolldices dice
            let newGs = gs.nextTurn roll
            let index = int newGs.position
            statistics.[index] <- statistics.[index] + 1
            runGame' dice newGs (attemps - 1)
    runGame' dice gs attemps

// end of setup


let faces = 4

let dice = new Dice(faces)
let attemps = int 5E5
let initialGameState = {position = GO; ccDeck = comunityChestDeck; chDeck = chanceDeck; doublesInARow = 0 }

let statistics = runGame dice initialGameState attemps

//statistics |> Seq.iter(fun (b,n) -> printfn "%A %f" b ((float n * 100.)/2E6))

let solution = statistics |> Seq.take 3 |> Seq.map (fun (b,_) -> b.ToStringNum) |> String.Concat

solution |> printfn "solution: %s"
Console.ReadKey(true) |> ignore