///Project Euler Problem 57
///In the card game poker, a hand consists of five cards and are ranked, from lowest 
///to highest, in the following way:
///
///    * High Card: Highest value card.
///    * One Pair: Two cards of the same value.
///    * Two Pairs: Two different pairs.
///    * Three of a Kind: Three cards of the same value.
///    * Straight: All cards are consecutive values.
///    * Flush: All cards of the same suit.
///    * Full House: Three of a kind and a pair.
///    * Four of a Kind: Four cards of the same value.
///    * Straight Flush: All cards are consecutive values of same suit.
///    * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
///
///The cards are valued in the order:
///2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
///
///If two players have the same ranked hands then the rank made up of the highest 
///value wins; for example, a pair of eights beats a pair of fives (see example 
///1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.
///
///Consider the following five hands dealt to two players:
///
///      Hand	 	Player 1          Player 2	     Winner
///       1	     5H 5C 6S 7S KD     2C 3S 8S 8D TD   Player 2
///               Pair of Fives     Pair of Eights
///       2	     5D 8C 9S JS AC     2C 5C 7D 8S QH   Player 1
///             Highest card Ace    Highest card Queen
///       3	     2D 9C AS AH AC     3D 6D 7D TD QD   Player 2
///               Three Aces        Flush with Diamonds
///       4	     4D 6S 9H QH QC     3D 6D 7H QD QS   Player 1
///              Pair of Queens     Pair of Queens
///             Highest card Nine   Highest card Seven
///       5	     2H 2D 4C 4D 4S     3C 3D 3S 9S 9D   Player 1
///                Full House       Full House
///              With Three Fours   with Three Threes
///
///The file, poker.txt, contains one-thousand random hands dealt to two players. 
///Each line of the file contains ten cards (separated by a single space): the first 
///five are Player 1's cards and the last five are Player 2's cards. You can assume 
///that all hands are valid (no invalid characters or repeated cards), each player's 
///hand is in no specific order, and in each hand there is a clear winner.
///
///How many hands does Player 1 win?
//-----------------------------------------------------------------------
// <copyright file="poker.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.IO

type Suite = 
    | Hearts
    | Spades
    | Clubs
    | Diamonds
    
type Face = 
    | Number of int
    | Jack
    | Queen
    | King
    | Ace
    static member op_Explicit (f: Face) =
        match f with
            | Number(n) -> n
            | Jack      -> 11
            | Queen     -> 12
            | King      -> 13
            | Ace       -> 14

type Rank =
    | High_Card of Face * Face * Face * Face * Face
    | One_Pair of Face * Face * Face * Face
    | Two_Pairs of Face * Face * Face
    | Three_of_a_Kind of Face * Face * Face
    | Straight of Face
    | Flush of Face * Face * Face * Face * Face
    | Full_House of Face * Face
    | Four_of_a_Kind of Face * Face
    | Straight_Flush of Face
    | Royal_Flush

type Card(suite: Suite, face: Face) = 
    interface IComparable<Card> with
        member card.CompareTo (c :Card) = 
                compare card.Face c.Face
    interface IComparable with
        member card.CompareTo (c :obj) = 
                let ic = unbox<Card> c 
                compare card.Face ic.Face
    member card.Suite = suite
    member card.Face  = face
    override card.Equals(o :obj)  = 
                let c = unbox<Card> o
                card = c
    override card.GetHashCode() = card.GetHashCode()
    static member Parse(s : string) = 
            if s.Length <> 2 then
                failwith ("Error parsing: " + s)
            else
                let parseSuite (c: char) = 
                    match c with
                        | 'H' -> Hearts
                        | 'S' -> Spades
                        | 'C' -> Clubs
                        | 'D' -> Diamonds
                        |  _  -> failwith ("Error parsing " + new String([|c|]))
                let parseFace (c: char) = 
                    match c with
                        | 'A' -> Ace
                        | 'K' -> King
                        | 'Q' -> Queen
                        | 'J' -> Jack
                        | 'T' -> Number(10)
                        |  n  -> let num = Convert.ToInt32(new String([|c|]))
                                 Number(num)
                let c = s.ToCharArray()
                let face = parseFace(c.[0])
                let suite = parseSuite(c.[1])
                Card(suite,face)
    
type Hand(cards : Card list) = 
    member hand.Cards = cards |> List.sortWith(fun a b -> compare b a)
    member hand.Rank = 
                let areSameSuit(h: Hand) = 
                    let firstCard = h.Cards |> List.head
                    h.Cards |> Seq.forall(fun c -> c.Suite = firstCard.Suite)
                let isRoyalFlush (h :Hand) = 
                    if h |> areSameSuit then
                        let cardFaces = h.Cards |> List.map(fun c -> c.Face)
                        let royalFlush = [Ace; King; Queen; Jack; Number(10)]
                        cardFaces = royalFlush
                    else
                        false
                let isStraight(h : Hand) = 
                    let values = h.Cards |> List.map(fun c -> int c.Face) |> List.rev
                    let first = values |> List.head
                    [first .. first+4] = values
                let isStraightFlush (h :Hand) = 
                    if h |> areSameSuit then
                        h |> isStraight
                    else
                        false
                let getFourOfAKind(h : Hand) = 
                    let groups = h.Cards |> Seq.countBy(fun c -> c.Face) |> Seq.toList |> List.sortWith(fun (_,n1) (_,n2) -> compare n2 n1)
                    if groups |> Seq.length = 2 then
                        let group1 = groups |> List.head
                        if group1 |> snd = 4 then
                            let kicker = groups |> Seq.nth 1 |> fst
                            Some(Four_of_a_Kind(fst group1, kicker))
                        else
                            None
                    else
                        None
                let isFourOfAKind(h : Hand) = getFourOfAKind(h) <> None //TODO: rework this
                let getFullHouse(h : Hand) = 
                    let groups = h.Cards |> Seq.countBy(fun c -> c.Face) |> Seq.toList |> List.sortWith(fun (_,n1) (_,n2) -> compare n2 n1)
                    if groups |> Seq.length = 2 then
                        let trio = groups |> List.head
                        if trio |> snd = 3 then
                            let pair = groups |> Seq.nth 1 |> fst
                            Some(Full_House(fst trio, pair))
                        else
                            None
                    else
                        None
                let isFullHouse(h : Hand) = getFullHouse(h) <> None //TODO: rework this
                let isFlush(h : Hand) = hand |> areSameSuit
                let getThreeOfAKind(h : Hand) = 
                    let groups = h.Cards |> Seq.countBy(fun c -> c.Face) |> Seq.toList |> List.sortWith(fun (_,n1) (_,n2) -> compare n2 n1)
                    if groups |> Seq.length = 3 then
                        let trio = groups |> List.head
                        if trio |> snd = 3 then
                            let kicker1 = groups |> Seq.nth 1 |> fst
                            let kicker2 = groups |> Seq.nth 2 |> fst
                            if kicker1 > kicker2 then
                                Some(Three_of_a_Kind(fst trio, kicker1, kicker2))
                            else
                                Some(Three_of_a_Kind(fst trio, kicker2, kicker1))
                        else
                            None
                    else
                        None
                let isThreeOfAKind(h : Hand) = getThreeOfAKind(h) <> None //TODO: rework this
                let getTwoPairs(h : Hand) = 
                    let groups = h.Cards 
                                 |> Seq.countBy(fun c -> c.Face) 
                                 |> Seq.toList 
                                 |> List.sortWith(fun (f1,n1) (f2,n2) -> let comp = compare n2 n1
                                                                         if comp = 0 then
                                                                             compare f2 f1
                                                                         else
                                                                             comp)
                    if groups |> Seq.length = 3 then
                        let fstPair = groups |> List.head
                        if fstPair |> snd = 2 then
                            let sndPair = groups |> Seq.nth 1 |> fst
                            let kicker = groups |> Seq.nth 2 |> fst
                            Some(Two_Pairs(fst fstPair, sndPair, kicker))
                        else
                            None
                    else
                        None
                let isTwoPairs(h : Hand) = getTwoPairs(h) <> None //TODO: rework this
                let getOnePair(h : Hand) = 
                    let groups = h.Cards 
                                 |> Seq.countBy(fun c -> c.Face) 
                                 |> Seq.toList 
                                 |> List.sortWith(fun (f1,n1) (f2,n2) -> let comp = compare n2 n1
                                                                         if comp = 0 then
                                                                             compare f2 f1
                                                                         else
                                                                             comp)
                    if groups |> Seq.length = 4 then
                        let pair = groups |> List.head
                        if pair |> snd = 2 then
                            let kicker1 = groups |> Seq.nth 1 |> fst
                            let kicker2 = groups |> Seq.nth 2 |> fst
                            let kicker3 = groups |> Seq.nth 3 |> fst
                            Some(One_Pair(fst pair,kicker1,kicker2,kicker3))
                        else
                            None
                    else
                        None
                let isOnePair(h : Hand) = getOnePair(h) <> None //TODO: rework this
                let getHighCard(h : Hand) = 
                    let groups = h.Cards 
                                 |> Seq.countBy(fun c -> c.Face) 
                                 |> Seq.toList 
                                 |> List.sortWith(fun (f1,n1) (f2,n2) -> let comp = compare n2 n1
                                                                         if comp = 0 then
                                                                             compare f2 f1
                                                                         else
                                                                             comp)
                    if groups |> Seq.length = 5 then
                        let kicker0 = groups |> Seq.nth 0 |> fst
                        let kicker1 = groups |> Seq.nth 1 |> fst
                        let kicker2 = groups |> Seq.nth 2 |> fst
                        let kicker3 = groups |> Seq.nth 3 |> fst
                        let kicker4 = groups |> Seq.nth 4 |> fst
                        Some(High_Card(kicker0,kicker1,kicker2,kicker3,kicker4))
                    else
                        None
                let isHighCard(h : Hand) = getHighCard(h) <> None //TODO: rework this
                match hand with
                    | _ when hand |> isRoyalFlush    -> Royal_Flush
                    | _ when hand |> isStraightFlush -> Straight_Flush(let c = hand.Cards |> List.head in c.Face)
                    | _ when hand |> isFourOfAKind   -> let kind = getFourOfAKind hand
                                                        match kind with
                                                            | None -> failwith "laca"
                                                            | Some(rank) -> rank
                    | _ when hand |> isFullHouse     -> let kind = getFullHouse hand
                                                        match kind with
                                                            | None -> failwith "laca"
                                                            | Some(rank) -> rank
                    | _ when hand |> isFlush        ->  let cards = hand.Cards |> List.toArray
                                                        Flush(cards.[0].Face,cards.[1].Face,cards.[2].Face,cards.[3].Face,cards.[4].Face)
                    | _ when hand |> isStraight     ->  Straight(let c = hand.Cards |> List.head in c.Face)
                    | _ when hand |> isThreeOfAKind ->  let kind = getThreeOfAKind hand
                                                        match kind with
                                                            | None -> failwith "laca"
                                                            | Some(rank) -> rank
                    | _ when hand |> isTwoPairs     ->  let kind = getTwoPairs hand
                                                        match kind with
                                                            | None -> failwith "laca"
                                                            | Some(rank) -> rank
                    | _ when hand |> isOnePair      ->  let kind = getOnePair hand
                                                        match kind with
                                                            | None -> failwith "laca"
                                                            | Some(rank) -> rank
                    | _ when hand |> isHighCard     ->  let kind = getHighCard hand
                                                        match kind with
                                                            | None -> failwith "laca"
                                                            | Some(rank) -> rank
                    | _ -> failwith ("Error: Could not rank: " + (sprintf "%A" hand))

    interface IComparable<Hand> with
        member hand.CompareTo (h :Hand) = 
                compare hand.Rank h.Rank
    interface IComparable with
        member hand.CompareTo (ob :obj) =
                let h = unbox<Hand> ob 
                compare hand.Rank h.Rank
    override hand.Equals(o :obj)  = 
                let h = unbox<Hand> o
                hand.Rank = h.Rank
    override hand.GetHashCode() = hand.GetHashCode() 
    static member Parse(s : string) = 
        let strCards = s.Split ' ';
        if strCards.Length <> 5 then
            failwith ("Error parsing: " + s)
        else
            let cards = strCards |> Seq.map(Card.Parse) |> Seq.toList
            Hand(cards)
(*
let player1Hand = Hand.Parse("5H 5C 6S 7S KD");
let player2Hand = Hand.Parse("2C 3S 8S 8D TD");
printfn "player1 hand: %A" player1Hand.Rank
printfn "player2 hand: %A" player2Hand.Rank

printfn "player1 wins:  %A" (player1Hand > player2Hand)

let player3Hand = Hand.Parse("5D 8C 9S JS AC");
let player4Hand = Hand.Parse("2C 5C 7D 8S QH");
printfn "player1 hand: %A" player3Hand.Rank
printfn "player2 hand: %A" player4Hand.Rank

printfn "player1 wins:  %A" (player3Hand > player4Hand)

let player5Hand = Hand.Parse("TH KH JH QH AH");
let player6Hand = Hand.Parse("3C 3D 3S 9S 9D");
printfn "player1 hand: %A" player5Hand.Rank
printfn "player2 hand: %A" player6Hand.Rank

printfn "player1 wins:  %A" (player5Hand > player6Hand)
*)

let hands = 
    let reader = File.ReadAllLines("poker.txt")
    reader |> Seq.map(fun line -> let cards = line.Split ' ' 
                                  let player1Hand = Hand([for i in 0 .. 4 do yield Card.Parse(cards.[i])])
                                  let player2Hand = Hand([for i in 5 .. 9 do yield Card.Parse(cards.[i])])
                                  (player1Hand,player2Hand))


let solution = hands |> Seq.fold(fun acc (player1Hand,player2Hand) -> if player1Hand > player2Hand then acc+1 else acc ) 0
solution |> printfn "player 1 won %d hands"

Console.ReadKey(true) |> ignore
