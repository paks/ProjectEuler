///Project Euler Problem 100
///If a box contains twenty-one coloured discs, composed of fifteen blue discs and 
///six red discs, and two discs were taken at random, it can be seen that the probability 
///of taking two blue discs, P(BB) = (15/21)(14/20) = 1/2.
///
///The next such arrangement, for which there is exactly 50% chance of taking two blue 
///discs at random, is a box containing eighty-five blue discs and thirty-five red discs.
///
///By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in 
///total, determine the number of blue discs that the box would contain.
//-----------------------------------------------------------------------
// <copyright file="blue.fs" >
// Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open System.Numerics

///solving this is equivalent ot solve the Diophantine equation
///          2x^2 - y^2 - 2x + y = 0
///using the solver here: http://www.alpertron.com.ar/QUAD.HTM
///by Dario Alejandro Alpern
///
///X0 = 0
///Y0 = 1
///
///and also:
///X0 = 1
///Y0 = 1
///
///Xn+1 = P Xn + Q Yn + K
///Yn+1 = R Xn + S Yn + L
///
///P = 3
///Q = 2
///K = -2
///R = 4
///S = 3
///L = -3
///
/// Xn+1 = 3Xn + 2Yn -2
/// Yn+1 = 4Xn + 3Yn -3

let solution =
    let rec solution' x y =
                let xn = 3I*x + 2I*y - 2I
                let yn = 4I*x + 3I*y - 3I
                if yn > 1000000000000I then
                    xn
                else
                    solution' xn yn
    solution' 1I 1I

solution |> printfn "solution: %A"
Console.ReadKey(true) |> ignore
