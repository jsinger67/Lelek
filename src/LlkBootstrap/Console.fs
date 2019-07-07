module Console

open System

let printColored color txt =
    let lastfgColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    printfn "%s" txt
    Console.ForegroundColor <- lastfgColor

let printRed =
    printColored ConsoleColor.Red

let printCyan =
    printColored ConsoleColor.Cyan

let printDarkGreen =
    printColored ConsoleColor.DarkGreen

let printYellow =
    printColored ConsoleColor.Yellow
