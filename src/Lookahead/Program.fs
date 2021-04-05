// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open Argu
open CLIArguments
open Console
open LelekParser
open LelekBS
open ParseTree
open AST
open LinLlkGrammar
open LexerTerminals

let reportStart inputFile =
    sprintf "Processing %s..." (Path.GetFullPath(inputFile)) |> printCyan

let parseInputFile fn trace =
    let start = DateTime.Now
    let ok, pt, us =
        try
            LlkParser.parse fn trace
        with
            | ex -> printfn "\n\nParsing exception:\n>>>\n%s\n\n" (ex.Message)
                    false, ParseTree.empty, []
    let duration = DateTime.Now - start

    us |> List.rev |> printfn "%A"

    printfn "\nCompilation time: %O" duration

    if ok then
        match us with
        | Gr(g)::_ ->
            g
        | _ ->
            printRed "Invalid parsing result"
            exit 1
    else
        printRed "Parsing failed"
        exit 1


[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Lelek", errorHandler = errorHandler)
    let args = parser.ParseCommandLine argv

    if args.IsUsageRequested then
        parser.PrintUsage |> ignore
        exit 0

    let inputFile = args.GetResult File
    reportStart inputFile

    let g = parseInputFile inputFile false
    let gBnf = g |> Linearize.linearizeGrammar |> LeftFactoring.leftFactor

    let maxK = if args.Contains MaxK then args.GetResult MaxK else 5

    let (LinLlkData(_, rl, _)) = gBnf

    let printRule(rl) =
        rl |> LinLlkRule.toShortString |> sprintf "Rule  : %s" |> Console.WriteLine
        rl


    rl
    |> List.iter(fun rule ->
        rule
        |> printRule
        |> LAAutomaton.calcLAData gBnf maxK
        |> LAAutomaton.LAData.toString ((indexToName gBnf) >> (sprintf @" ""%s"" "))
        |> sprintf "LA NFA: %s\n\n"
        |> Console.WriteLine
    )

    0
