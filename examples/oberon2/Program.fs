open System.IO
open Oberon2
open LelekParser
open ParseTree

// Lelek arguments:
// $SolutionDir=Get-Location
// cd .\src\Lelek
// dotnet run -- --file ${SolutionDir}\grammars\oberon-2.llk --generate-lexersrc ${SolutionDir}\examples\oberon2\Oberon2Lexer.fs --generate-parsersrc ${SolutionDir}\examples\oberon2\Oberon2Parser.fs --save-linear-grammar --asttype AST.AST --srcnamespacename Oberon2 --lexermodulename Oberon2Lexer --parsermodulename Oberon2Parser

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "\nMissing input file argument.\n"
        exit 1

    AST.init argv.[0]
    
    let start = System.DateTime.Now
    let ok, pt, us =
        try
            Oberon2Parser.parse argv.[0] false
        with
            | ex -> printfn "\n\nParsing exception:\n>>>\n%s\n\n" (ex.Message)
                    false, ParseTree.empty, []

    let duration = System.DateTime.Now - start

    try
        let visualizeSvg = ParseTreeVisualization.visualize SvgDrawer.drawAST
        visualizeSvg (Path.ChangeExtension(argv.[0], "svg")) pt
    with
        | ex -> printfn "\n\nException in ParseTreeVisualization: '%s'\n\n" (ex.Message)

    us |> List.rev |> printfn "%A"


    printfn "\nCompilation time: %O" duration

    if ok then 0 else 1
