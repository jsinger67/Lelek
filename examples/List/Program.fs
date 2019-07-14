open System.IO
open LelekParser
open ParseTree
open SrcNamespace

// To generate in Powershell:
// $SolutionDir=Get-Location
// cd .\src\Lelek
// dotnet run -- --file ${SolutionDir}\grammars\lst.llk --generate-lexersrc ${SolutionDir}\examples\List\ListLexer.fs --generate-parsersrc ${SolutionDir}\examples\List\ListParser.fs --save-linear-grammar --asttype AST.AST

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        printfn "\nMissing input file argument.\n"
        exit 1

    AST.init argv.[0]
    
    let start = System.DateTime.Now
    let ok, pt, us =
        try
            ParserModule.parse argv.[0] false
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
