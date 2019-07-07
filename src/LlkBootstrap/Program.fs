// Learn more about F# at http://fsharp.org

open System
open System.IO
open Console
open LelekParser
open LexerSrcGen
open ParserSrcGen
open LinLlkGrammar

let reportStart inputFile =
    sprintf "Processing %s..." (Path.GetFullPath(inputFile)) |> printCyan

let reportError e w m =
    printfn "Exiting due to previous errors\r\n%s" m
    sprintf "%d Error(s), %d Warning(s)" e w |> printRed

let parseInputFile fn =
    let result = LlkParser.parseLlkFile fn
    match result with
    | Ok g -> g
    | Error e ->
        reportError 1 0 e
        exit 1

let saveLin bnf inputFile =
    let fn = Path.ChangeExtension(inputFile, ".lin")
    use swbnf = new StreamWriter(File.Open(fn, FileMode.Create))
    swbnf.Write (LinLlkData.toString bnf)

let checkUnreachables bnf =
    let unreachables =
        bnf
        |> LinLlkData.findUnreachables
    match unreachables with
    | [] ->
        printDarkGreen "Grammar contains no unreachable symbols!"
        true
    | _  ->
        printYellow (sprintf "Grammar contains %d unreachable symbols:" (unreachables.Length))
        unreachables
        |> List.iter (sprintf "%s" >> Console.WriteLine)
        false

let generateLexerScr logger bnf lxSrcFile lxNameSpace lxModule =
    let res = generateLexerSrc logger bnf lxSrcFile lxNameSpace lxModule
    match res with
    | Ok _ ->
        printDarkGreen (sprintf "Lexer source generated to %s" lxSrcFile)
    | Error err ->
        printYellow "Error creating lexer:"
        printRed err

let generateParserSrc par =
    let res = generateParserSrc par
    match res with
    | Ok _ ->
        printDarkGreen (sprintf "Parser source generated to %s" par.OutFileName)
    | Error err ->
        printYellow "Error creating parser:"
        printRed err

[<EntryPoint>]
let main argv =
    let solutionDir = argv.[0]
    let inputFile = (solutionDir + @"..\LelekBS\Llk.llk")
    reportStart inputFile

    let logger =
        ParserGenLogger.provideLogger
            inputFile
            (Path.GetDirectoryName(inputFile) + @"\log\" +
            (Path.GetFileNameWithoutExtension(inputFile)) + ".log")
    
    let g = parseInputFile inputFile
    let gBnf = g |> Linearize.linearizeGrammar

    saveLin gBnf inputFile
    let ns = "LelekBS"
    let ml = "LlkLexer"

    generateLexerScr logger gBnf (solutionDir + @"..\LelekBS\LlkLexer.fs") ns ml

    if gBnf |> checkUnreachables |> not then
        printRed "Can't proceed..."
        exit 1

    try
        generateParserSrc {
            ParserSrcGenParams.Logger = logger
            MaxLookahead = 5
            Grammar = gBnf
            OutFileName = solutionDir + @"..\LelekBS\LlkParser.fs"
            ParserNamespace = ns
            ParserModule = "LlkParser"
            LexerModule = ml
            ASTTypeName = "AST.AST" }
    with
        | ex ->
            ex.GetType().FullName |> sprintf "Exception '%s' caught!" |> printRed
            ex.Message |> printRed
            ex.StackTrace |> printYellow
            "Sorry. Please report this error!" |> printCyan

    0
