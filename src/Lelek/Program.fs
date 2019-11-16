// Learn more about F# at http://fsharp.org

open System
open System.IO
open Argu
open CLIArguments
open Console
open LelekParser
open LexerTerminals
open LexerSrcGen
open ParserSrcGen
open LinLlkGrammar
open ParserGenLogger
open ParsingPrediction
open ParseTree
open LelekBS
open AST

// Arguments for Calc example (src\Lelek\Properties\launchSettings.json):
// --file $(SolutionDir)\Grammars\Calc.llk --generate-lexersrc $(SolutionDir)examples\Calc\CalcLexer.fs --generate-parsersrc $(SolutionDir)examples\Calc\CalcParser.fs --save-linear-grammar --asttype AST.AST

// To run Calc example in powershell:
// $SolutionDir = "D:\Source-Code\git\Lelek2\"
// dotnet run --file ${SolutionDir}\Grammars\Calc.llk --generate-lexersrc ${SolutionDir}examples\Calc\CalcLexer.fs --generate-parsersrc ${SolutionDir}examples\Calc\CalcParser.fs --save-linear-grammar --asttype AST.AST

let reportStart inputFile =
    sprintf "Processing %s..." (Path.GetFullPath(inputFile)) |> printCyan

let reportError e w m =
    printfn "Exiting due to previous errors\r\n%s" m
    sprintf "%d Error(s), %d Warning(s)" e w |> printRed

let parseInputFile fn trace =
    let start = DateTime.Now
    let ok, pt, us =
        try
            LlkParser.parse fn trace
        with
            | ex -> printfn "\n\nParsing exception:\n>>>\n%s\n\n" (ex.Message)
                    false, ParseTree.empty, []
    let duration = DateTime.Now - start

    try
        let visualizseSvg = ParseTreeVisualization.visualize SvgDrawer.drawAST
        visualizseSvg (Path.ChangeExtension(fn, "svg")) pt
    with
        | ex -> printfn "\n\nException in ParseTreeVisualization: '%s'\n\n" (ex.Message)

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


let saveLin bnf inputFile cfgFile =
    let fn = cfgFile |> Option.defaultValue (Path.ChangeExtension(inputFile, ".lin")) 
    use swbnf = new StreamWriter(File.Open(fn, FileMode.Create))
    swbnf.Write (LinLlkData.toString bnf)

let checkLeftRecursions bnf =
    let recursions = bnf |> LinLlkData.detectLeftRecursions
    if recursions.IsEmpty then
        printDarkGreen "Grammar contains no left recursions!"
    else
        printYellow (sprintf "Grammar contains at least %d left recursions:" (recursions.Length))
        recursions
        |> List.iter (String.concat "->" >> sprintf "  %s" >> printCyan)
        printRed "\nERROR: Can't process left recursive grammars! Exiting..."
        exit 1

let findPrefixes name bnf =
    let prefixes = bnf |> LeftFactoring.findLongestPrefixes
    if prefixes.IsEmpty then
        printDarkGreen (sprintf "%s grammar contains no left prefixes!" name)
    else
        printYellow (sprintf "%s grammar contains %d left prefixes:" name (prefixes.Length))
        prefixes
        |> List.iter (fun (sym, pref) ->
            sym |> sprintf "Rules: %s" |> printYellow
            pref |> List.map LinLlkSymbol.toString |> String.concat ", " |> sprintf "Prefix: %s\n" |> printCyan
        )

let leftFacor bnf =
    LeftFactoring.leftFactor bnf

let canDecide bnf (conflicts: LL1Conflicts) maxK =
    let rec tryToDecide k =
        if k < maxK then
            conflicts
            |> List.iter (fun cnflct ->
                String('-', 80) |> printCyan
                cnflct.Head
                |> First1SetOfRule.f1SetOf
                |> Set.map LinLlkSymbol.toString |> String.concat ", " |> sprintf "%s:" |> printCyan

                let ok, actualK =
                    cnflct
                    |> canResolveConflicts bnf k
                if ok then
                    printDarkGreen (sprintf "decidable with k=%d" actualK)
                else
                    tryToDecide (k + 1)
            )
        else
            ()

    tryToDecide 1


let findLL1Conflicts (logger: Logger) name bnf k =
    let conflicts = findLL1Conflicts logger bnf
    if conflicts.IsEmpty then
        printDarkGreen (sprintf "%s grammar contains no LL(1) conflicts!" name)
    else
        printYellow (sprintf "%s grammar contains %d LL(1) conflicts:" name (conflicts.Length))
        conflicts
        |> List.iter (fun cnflct ->
            String('-', 80) |> printCyan
            cnflct.Head
            |> First1SetOfRule.f1SetOf
            |> Set.map LinLlkSymbol.toString |> String.concat ", " |> sprintf "%s:" |> printCyan
            cnflct
            |> List.iter (fun (First1SetOfRule(Rule = rl)) ->
                rl
                |> LinLlkRule.toString
                |> sprintf "%s"
                |> Console.WriteLine
                rl
                |> LAAutomaton.calcLAData bnf k
                |> LAAutomaton.LAData.toString ((indexToName bnf) >> (sprintf @" ""%s"" "))
                |> sprintf "LA NFA: %s\n"
                |> Console.WriteLine
            )
        )

        String('=', 80) |> printRed
        canDecide bnf conflicts k
        String('=', 80) |> printRed


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
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Lelek", errorHandler = errorHandler)
    let args = parser.ParseCommandLine argv

    if args.IsUsageRequested then
        parser.PrintUsage |> ignore
        exit 0

    let inputFile = args.GetResult File
    reportStart inputFile

    let logger =
        provideLogger
            inputFile
            (Path.GetDirectoryName(inputFile) + @"\log\" +
            (Path.GetFileNameWithoutExtension(inputFile)) + ".log")
    
    let g = parseInputFile inputFile (args.Contains TraceMode)
    let gBnf = g |> Linearize.linearizeGrammar

    let ns =
        if args.Contains SrcNameSpaceName then
            args.GetResult SrcNameSpaceName
        else
            "SrcNamespace"

    let ml =
        if args.Contains LexerModuleName then
            args.GetResult LexerModuleName
        else
            "LexerModule"

    if gBnf |> checkUnreachables |> not then
        printRed "Can't proceed..."
        exit 1

    checkLeftRecursions gBnf

    if args.Contains Find_Prefixes then
        findPrefixes "Input" gBnf

    let gBnf = leftFacor gBnf

    if args.Contains Save_Linear_Grammar then
        saveLin gBnf inputFile (args.GetResult Save_Linear_Grammar)

    findPrefixes "Processed" gBnf

    let maxK = if args.Contains MaxK then args.GetResult MaxK else 5

    //if args.Contains Find_LL1Conflicts then
    //    findLL1Conflicts logger "Processed" gBnf maxK

    if args.Contains Generate_LexerSrc then        
        generateLexerScr logger gBnf (args.GetResult Generate_LexerSrc) ns ml

    if args.Contains Generate_LexerSrc && args.Contains Generate_ParserSrc then
        try
            generateParserSrc {
                ParserSrcGenParams.Logger = logger
                MaxLookahead = maxK
                Grammar = gBnf
                OutFileName = args.GetResult Generate_ParserSrc
                ParserNamespace = ns
                ParserModule =
                    if args.Contains ParserModuleName then
                        args.GetResult ParserModuleName
                    else
                        "ParserModule"
                LexerModule = ml
                ASTTypeName = args.GetResult ASTType }
        with
            | ex ->
                ex.GetType().FullName |> sprintf "Exception '%s' caught!" |> printRed
                ex.Message |> printRed
                ex.StackTrace |> printYellow
                "Sorry. Please report this error!" |> printCyan

    0
