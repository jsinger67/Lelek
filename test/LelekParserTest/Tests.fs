module Tests2

open Xunit
open Swensen.Unquote
open LelekParser
open LlkParser
open LlkGrammar
open LinLlkGrammar
open LinLlkData
open LAAutomaton
open LANfa
open GraphvisConversions
open System.Text.RegularExpressions


let stringToBNFGrammar str =
    let parseResult = parseLlkString str
    match parseResult with
    | Result.Ok g ->
        g |> Linearize.linearizeGrammar
    | Result.Error e ->
        Assert.True(false)
        LinLlkData(CommentDecl.empty, [], [])

let fileToBNFGrammar fn =
    let parseResult = parseLlkFile fn
    match parseResult with
    | Result.Ok g ->
        g |> Linearize.linearizeGrammar
    | Result.Error e ->
        Assert.True(false)
        LinLlkData(CommentDecl.empty, [], [])

// This is used to ease the use of output from PasingEmu tool :)
let makeTerminalListFromString str =
    let terminalOrEps = function
        | 'e'   -> LinLlkEpsilon
        | c     ->  c |> string |> LinLlkTerminal

    str
    |> Seq.map terminalOrEps
    |> List.ofSeq

[<Fact>]
let ``LinLlkSymbol.toString Test 1`` () =
    let bnfSymbol = LinLlkTerminal @""""   // Not escaped yet
    test <@ LinLlkSymbol.toString bnfSymbol = @"""\""""" @>

[<Fact>]
let ``LinLlkSymbol.toString Test 2`` () =
    let bnfSymbol = LinLlkTerminal @"\"""  // Already escaped
    let str = LinLlkSymbol.toString bnfSymbol
    test <@ LinLlkSymbol.toString bnfSymbol = @"""\""""" @>

[<Fact>]
let ``nullables Test 1`` () =
    let bnfGrammar = stringToBNFGrammar @"S = A; A = ""a""; A = B; B = ""b"";"
    test <@ calcNullables bnfGrammar = [ ] @>

[<Fact>]
let ``nullables Test 2`` () =
    let bnfGrammar = stringToBNFGrammar @"S = A; A = ""a""; A = ""x""; B = ""b""; B = ;"
    test <@ calcNullables bnfGrammar = [ "b" ] @>

[<Fact>]
let ``nullables Test 3`` () =
    let bnfGrammar = stringToBNFGrammar @"S = A; A = ""a""; A = B; B = ""b""; B = ;"
    test <@ calcNullables bnfGrammar = [ "s"; "a"; "b" ] @>

[<Fact>]
let ``detectLeftRecursions Test 1`` () =
    let bnfGrammar = stringToBNFGrammar @"S = ""s""; A = ""a""; B = ""b"";"
    test <@ detectLeftRecursions bnfGrammar = [ ] @>

[<Fact>]
let ``detectLeftRecursions Test 2`` () =
    let bnfGrammar = stringToBNFGrammar @"S = S ""s"";"
    test <@ detectLeftRecursions bnfGrammar = [ ["s"; "s"] ] @>

[<Fact>]
let ``detectLeftRecursions Test 3`` () =
    let bnfGrammar = stringToBNFGrammar @"S = ""s""; A = ""a"";"
    test <@ detectLeftRecursions bnfGrammar = [ ] @>

[<Fact>]
let ``detectLeftRecursions Test 4`` () =
    let bnfGrammar = stringToBNFGrammar @"S = A; A = ""a""; A = B; B = ;"
    test <@ detectLeftRecursions bnfGrammar = [ ] @>

[<Fact>]
let ``detectLeftRecursions Test 5`` () =
    let bnfGrammar = stringToBNFGrammar @"A = B | ""a""; B = C | ""b""; C = A ""a"" | ""c"";"
    test <@ detectLeftRecursions bnfGrammar = [ ["b"; "c"; "a"; "b"]; ["c"; "a"; "b"; "c"]; ["a"; "b"; "c"; "a"] ]  @>

[<Fact>]
let ``detectLeftRecursions Test 6`` () =
    let bnfGrammar = stringToBNFGrammar @"A = B C | ""x""; B = ""y"" | ; C = A ""x"" | ""z"";"
    test <@ detectLeftRecursions bnfGrammar = [ ["b"; "c"; "a"; "b"]; ["a"; "b"; "c"; "a"] ]  @>

[<Fact>]
let ``detectLeftRecursions Test 7`` () =
    let bnfGrammar = stringToBNFGrammar @"A = B C | ""x""; B = ""y"" | ""z""; C = A ""x"" | ""z"";"
    test <@ detectLeftRecursions bnfGrammar = [ ]  @>

[<Fact>]
let ``detectLeftRecursions Test 8`` () =
    let bnfGrammar = stringToBNFGrammar @"s = a; d = s ""DS1""; d = s ""DS2""; a = ""A"";"
    test <@ detectLeftRecursions bnfGrammar = [ ]  @>

[<Fact>]
let ``detectLeftRecursions Test 9`` () =
    let bnfGrammar = stringToBNFGrammar @"E = E ""+"" E2; E = E ""−"" E2; E = E2; E2 = E2 ""*"" E3; E2 = E2 ""/"" E3; E2 = E3; E3 = ""num""; E3 = ""("" E "")"";"
    test <@ detectLeftRecursions bnfGrammar = [["e"; "e"]; ["e2"; "e2"]]   @>

[<Fact>]
let ``detectLeftRecursions Test 10`` () =
    let bnfGrammar = stringToBNFGrammar @"or_expr = xor_expr; or_expr = or_expr ""|"" xor_expr;"
    test <@ detectLeftRecursions bnfGrammar = [["or_expr"; "or_expr"]]   @>


// -------------------------------------------------------------------------------------------------
// Tests of LA automata generation
// -------------------------------------------------------------------------------------------------
let i2s = (sprintf " %d ")
let i2n g = (LexerTerminals.indexToName g) >> (sprintf @"""%s""")

[<Fact>]
let ``calcLARx 1`` () =
    let g = stringToBNFGrammar @"S = A; A = ""a"" | ""b"";"
    let (LinLlkData(_, rl, _)) = g
    let rule = rl.Head
    let nfa = calcLAData g 1 rule
    let glaRx = nfa |> (LAData.toString (i2n g))
    test <@ glaRx = "(\"a\"|\"b\")" @>

[<Fact>]
let ``calcLARx 2`` () =
    let g = stringToBNFGrammar @"S = A; A = B | C; B = ""a"" | ""b""; C = ""c"" | ""d"";"
    let (LinLlkData(_, rl, _)) = g
    let rule = rl.Head
    let nfa = calcLAData g 1 rule
    let glaRx = nfa |> (LAData.toString (i2n g))
    test <@ glaRx = "((\"a\"|\"b\")|(\"c\"|\"d\"))" @>

[<Fact>]
let ``calcLARx 3`` () =
    let g = stringToBNFGrammar @"S = A; A = B | C; B = ""a"" | ""b""; C = ""c"" | ;"
    let (LinLlkData(_, rl, _)) = g
    let rule = rl.Head
    let nfa = calcLAData g 1 rule
    let glaRx = nfa |> (LAData.toString (i2n g))
    test <@ glaRx = "(((\"a\"|\"b\")|(\"c\"|ε))$)" @>

[<Fact>]
let ``calcLARx 4`` () =
    let g = stringToBNFGrammar @"S = A; A = B | C; B = ""a"" | ""b""; C = ""c"" | ;"
    let (LinLlkData(_, rl, _)) = g
    let rule = rl.Head
    let nfa = calcLAData g 2 rule
    let glaRx = nfa |> (LAData.toString (i2n g))
    test <@ glaRx = "(((\"a\"|\"b\")|(\"c\"|ε))($$))" @>

[<Fact>]
let ``calcLARx 5`` () =
    let g = stringToBNFGrammar @"S = ""a"" T U ""f"" ""g""; T = ""b"" | ""c""; U = ""d"" | ""e"";"
    // S->aTUfg
    // T->b
    // T->c
    // U->d
    // U->e
    // =>
    // {abdfg, abefg, acdfg, acefg}
    let (LinLlkData(_, rl, _)) = g
    let rule = rl.Head
    let nfa = calcLAData g 5 rule
    let glaRx = nfa |> (LAData.toString (i2n g))
    test <@ glaRx = "(\"a\"(\"b\"|\"c\")(\"d\"|\"e\")\"f\"\"g\")" @>

[<Fact>]
let ``calcLARx 6`` () =
    let g = stringToBNFGrammar @"S = A; A = B [C] D; B = ""a"" | ""b""; C = ""c""; D = ""d""; D =;"
    let (LinLlkData(_, rl, _)) = g
    let rule = rl.Head
    let nfa = calcLAData g 3 rule
    let glaRx = nfa |> (LAData.toString (i2n g))
    test <@ glaRx = "(((\"a\"|\"b\")(ε|\"c\")(\"d\"|ε))($$))" @>

[<Fact>]
let ``calcLARx 7`` () =
    let g = stringToBNFGrammar @"S = A; A = B [C] D; B = ; C = ""c""; D = ;"
    let (LinLlkData(_, rl, _)) = g
    let rule = rl.Head
    let nfa = calcLAData g 2 rule
    let glaRx = nfa |> (LAData.toString (i2n g))
    test <@ glaRx = "((ε(ε|\"c\")ε)($$))" @>

[<Fact>]
let ``calcLARx 8`` () =
    let g = stringToBNFGrammar @"S = A; A = B [C] D; B = ; C = ""c""; D = ;"
    let (LinLlkData(_, rl, _)) = g
    let rule = rl.Head
    let nfa = calcLAData g 3 rule
    let glaRx = nfa |> (LAData.toString (i2n g))
    test <@ glaRx = "((ε(ε|\"c\")ε)($$$))" @>

// -------------------------------------------------------------------------------------------------
// Tests of LA NFA
// -------------------------------------------------------------------------------------------------
let escape s =
    let rxEscQuote = Regex(@"(?<!\\)""", RegexOptions.Compiled)
    rxEscQuote.Replace(s, @"\""")

let tt2str g =
    let i2n g = (LexerTerminals.indexToName g) >> (sprintf "%s")
    LAItem.toString (i2n g)

[<Fact>]
let ``Nfa.ofLAData empty alternative`` () =
    let laData = Item(Eps)

    let title = "empty"

    let nfa =
        laData |> Nfa.ofLAData 1

    let x =
        nfa
        |> Nfa.toDot title (LAItem.toString (sprintf "%d"))

    test <@ x = """digraph G {
    rankdir=LR;
    label="empty";

    "0" [shape=circle, color=green];
    "1" [shape=doublecircle, color=red];


    "0" -> "1" [label="ε"];

}
"""     @>

[<Fact>]
let ``Nfa.ofLAData empty alternative 2`` () =
    let laData = Concat [Item Eps; Union (set [])]

    let title = "empty"

    let nfa =
        laData |> Nfa.ofLAData 1

    let x =
        nfa
        |> Nfa.toDot title (LAItem.toString (sprintf "%d"))

    test <@ x = """digraph G {
    rankdir=LR;
    label="empty";

    "0" [shape=circle, color=green];
    "1" [shape=doublecircle, color=red];


    "0" -> "1" [label="ε"];

}
"""     @>

