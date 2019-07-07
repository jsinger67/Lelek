namespace LelekParser

/// Conversions of data types into their F# code representtation
module FSharpSrcGen =

    [<RequireQualifiedAccess>]
    module LAItem =
        open LAAutomaton

        let toFs  = function
            | Te s -> sprintf "Te(%d)" s;
            | Eps -> "Eps"
            | Eos -> "Eos"


    [<RequireQualifiedAccess>]
    module Transition =
        open LANfa

        let toFs (Transition(Item = i; ToState = t)): string =
            sprintf "Transition(Item = %s, ToState = %d)" (LAItem.toFs i) t


    [<RequireQualifiedAccess>]
    module NfaState =
        open LANfa

        let toFs ({Transitions = tr; Accepted = a}): string =
            let tStr = tr |> List.map Transition.toFs |> String.concat "\n                                                    "
            sprintf "{Transitions = [\n                                                    %s]; Accepted = %b}" tStr a


    [<RequireQualifiedAccess>]
    module DfaTransition =
        open LADfa

        let toFs (DfaTransition(Item = i; ToState = s)): string =
            let iStr = LAItem.toFs i
            let sStr = intSetToFs s
            sprintf "DfaTransition(Item = %s, ToState = %s)" iStr sStr


    [<RequireQualifiedAccess>]
    module DfaState =
        open LADfa

        let toFs (state: DfaState): string =
            let tStr = state.Transitions |> List.map (DfaTransition.toFs) |> String.concat "\n                                            "
            let nStr = state.NfAStates |> List.map (fun (i,s) -> sprintf "(0x%x, %s)" i (NfaState.toFs s)) |> String.concat "\n                                            "
            sprintf @"                                    {   Transitions = [
                                            %s]
                                        Accepted = %b
                                        NfAStates = [
                                            %s]
                                    }"
                tStr state.Accepted nStr


    [<RequireQualifiedAccess>]
    module Dfa =
        open LADfa

        let toFs (dfa: Dfa): string =
            let intSetToFs (s: Set<int>): string =
                //s |> Set.map(fun i -> sprintf "%x") |> String.concat "-"
                s |> Set.fold (fun acc elem -> acc + (sprintf ".Add(0x%x)" elem) ) "Set.empty"

            dfa
            |> Map.toSeq
            |> Seq.fold (fun acc (stateKey, stateVal) ->
                let kStr = intSetToFs stateKey
                let vStr = DfaState.toFs stateVal
                acc + (sprintf "\n                                .Add(%s,\n%s)" kStr vStr)
            ) "                            Map.empty"


    [<RequireQualifiedAccess>]
    module Tr =
        open CompiledLADfa

        let toFs (Tr(Tok = t; Next = s)): string =
            sprintf "                                    Tr(Tok = %d, Next = %d)" t s


    [<RequireQualifiedAccess>]
    module State =
        open CompiledLADfa
        let toFs ({Id = id; Transitions = ts; Accepted = a; Prediction = pr}): string =
            let tsStr =
                match ts with
                | [||] -> "[||]"
                | _ -> ts |> Array.map Tr.toFs |> String.concat "\n" |> sprintf """[|
%s
                                |]"""

            sprintf """                            {
                                Id = %d
                                Transitions = %s
                                Accepted = %b
                                Prediction = %d
                            }""" id tsStr a pr


    [<RequireQualifiedAccess>]
    module CompiledDfa =
        open CompiledLADfa

        let toFs (cdfa: CompiledDfa): string =
            match cdfa with
            | [||] -> "[||]\n"
            | _ ->
                cdfa |> Array.map State.toFs |> String.concat "\n" |> sprintf """
                        [|
%s
                        |]
"""


    [<RequireQualifiedAccess>]
    module ParserSymbol =
        let toFs ps =
            sprintf "%d" ps


    [<RequireQualifiedAccess>]
    module Production =
        open ParserTypes

        let toFs (pls: ParserSymbol list) =
            pls
            |> List.map ParserSymbol.toFs
            |> String.concat "; "
            |> sprintf "[|%s|]"


    [<RequireQualifiedAccess>]
    module ParserRuleSrc =
        open CompiledLADfa
        open ParserData

        let toFs (ParserRuleSrc(Name = name; Production = prod; PTOp = op; Action = act)) (cdfa: CompiledDfa) =
            let cmpDfTxt = cdfa |> CompiledDfa.toFs
            sprintf """
                ParserRule(
                    Name = "%s",
                    Production = %s,
                    PTOp = %s,
                    Action = %s,
                    LookaheadDFA = %s                )"""
                    name
                    (Production.toFs prod)
                    op
                    act
                    cmpDfTxt


    [<RequireQualifiedAccess>]
    module RuleSetDataSrc =
        open ParserData
        open CompiledLADfa

        let toFs (RuleSetDataSrc(Rules = parserRules; LookaheadDFA = cdfa; BaseIndex = idx)): string =
            let ruleName =
                parserRules
                |> List.head
                |> ParserRuleSrc.nameOf
            let prTxt =
                parserRules
                |> List.mapi (fun i r ->
                    let d = if i = 0 then cdfa else CompiledDfa.empty
                    ParserRuleSrc.toFs r d)
                |> String.concat ""
            sprintf """

                // -----------------------------------------------------------------------------
                // %-4d Rules of "%s"
                // -----------------------------------------------------------------------------%s""" idx ruleName prTxt


    [<RequireQualifiedAccess>]
    module ParserData =
        open ParserData

        let toFs (pd: RuleSetDataSrc list): string =
            pd
            |> Seq.fold (fun acc elem ->
                let vStr = RuleSetDataSrc.toFs elem
                acc + vStr) ""
            |> sprintf """
            [|%s
            |]"""
