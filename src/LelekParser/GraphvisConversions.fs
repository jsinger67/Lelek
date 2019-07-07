namespace LelekParser

/// Conversions of data into dot syntax.
module GraphvisConversions =

    [<RequireQualifiedAccess>]
    module CompiledDfa =
        open CompiledLADfa

        let toDot title (printItem: (int -> string)) (cdfa: CompiledDfa): string =
            let nodes =
                cdfa
                |> Array.fold (fun acc s ->
                    if s.Accepted then
                        acc + (sprintf "    \"%d\" [shape=doublecircle, color=red];\r\n" s.Id)
                    else
                        if s.Id = 0 then
                            acc + (sprintf "    \"%d\" [shape=ellipse, color=green];\r\n" s.Id)
                        else
                            acc + (sprintf "    \"%d\" [shape=ellipse, color=blue];\r\n" s.Id)
                ) ""

            let edges =
                cdfa
                |> Array.fold (fun acc s ->
                    s.Transitions
                    |> Array.fold (fun acc elem ->
                        let (Tr(Tok = t; Next = n)) = elem
                        acc + (sprintf "    \"%d\" -> \"%d\" [label=\"%s\"];\r\n" s.Id n (printItem t))
                    ) acc
                ) ""

            sprintf """digraph G {
    rankdir=LR;
    label="%s";

%s

%s
}
"""
                title nodes edges


    [<RequireQualifiedAccess>]
    module Dfa =
        open LAAutomaton
        open LADfa

        let toDot title (printItem: (LAItem -> string)) (dfa: Dfa): string =
            let printSet s =
                s
                |> Set.map (sprintf "%x")
                |> String.concat "|"
            let nodes =
                dfa
                |> Map.fold (fun acc k v ->
                    if v.Accepted then
                        acc + (sprintf "    \"%s\" [shape=doublecircle, color=red];\r\n" (printSet k))
                    else
                        if k = Set.singleton 0 then
                            acc + (sprintf "    \"%s\" [shape=ellipse, color=green];\r\n" (printSet k))
                        else
                            acc + (sprintf "    \"%s\" [shape=ellipse, color=blue];\r\n" (printSet k))
                ) ""

            let edges =
                dfa
                |> Map.fold (fun acc k v ->
                    v.Transitions
                    |> List.fold (fun acc elem ->
                        let (DfaTransition(Item = item; ToState = ts)) = elem
                        acc + (sprintf "    \"%s\" -> \"%s\" [label=\"%s\"];\r\n" (printSet k) (printSet ts) (printItem item))
                    ) acc
                ) ""

            sprintf """digraph G {
    rankdir=LR;
    label="%s";

%s

%s
}
"""
                title nodes edges


    [<RequireQualifiedAccess>]
    module Nfa =
        open LAAutomaton
        open LANfa

        let toDot title (printItem: (LAItem -> string)) (nfa: Nfa): string =
            let nodes =
                nfa
                |> Map.fold (fun acc k v ->
                    if v.Accepted then
                        acc + (sprintf "    \"%x\" [shape=doublecircle, color=red];\r\n" k)
                    else
                        if k = 0 then
                            acc + (sprintf "    \"%x\" [shape=circle, color=green];\r\n" k)
                        else
                            acc + (sprintf "    \"%x\" [shape=circle, color=blue];\r\n" k)
                ) ""

            let edges =
                nfa
                |> Map.fold (fun acc k v ->
                    v.Transitions
                    |> List.fold (fun acc elem ->
                        let (Transition(Item = item; ToState = ts)) = elem
                        acc + (sprintf "    \"%x\" -> \"%x\" [label=\"%s\"];\r\n" k ts (printItem item))
                    ) acc
                ) ""

            sprintf """digraph G {
    rankdir=LR;
    label="%s";

%s

%s
}
"""
                title nodes edges

