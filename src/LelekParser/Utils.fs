namespace LelekParser

module Utils =
    open System.Text.RegularExpressions

    let rec doWhileChanging (f: 'T -> 'T) (arg: 'T) : 'T =
        let res = f arg
        if res = arg then
            arg
        else
            doWhileChanging f res

    let generateName (exclusions: string seq) (prefix: string) : string =
        let rxNumSuffix = Regex(@"\d+$", RegexOptions.Compiled)
        let startNum, pref =
            let mat = rxNumSuffix.Match(prefix)
            if mat.Success then
                ((mat.Value) |> int) + 1, prefix.Substring(0, mat.Index)
            else
                1, prefix
        let rec _genName num : string =
            let newTry = sprintf "%s%d" pref num
            match Seq.tryFind ((=) newTry) exclusions with
            | Some _ -> _genName (num + 1)
            | _ -> newTry

        if Seq.contains prefix exclusions then
            _genName startNum
        else
            prefix

    let escape s =
        let rxEscQuote = Regex(@"(?<!\\)""", RegexOptions.Compiled)
        rxEscQuote.Replace(s, @"\""")
     
    let escapeVerbatim s =
        let rxEscQuote = Regex(@"(?<!(""))""", RegexOptions.Compiled)
        rxEscQuote.Replace(s, @"""""")

    let internalError dir file line msg =
        failwithf "Internal error: \"%s\" in %s\\%s(%s)" msg dir file line

    // This binary operator returns the left operand after applying it to the function at the right hand side.
    // The function f must have the return type unit.
    let (%>) arg f =
        arg |> f; arg