namespace LelekParser

module ParserGenLogger =
    open LANfa
    open LADfa
    open CompiledLADfa
    open System.IO
    open System.Text.RegularExpressions
    open LAAutomaton
    open GraphvisConversions

    type Logger = {
        /// logMainNfa "expression" nfa
        logMainNfa: string -> Nfa -> unit
        /// logSubNfa "expression" 0 nfa
        logSubNfa: string -> int -> Nfa -> unit
        /// logDfa "expression" dfa
        logDfa: string -> Dfa -> unit
        /// logCompiledDfa "expression" cdfa
        logCompiledDfa: (int -> string) -> string -> CompiledDfa -> unit
        /// log text
        log: string -> unit
        /// the input file
        inputFile: string
    }

    let provideLogger inputFile (logFileName: string) =
        let rec tryProvideLogger (logFileName: string) rep =
            try
                let logFolder = Path.GetDirectoryName(logFileName)
                let logToFile fN (msg: string) =
                    use sw = new StreamWriter(File.Open(logFolder + @"\" + fN, FileMode.Create))
                    sw.Write msg

                if Directory.Exists(logFolder) then
                    Directory.Delete(logFolder, true)
        
                Directory.CreateDirectory(logFolder) |> ignore

                let swLog = new StreamWriter(File.Open(logFileName, FileMode.Create))

                {
                    logMainNfa = (fun title nfa ->
                                    let modTitle = (sprintf "%s_nfa" title)
                                    nfa
                                    |> Nfa.toDot modTitle (LAItem.toString (sprintf " %d "))
                                    |> logToFile (modTitle + ".dot"))
                    logSubNfa = (fun title num nfa ->
                                    let modTitle = (sprintf "%s_contr%d_nfa" title num)
                                    nfa
                                    |> Nfa.toDot modTitle (LAItem.toString (sprintf " %d "))
                                    |> logToFile (modTitle + ".dot"))
                    logDfa = (fun title dfa ->
                                    let modTitle = (sprintf "%s_dfa" title)
                                    dfa
                                    |> Dfa.toDot modTitle (LAItem.toString (sprintf " %d "))
                                    |> logToFile (modTitle + ".dot"))
                    logCompiledDfa = (fun printToken title cdfa ->
                                    let modTitle = (sprintf "%s_cdfa" title)
                                    cdfa
                                    |> CompiledDfa.toDot modTitle printToken
                                    |> logToFile (modTitle + ".dot"))
                    log = (fun msg -> swLog.WriteLine msg; swLog.Flush())

                    inputFile = inputFile
                }
            with
                | :? IOException ->
                    let logFolder = Path.GetDirectoryName(logFileName)
                    let logFile = Path.GetFileName(logFileName)
                    let m = Regex("\.\d+$").Match(logFolder)
                    if m.Success then
                        let newFileName = sprintf "%s.%d%c%s" (logFolder.Substring(0, m.Index)) rep (Path.DirectorySeparatorChar) logFile
                        tryProvideLogger newFileName (rep + 1)
                    else
                        let newFileName = sprintf "%s.%d%c%s" logFolder rep (Path.DirectorySeparatorChar) logFile
                        tryProvideLogger newFileName (rep + 1)

        tryProvideLogger logFileName 0
