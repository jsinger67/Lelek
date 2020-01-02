namespace LelekParser


module LexerSrcGen =
    open DotLiquid
    open ParserGenLogger
    open LlkGrammar
    open LinLlkGrammar
    open LexerTerminals
    open System.IO
    open System.Text.RegularExpressions
    open System.Reflection

    type LexerLiquidData = {
        name_space_name : string
        module_name : string
        terminals : string list
        rx_string : string
        token_types : string list
        line_comment : bool
        block_comment : bool
    }

    let private checkRx (logger: Logger) (g: LinLlkData) : Result<string, string> =

        let terminals = terminals g
        let errs =
            terminals
            |> List.map (fun rx ->
                try
                    Regex(rx, RegexOptions.Compiled) |> ignore
                    ""
                with
                | :? System.ArgumentException as ex ->
                    ex.Message
            )
            |> List.filter (fun s -> s |> String.length > 0)

        "Tokens:" |> logger.log
        "type TokenType =" |> logger.log
        match errs with
        | []    ->
            let rxStr =
                g
                |> augmentedTerminals
                |> List.mapi (fun i s ->
                    let rx = s |> Utils.escapeVerbatim
                    if i > 1 then
                        rx |> (sprintf "    | Token%d = %4d // \"%s\"" i i) |> logger.log
                    rx |> (sprintf "(?<G%d>%s)" i))
                |> String.concat "|"
            Ok rxStr
        | _     ->
            let errTxt = errs |> String.concat "\n"
            errTxt |> logger.log
            errTxt |> Error


    let private rxEscQuote = Regex(@"(?<!\\)""", RegexOptions.Compiled)

    let generateLexerSrc (logger: Logger) (g: LinLlkData) (fileName: string) (nameSpaceName: string) (moduleName: string): Result<unit, string> =

        let (LinLlkData(CommentDcl = cm)) = g

        let escape s =
            rxEscQuote.Replace(s, @"\""")

        let terminalList =
            g
            |> augmentedTerminals
            |> List.map (escape >> (sprintf @"""%s"""))

        let tokenTypes =
            let lnTk = (((g |> terminalTypNames |> List.map (fun s -> s.Length) |> List.max) / 4) + 1) * 4
            let pad (s: string): string =
                s + (System.String(' ', lnTk - s.Length))
            List.zip (g |> terminalTypNames) (g |> augmentedTerminals)
            |> List.mapi (fun i (tt, t) -> sprintf "| %s = %3d // \"%s\"" (pad tt) i t)

        let buildLexerSrc rx =
            let data = {
                name_space_name = nameSpaceName
                module_name = moduleName
                terminals = terminalList
                rx_string = rx
                token_types = tokenTypes
                line_comment = CommentDecl.hasLineComment cm
                block_comment = CommentDecl.hasBlockComment cm
            }
            let folder = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
            use sr = new StreamReader(File.OpenRead(folder + @"/Templates/LexerSrc.liquid"))            
            let template = Template.Parse(sr.ReadToEnd())
            let compiledSrc = template.Render(Hash.FromAnonymousObject(data))
            use swLexerSrc = new StreamWriter(File.Open(fileName, FileMode.Create))
            swLexerSrc.Write compiledSrc

        g
        |> checkRx logger
        |> Result.map buildLexerSrc

