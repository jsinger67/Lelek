namespace LelekParser

module ParserFeedback =
    open System
    open Token
    open CompiledLADfa

    type ParserFeedback = {
        /// Outputs an error message
        printMessage: string -> unit
        /// Outputs a warning
        printWarning: string -> unit
        /// Outputs an error message
        printError: string -> unit
        /// Converts a token type to a string
        printTokenError: State -> Token -> unit
        /// Returns the number of issued warnings
        warningCount: unit -> int
        /// Returns the number of issued errors
        errorCount: unit -> int
    }

    let provideParserFeedback printTokenError =
        let mutable warnings = 0
        let mutable errors = 0


        let printColored color txt =
            let lastfgColor = Console.ForegroundColor
            Console.ForegroundColor <- color
            printfn "%s" txt
            Console.ForegroundColor <- lastfgColor

        let printRed =
            printColored ConsoleColor.Red

        let printCyan =
            printColored ConsoleColor.Cyan

        let printError s =
            errors <- errors + 1
            printRed s

        {
            printMessage = Console.WriteLine
            
            printWarning = (fun s ->
                warnings <- warnings + 1
                printCyan s)
            
            printError = printError
            
            printTokenError = (fun dfaState token ->
                errors <- errors + 1
                printTokenError dfaState token
                |> printError
            )
            
            warningCount = (fun () -> warnings)
            
            errorCount = (fun () -> errors)
        }
