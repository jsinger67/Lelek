namespace LelekParser

module TokenStream =
    open Token
    open System.IO

    type TokenStream = {
        /// The number of availabe lookahead tokens
        k: int
        /// The file name that is parsed
        fileName: string
        /// Provides at maximum k tokens lookahead
        lookahead: int -> Token
        /// Advances the 'lookahead window' for the given numer of tokens
        consume: int -> unit
        /// Test if alle input was processed by the parser
        allInputConsumed: unit -> bool
    }


    let provideTokenStream (lexer: string -> Token seq) (fileName) (k: int) =
        use sr = new StreamReader(File.OpenRead(fileName))
        /// TODO: This is rather quick and dirty - but sufficient fast.
        ///       Think about a squential handling to possibly improve performance.
        let tokens = sr.ReadToEnd() |> lexer |> Array.ofSeq
        let mutable pos = 0

        {
            k = k
            fileName = Path.GetFullPath(fileName)
            lookahead = fun i -> tokens.[pos + i]
            consume = fun n -> pos <- pos  + n
            allInputConsumed = fun () -> pos >= tokens.Length - k - 1
        }
