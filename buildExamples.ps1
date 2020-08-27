# --------------------------------------------------------------------------------------------------
# You don't need to call this script regularily because the generated sources are under source control.
# But if you change examples and want to test the results this script helps you.
# --------------------------------------------------------------------------------------------------

$ErrorCont = 0
$SolutionDir = Split-Path $MyInvocation.MyCommand.Path -Parent

Set-Location ${SolutionDir}\src\Lelek

# --------------------------------------------------------------------------------------------------
Write-Host "Building List example..." -ForegroundColor Cyan
dotnet run -- --file ${SolutionDir}\grammars\lst.llk --generate-lexersrc ${SolutionDir}\examples\List\ListLexer.fs --generate-parsersrc ${SolutionDir}\examples\List\ListParser.fs --save-linear-grammar --asttype AST.AST
if ($LASTEXITCODE -ne 0) {
    ++$ErrorCont    
}


# --------------------------------------------------------------------------------------------------
Write-Host "Building Calc example..." -ForegroundColor Cyan
dotnet run -- --file ${SolutionDir}\grammars\Calc.llk --generate-lexersrc ${SolutionDir}\examples\Calc\CalcLexer.fs --generate-parsersrc ${SolutionDir}\examples\Calc\CalcParser.fs --save-linear-grammar --asttype AST.AST
if ($LASTEXITCODE -ne 0) {
    ++$ErrorCont    
}

# --------------------------------------------------------------------------------------------------
Write-Host "Building Oberon-2 example..." -ForegroundColor Cyan
dotnet run -- --file ${SolutionDir}\grammars\oberon-2.llk --generate-lexersrc ${SolutionDir}\examples\oberon2\Oberon2Lexer.fs --generate-parsersrc ${SolutionDir}\examples\oberon2\Oberon2Parser.fs --save-linear-grammar --asttype AST.AST --srcnamespacename Oberon2 --lexermodulename Oberon2Lexer --parsermodulename Oberon2Parser
if ($LASTEXITCODE -ne 0) {
    ++$ErrorCont    
}

# --------------------------------------------------------------------------------------------------
Write-Host "Building BooleanParser example..." -ForegroundColor Cyan
dotnet run -- --file ${SolutionDir}\examples\BooleanParser\BooleanParser.llk --generate-lexersrc ${SolutionDir}\examples\BooleanParser\Lexer.fs --generate-parsersrc ${SolutionDir}\examples\BooleanParser\Parser.fs --save-linear-grammar --asttype AST.AST --srcnamespacename BP --lexermodulename BPLexer --parsermodulename BPParser
if ($LASTEXITCODE -ne 0) {
    ++$ErrorCont    
}

# --------------------------------------------------------------------------------------------------
Write-Host "Building JsonParser example..." -ForegroundColor Cyan
dotnet run -- --file ${SolutionDir}\examples\JsonParser\json.llk --generate-lexersrc ${SolutionDir}\examples\JsonParser\Lexer.fs --generate-parsersrc ${SolutionDir}\examples\JsonParser\Parser.fs --save-linear-grammar --asttype AST.AST --srcnamespacename JP --lexermodulename JPLexer --parsermodulename JPParser
if ($LASTEXITCODE -ne 0) {
    ++$ErrorCont    
}

# --------------------------------------------------------------------------------------------------
# Final message
# --------------------------------------------------------------------------------------------------
if ($ErrorCont -gt 0) {
    $Msg = "$ErrorCount error(s) occurred."
    Write-Host -Object $Msg  -ForegroundColor Red
} else {
    Write-Host "All examples successfully built." -ForegroundColor Green
}

Set-Location $SolutionDir
