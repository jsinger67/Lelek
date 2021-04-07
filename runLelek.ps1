
[CmdLetBinding()]
Param(
    [string] $Grammar = $(throw("Please give Grammar name as parameter")),
    [string] $GrammarLocation,
    [string] $OutputLocation
)

$SolutionDir=Get-Location
Set-Location .\src\Lelek

if ([string]::IsNullOrEmpty($GrammarLocation)) {
    $GrammarLocation = "${SolutionDir}\grammars"
}

if ([string]::IsNullOrEmpty($OutputLocation)) {
    $OutputLocation = "${SolutionDir}\examples\$Grammar"
}

if (-not [IO.Directory]::Exists($OutputLocation)) {
    New-Item -ItemType Directory -Path $OutputLocation -Verbose
}

$Prefix = [Char]::ToUpperInvariant($Grammar[0]) + $Grammar.Substring(1)
dotnet run -- --file "$GrammarLocation\$Grammar.llk" --generate-lexersrc "$OutputLocation\${Prefix}Lexer.fs" --generate-parsersrc "$OutputLocation\${Prefix}Parser.fs" --save-linear-grammar --asttype AST.AST

Set-Location $SolutionDir
