# --------------------------------------------------------------------------------------------------
# This script runs all examples.
# --------------------------------------------------------------------------------------------------

$ErrorCont = 0
$SolutionDir = Split-Path $MyInvocation.MyCommand.Path -Parent


# --------------------------------------------------------------------------------------------------
Set-Location ${SolutionDir}\examples\List
Write-Host "Running List example..." -ForegroundColor Cyan
dotnet run .\ListTest.txt
if ($LASTEXITCODE -ne 0) {
    ++$ErrorCont    
}


# --------------------------------------------------------------------------------------------------
Set-Location ${SolutionDir}\examples\Calc
Write-Host "Running Calc example..." -ForegroundColor Cyan
dotnet run .\CalcTest.txt
if ($LASTEXITCODE -ne 0) {
    ++$ErrorCont    
}


# --------------------------------------------------------------------------------------------------
Set-Location ${SolutionDir}\examples\oberon2
Write-Host "Running Oberon-2 example..." -ForegroundColor Cyan
Get-ChildItem .\Oberon2Source\*.Mod |
ForEach-Object {
    Write-Host "    Parsing Oberon-2 source $($_.Name)..." -ForegroundColor Gray
    dotnet run $_.FullName
}
if ($LASTEXITCODE -ne 0) {
    ++$ErrorCont    
}


# --------------------------------------------------------------------------------------------------
Set-Location ${SolutionDir}\examples\BooleanParser
Write-Host "Running BooleanParser example..." -ForegroundColor Cyan
dotnet run .\BooleanParserTest.txt
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
    Write-Host "All examples successfully executed." -ForegroundColor Green
}

Set-Location $SolutionDir
