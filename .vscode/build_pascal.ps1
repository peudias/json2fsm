param(
    [string]$CompilerPath = "$PWD\pascalwindows\bin\i386-win32\fpc.exe",
    [string]$Source = "$PWD\src\hello.pas",
    [string]$OutDir = "$PWD\bin",
    [string]$IntermediateExe = "$PWD\src\hello.exe",
    [string]$FinalExe = "$PWD\bin\hello.exe"
)

Write-Host "Usando compilador: $CompilerPath"
Write-Host "Compilando $Source ..."

Start-Process -NoNewWindow -Wait -FilePath $CompilerPath -ArgumentList "$Source"

if (Test-Path $IntermediateExe) {
    if (-not (Test-Path $OutDir)) { New-Item -ItemType Directory -Path $OutDir | Out-Null }
    Move-Item -Force $IntermediateExe $FinalExe
    Write-Host "Executable movido para: $FinalExe"
} else {
    Write-Host "Arquivo executável não encontrado em $IntermediateExe"
    exit 1
}
