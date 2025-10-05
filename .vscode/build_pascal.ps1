param(
    [string]$CompilerPath = "$PWD\pascalwindows\bin\i386-win32\fpc.exe",
    [string]$Source = "$PWD\src\afn2afd.pas",
    [string]$OutDir = "$PWD\bin",
    [string]$IntermediateExe = "$PWD\src\afn2afd.exe",
    [string]$FinalExe = "$PWD\bin\afn2afd.exe"
)

Write-Host "Usando compilador: $CompilerPath"
Write-Host "Compilando $Source ..."

# Compute a repository-relative units path and pass it to fpc with -Fu so units like 'crt' are found.
# Derive $fpctarget from the compiler's bin folder name (e.g. 'i386-win32').
$compilerDir = Split-Path -Parent $CompilerPath
$fpctarget = Split-Path -Leaf $compilerDir
# units are expected at ../..\units\$fpctarget relative to the compiler bin folder
$repoRoot = Resolve-Path "$PWD"
$unitsPath = Join-Path -Path "$repoRoot" -ChildPath "pascalwindows\units\$fpctarget"

Write-Host "Using units path: $unitsPath"

# Build argument list: include -Fu with the computed units path and the source file.
$rtlPath = Join-Path -Path $unitsPath -ChildPath "rtl"
$rtlConsolePath = Join-Path -Path $unitsPath -ChildPath "rtl-console"

# Provide the main units folder and common RTL folders so 'crt' and others are found.
$fpcArgs = @(
    "-Fu$unitsPath",
    "-Fu$rtlPath",
    "-Fu$rtlConsolePath",
    "$Source"
)

Start-Process -NoNewWindow -Wait -FilePath $CompilerPath -ArgumentList $fpcArgs

if (Test-Path $IntermediateExe) {
    if (-not (Test-Path $OutDir)) { New-Item -ItemType Directory -Path $OutDir | Out-Null }
    Move-Item -Force $IntermediateExe $FinalExe
    Write-Host "Executable movido para: $FinalExe"
} else {
    Write-Host "Arquivo executável não encontrado em $IntermediateExe"
    exit 1
}
