# Script para compilar o projeto Lazarus após instalação
# Execute este script APÓS instalar o Lazarus

Write-Host "`n==================================================" -ForegroundColor Cyan
Write-Host "   Compilador GUI AFN->AFD com Lazarus" -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan

# Procurar Lazarus instalado
$lazPaths = @(
    "C:\lazarus",
    "C:\Program Files\Lazarus",
    "C:\Program Files (x86)\Lazarus",
    "$env:ProgramFiles\Lazarus",
    "$env:LOCALAPPDATA\Lazarus"
)

$lazbuildPath = $null
foreach ($path in $lazPaths) {
    $testPath = Join-Path $path "lazbuild.exe"
    if (Test-Path $testPath) {
        $lazbuildPath = $testPath
        Write-Host "`n✅ Lazarus encontrado em: $path" -ForegroundColor Green
        break
    }
}

if (-not $lazbuildPath) {
    Write-Host "`n❌ ERRO: Lazarus não encontrado!" -ForegroundColor Red
    Write-Host "`nPor favor, instale o Lazarus primeiro:" -ForegroundColor Yellow
    Write-Host "https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%203.6/" -ForegroundColor Cyan
    Write-Host "`nApós instalar, execute este script novamente." -ForegroundColor Yellow
    pause
    exit 1
}

Write-Host "`nlazbuild encontrado: $lazbuildPath" -ForegroundColor Green

# Verificar se o projeto existe
$projectPath = ".\src\afn2afdgui.lpi"
if (-not (Test-Path $projectPath)) {
    Write-Host "`n❌ ERRO: Projeto não encontrado em $projectPath" -ForegroundColor Red
    pause
    exit 1
}

Write-Host "`n🔨 Compilando projeto..." -ForegroundColor Yellow
Write-Host "Projeto: $projectPath" -ForegroundColor Gray

# Compilar
& $lazbuildPath --build-mode=Release $projectPath

if ($LASTEXITCODE -eq 0) {
    Write-Host "`n✅ Compilação concluída com sucesso!" -ForegroundColor Green
    
    $exePath = ".\bin\afn2afdgui.exe"
    if (Test-Path $exePath) {
        Write-Host "`n🎉 Executável criado: $exePath" -ForegroundColor Green
        Write-Host "`n🚀 Deseja executar agora? (S/N)" -ForegroundColor Cyan
        $response = Read-Host
        
        if ($response -eq 'S' -or $response -eq 's' -or $response -eq 'Y' -or $response -eq 'y') {
            Write-Host "`n▶️  Iniciando aplicação GUI..." -ForegroundColor Yellow
            Start-Process $exePath
            Write-Host "`n✅ Aplicação iniciada!" -ForegroundColor Green
        }
    } else {
        Write-Host "`n⚠️  Executável não encontrado em $exePath" -ForegroundColor Yellow
    }
} else {
    Write-Host "`n❌ Erro na compilação! Código de saída: $LASTEXITCODE" -ForegroundColor Red
}

Write-Host "`n==================================================" -ForegroundColor Cyan
pause
