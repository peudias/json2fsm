# Script para instalar o Lazarus IDE a partir do arquivo local
# Requisitos: PowerShell 5.1+

Write-Host ""
Write-Host "=================================================" -ForegroundColor Cyan
Write-Host "   Instalacao do Lazarus IDE" -ForegroundColor Cyan
Write-Host "=================================================" -ForegroundColor Cyan
Write-Host ""

# Verificar se Lazarus ja esta instalado
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
        Write-Host "OK - Lazarus ja esta instalado em: $path" -ForegroundColor Green
        Write-Host ""
        Write-Host "Deseja reinstalar? (S/N)" -ForegroundColor Yellow
        $response = Read-Host
        if ($response -ne "S" -and $response -ne "s") {
            Write-Host ""
            Write-Host "OK - Usando Lazarus existente." -ForegroundColor Green
            Write-Host ""
            Write-Host "Para compilar o projeto, execute:" -ForegroundColor Cyan
            Write-Host "  .\install_and_compile_gui.ps1" -ForegroundColor White
            Write-Host ""
            pause
            exit 0
        }
        break
    }
}

# URL e caminho do instalador
# Usando mirror direto do SourceForge para evitar redirecionamentos
$downloadUrl = "https://downloads.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%204.4/lazarus-4.4-fpc-3.2.2-win64.exe"
$installerPath = ".\lazarus-4.4-fpc-3.2.2-win64.exe"

# Verificar se o instalador ja existe localmente
if (-not (Test-Path $installerPath)) {
    Write-Host "Instalador nao encontrado localmente." -ForegroundColor Yellow
    Write-Host "Iniciando download do Lazarus 4.4..." -ForegroundColor Cyan
    Write-Host ""
    Write-Host "URL: $downloadUrl" -ForegroundColor White
    Write-Host "Destino: $installerPath" -ForegroundColor White
    Write-Host ""
    Write-Host "Aguarde... (arquivo ~270 MB, pode levar alguns minutos)" -ForegroundColor Yellow
    Write-Host ""
    
    try {
        # Baixar o instalador usando WebClient (segue redirecionamentos automaticamente)
        Write-Host "Conectando ao SourceForge..." -ForegroundColor Cyan
        $webClient = New-Object System.Net.WebClient
        $fullPath = Join-Path (Get-Location) "lazarus-4.4-fpc-3.2.2-win64.exe"
        $webClient.DownloadFile($downloadUrl, $fullPath)
        
        # Verificar se o arquivo foi baixado com tamanho correto
        if (Test-Path $installerPath) {
            $downloadedSize = (Get-Item $installerPath).Length / 1MB
            if ($downloadedSize -lt 100) {
                Write-Host ""
                Write-Host "AVISO: Arquivo baixado muito pequeno ($([math]::Round($downloadedSize, 2)) MB)" -ForegroundColor Yellow
                Write-Host "Isso pode indicar um problema no download." -ForegroundColor Yellow
                Write-Host ""
                Remove-Item $installerPath -Force
                throw "Download invalido - arquivo muito pequeno"
            }
        }
        
        Write-Host "OK - Download concluido!" -ForegroundColor Green
        Write-Host ""
        
    } catch {
        Write-Host ""
        Write-Host "ERRO ao baixar o instalador: $_" -ForegroundColor Red
        Write-Host ""
        Write-Host "Alternativas:" -ForegroundColor Yellow
        Write-Host "   1. Baixe manualmente de: https://www.lazarus-ide.org/" -ForegroundColor White
        Write-Host "   2. Salve como: lazarus-4.4-fpc-3.2.2-win64.exe neste diretorio" -ForegroundColor White
        Write-Host "   3. Execute este script novamente" -ForegroundColor White
        Write-Host ""
        pause
        exit 1
    }
} else {
    Write-Host "Instalador encontrado localmente!" -ForegroundColor Green
    Write-Host ""
}

$fileSize = (Get-Item $installerPath).Length / 1MB
Write-Host "Arquivo: lazarus-4.4-fpc-3.2.2-win64.exe" -ForegroundColor Green
Write-Host "Tamanho: $([math]::Round($fileSize, 2)) MB" -ForegroundColor Cyan
Write-Host ""

Write-Host "Iniciando instalador..." -ForegroundColor Yellow
Write-Host ""
Write-Host "INSTRUCOES DE INSTALACAO:" -ForegroundColor Cyan
Write-Host "   1. Aceite a licenca" -ForegroundColor White
Write-Host "   2. Mantenha o caminho padrao: C:\lazarus" -ForegroundColor White
Write-Host "   3. Instale todos os componentes" -ForegroundColor White
Write-Host "   4. Aguarde a instalacao (~2-3 minutos)" -ForegroundColor White
Write-Host ""
Write-Host "IMPORTANTE: Instale em C:\lazarus para as tasks do VS Code funcionarem!" -ForegroundColor Yellow
Write-Host ""

try {
    # Executar instalador
    Start-Process -FilePath $installerPath -Wait
    
    Write-Host ""
    Write-Host "OK - Instalacao concluida!" -ForegroundColor Green
    Write-Host ""
    
    # Verificar se foi instalado
    $newLazPath = "C:\lazarus\lazbuild.exe"
    if (Test-Path $newLazPath) {
        Write-Host "OK - Lazarus instalado com sucesso em C:\lazarus!" -ForegroundColor Green
        Write-Host ""
        Write-Host "Proximos passos:" -ForegroundColor Cyan
        Write-Host "   1. Execute: .\install_and_compile_gui.ps1" -ForegroundColor White
        Write-Host "   2. Ou use Ctrl+Shift+B no VS Code" -ForegroundColor White
    } else {
        Write-Host "AVISO - Nao foi possivel verificar a instalacao em C:\lazarus" -ForegroundColor Yellow
        Write-Host "   Verifique se o Lazarus foi instalado corretamente." -ForegroundColor Yellow
    }
    
} catch {
    Write-Host ""
    Write-Host "ERRO ao executar instalador: $_" -ForegroundColor Red
}

Write-Host ""
Write-Host "=================================================" -ForegroundColor Cyan
Write-Host ""
pause
