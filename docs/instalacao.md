# 📦 Instalação

Este guia te levará do zero até executar a aplicação em **menos de 10 minutos**.

## 📋 Pré-requisitos

Antes de começar, você precisa ter:

| Item | Versão | Status |
|------|--------|--------|
| Windows | 10 ou 11 | ✅ Obrigatório |
| PowerShell | 5.1+ | ✅ Obrigatório |
| Visual Studio Code | Última | 🔵 Recomendado |
| Git | 2.0+ | 🔵 Recomendado |

## 🎯 Instalação Rápida

### Passo 1: Clonar o Repositório

```powershell
# Clone o repositório
git clone https://github.com/peudias/json2fsm.git

# Entre no diretório
cd json2fsm
```

### Passo 2: Instalar Lazarus IDE

O Lazarus IDE já inclui o Free Pascal Compiler, e o instalador está incluído no repositório!

<!-- tabs:start -->

#### **Script Automático ⭐**

```powershell
# Execute o script de instalação
.\download_install_lazarus.ps1
```

O script irá:
- ✅ Verificar se o Lazarus já está instalado
- ✅ Instalar `lazarus-4.4-fpc-3.2.2-win64.exe` em `C:\lazarus`
- ✅ Confirmar a instalação

#### **Instalação Manual**

```powershell
# Execute o instalador incluído no repositório
.\lazarus-4.4-fpc-3.2.2-win64.exe
```

Ou baixe a versão mais recente:
1. Acesse: [https://www.lazarus-ide.org/](https://www.lazarus-ide.org/)
2. Baixe: **Lazarus 4.4 - Windows 64-bit**
3. Execute o instalador

<!-- tabs:end -->

**⚙️ Configurações de Instalação:**
- 📁 **Diretório:** `C:\lazarus` (padrão recomendado)
- ⏱️ **Tempo:** ~2-3 minutos
- 💾 **Espaço:** ~800 MB
- 📦 **Versão:** Lazarus 4.4 com FPC 3.2.2

> ⚠️ **IMPORTANTE:** Instale em `C:\lazarus` para as tasks do VS Code funcionarem automaticamente!

### Passo 3: Compilar o Projeto

<!-- tabs:start -->

#### **Via VS Code ⭐**

```powershell
# 1. Abrir no VS Code
code .

# 2. Apertar Ctrl+Shift+B
# 3. Escolher: "🎨 GUI: Compilar e Executar"
```

#### **Via Script PowerShell**

```powershell
# Executar script de compilação
.\install_and_compile_gui.ps1
```

#### **Via Linha de Comando**

```powershell
# Compilar manualmente
C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi

# Executar
.\bin\afn2afdgui.exe
```

<!-- tabs:end -->

## ✅ Verificação da Instalação

Execute este script para verificar se tudo está OK:

```powershell
# Verificar se Lazarus foi instalado
Test-Path "C:\lazarus\lazbuild.exe"

# Verificar se o executável foi compilado
Test-Path ".\bin\afn2afdgui.exe"

# Se ambos retornarem True, está tudo OK! ✅
```

## 🎮 Primeiro Uso

Após a instalação, execute:

```powershell
.\bin\afn2afdgui.exe
```

Você verá a interface com um AFN de exemplo já carregado. Clique em **"🔄 Converter AFN → AFD"** e veja a mágica acontecer! ✨

## 🐛 Problemas na Instalação?

### ❌ Erro: "Lazarus não encontrado"

**Causa:** Lazarus não está em `C:\lazarus`

**Solução:**
```powershell
# Edite .vscode\tasks.json e ajuste o caminho:
# Linha ~7:
"command": "SEU_CAMINHO\\lazbuild.exe",
```

### ❌ Erro: "Execution Policy"

**Causa:** PowerShell bloqueando scripts

**Solução:**
```powershell
Set-ExecutionPolicy -Scope CurrentUser -ExecutionPolicy RemoteSigned
```

### ❌ Erro: "Can't find unit Interfaces"

**Causa:** Tentando compilar sem Lazarus instalado

**Solução:** Instale o Lazarus IDE primeiro

### ❌ Erro: "fpc.exe not found"

**Causa:** Lazarus não instalou o Free Pascal corretamente

**Solução:** Reinstale o Lazarus

## 📊 Requisitos de Sistema

| Componente | Mínimo | Recomendado |
|------------|--------|-------------|
| **RAM** | 2 GB | 4 GB |
| **Espaço em disco** | 1 GB | 2 GB |
| **Processador** | Intel Core i3 | Intel Core i5+ |
| **Tela** | 1024x768 | 1920x1080 |

## 🔄 Atualizando

Para atualizar para a versão mais recente:

```powershell
# Puxar última versão
git pull origin master

# Recompilar
C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi
```

## 🗑️ Desinstalação

Para remover completamente:

```powershell
# 1. Desinstalar Lazarus
# Painel de Controle → Programas → Lazarus 4.4

# 2. Remover diretório do projeto
cd ..
Remove-Item -Recurse -Force json2fsm
```

## 🎯 Próximos Passos

Agora que você instalou com sucesso:

- 📖 [Leia o Guia de Uso](uso.md)
- 🧪 [Explore os Casos de Teste](testes.md)
- 🔬 [Entenda o Algoritmo](algoritmo.md)

---

**💡 Dica:** Marque esta página nos favoritos para referência futura!
