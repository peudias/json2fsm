# 🐛 Troubleshooting

Guia de solução de problemas comuns.

## 🚨 Erros de Compilação

### ❌ "Lazarus não encontrado ao compilar GUI"

**Sintoma:**
```
'C:\lazarus\lazbuild.exe' is not recognized...
```

**Causas:**
1. Lazarus não instalado
2. Lazarus instalado em local diferente
3. Tasks do VS Code com caminho errado

**Soluções:**

<!-- tabs:start -->

#### **Solução 1: Instalar Lazarus**

```powershell
# 1. Baixar Lazarus
# https://www.lazarus-ide.org/

# 2. Instalar em C:\lazarus (padrão)

# 3. Verificar instalação
Test-Path "C:\lazarus\lazbuild.exe"
# Deve retornar: True
```

#### **Solução 2: Ajustar Caminho**

```powershell
# 1. Abra .vscode\tasks.json

# 2. Encontre a linha:
"command": "C:\\lazarus\\lazbuild.exe",

# 3. Substitua pelo caminho correto:
"command": "SEU_CAMINHO\\lazbuild.exe",
```

#### **Solução 3: Variável de Ambiente**

```powershell
# Adicionar ao PATH
$env:PATH += ";C:\lazarus"

# Ou permanentemente:
[Environment]::SetEnvironmentVariable(
  "Path",
  $env:Path + ";C:\lazarus",
  "User"
)
```

<!-- tabs:end -->

---

### ❌ "Can't find unit Interfaces"

**Sintoma:**
```
Fatal: Can't find unit Interfaces used by afn2afdgui
```

**Causa:** Tentando compilar GUI sem Lazarus instalado

**Solução:**
```powershell
# Instale o Lazarus IDE
# Não tente compilar apenas com FPC
```

---

### ❌ "Error: Identifier not found 'SetName'"

**Sintoma:**
```
MainForm.pas(45,12) Error: Identifier not found "SetName"
```

**Causa:** Versão antiga do código (bug corrigido)

**Solução:**
```bash
# Atualize para última versão
git pull origin master

# Recompile
C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi
```

---

### ❌ "fpc.exe not found"

**Sintoma:**
```
The compiler "fpc.exe" was not found in the PATH
```

**Causa:** Free Pascal Compiler não instalado corretamente com Lazarus

**Solução:**
```powershell
# 1. Reinstalar Lazarus
# 2. Durante instalação, verificar que FPC será instalado
# 3. Aceitar todas opções padrão

# Verificar após instalação:
Test-Path "C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe"
```

---

## 🖥️ Problemas de Execução

### ❌ GUI não abre após compilar

**Sintoma:** Executável compila mas nada acontece

**Debug:**
```powershell
# 1. Verificar se executável existe
Test-Path ".\bin\afn2afdgui.exe"

# 2. Executar manualmente no terminal
.\bin\afn2afdgui.exe

# 3. Ver mensagem de erro (se houver)
```

**Soluções Comuns:**

1. **Falta DLL:**
   ```
   Erro: "The program can't start because xxx.dll is missing"
   
   Solução: Compile com --build-mode=Release
   ```

2. **Antivírus bloqueando:**
   ```
   Adicione exceção para .\bin\afn2afdgui.exe
   ```

3. **Permissões:**
   ```powershell
   # Executar como Administrador
   Start-Process .\bin\afn2afdgui.exe -Verb RunAs
   ```

---

### ❌ "Access violation" ao converter

**Sintoma:** Programa trava ao clicar em "Converter"

**Causas:**
1. AFN inválido
2. Bug no código
3. Memória corrompida

**Soluções:**

```powershell
# 1. Teste com arquivo exemplo
.\bin\afn2afdgui.exe
# Carregar testes\test_ab.txt

# 2. Se funciona, problema é no seu arquivo de entrada

# 3. Valide formato:
# - Linha 1: alfabeto
# - Linha 2: estados
# - Linha 3: iniciais
# - Linha 4: finais
# - Linhas 5+: transições
```

---

### ❌ Diagrama não aparece

**Sintoma:** Resultado textual OK, mas diagramas vazios

**Causas:**
1. Bug na renderização
2. Estados não estão posicionados
3. Canvas não foi invalidado

**Solução:**
```powershell
# 1. Reabrir aplicação
.\bin\afn2afdgui.exe

# 2. Carregar arquivo novamente

# 3. Converter novamente

# 4. Alternar entre abas:
# "Diagrama AFN" ↔ "Diagrama AFD"
```

---

## ⚙️ Problemas do VS Code

### ❌ Tasks não aparecem no menu

**Sintoma:** Ctrl+Shift+B não mostra tasks customizadas

**Soluções:**

1. **Recarregar VS Code:**
   ```
   Ctrl+Shift+P → "Reload Window"
   ```

2. **Fechar e reabrir:**
   ```
   File → Close Folder
   File → Open Folder → Selecionar json2fsm
   ```

3. **Verificar tasks.json:**
   ```powershell
   Test-Path ".vscode\tasks.json"
   # Deve existir
   ```

4. **Validar JSON:**
   ```
   Abra .vscode\tasks.json
   Veja se há erros de sintaxe
   ```

---

### ❌ "Execution Policy" no PowerShell

**Sintoma:**
```
.\install_and_compile_gui.ps1 : File cannot be loaded because
running scripts is disabled on this system
```

**Causa:** PowerShell bloqueando execução de scripts

**Solução:**
```powershell
# Temporário (sessão atual):
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass

# Permanente (usuário atual):
Set-ExecutionPolicy -Scope CurrentUser -ExecutionPolicy RemoteSigned

# Verificar:
Get-ExecutionPolicy -List
```

---

## 📊 Problemas de Performance

### 🐌 Conversão muito lenta

**Sintoma:** Conversão leva >10 segundos

**Causas:**
1. AFN muito grande
2. Explosão exponencial de estados
3. Computador lento

**Soluções:**

```powershell
# 1. Ver quantos estados tem o AFN
# No arquivo: linha 2

# 2. Se >15 estados, considere simplificar

# 3. Teste com arquivo menor primeiro:
# testes\test_ab.txt (3 estados)
```

**Dica:** Use `test_nao_det.txt` para testar limites (4→8 estados)

---

### 💾 Consumo de memória alto

**Sintoma:** Aplicação usando >500MB RAM

**Causa:** Muitos estados compostos no DFA

**Solução:**
```powershell
# Normal para AFNs grandes
# Feche e reabra se necessário
```

---

## 🎨 Problemas de Interface

### 🖼️ Janela muito grande/pequena

**Solução:**
```
1. Arraste cantos da janela para redimensionar
2. Arraste splitter vertical para ajustar painéis
3. Tamanho padrão: 1200x600px
```

---

### 📄 Texto cortado ou sobreposto

**Causa:** Fontes do sistema diferentes

**Solução:**
```powershell
# Recompile com modo Release
C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi
```

---

### 🎨 Cores estranhas no diagrama

**Causa:** Configurações de alto contraste do Windows

**Solução:**
```
Windows Settings → Accessibility → High Contrast
Desabilitar ou ajustar
```

---

## 🌐 Problemas de Documentação

### ❌ Docsify não carrega

**Sintoma:** Ao abrir `docs/index.html`, página em branco

**Causas:**
1. Bloqueio CORS (abrindo localmente)
2. JavaScript desabilitado
3. Arquivos não estão no lugar certo

**Soluções:**

<!-- tabs:start -->

#### **Solução 1: Servidor Local**

```bash
# Instalar docsify-cli
npm i docsify-cli -g

# Servir documentação
cd c:\Users\Henrique\CEFET\json2fsm
docsify serve docs

# Abrir: http://localhost:3000
```

#### **Solução 2: GitHub Pages**

```bash
# Push para GitHub
git add docs/
git commit -m "docs: adiciona documentação Docsify"
git push origin master

# Configurar GitHub Pages:
# Repo → Settings → Pages → Source: docs/
```

#### **Solução 3: Extensão VS Code**

```
1. Instalar "Live Server" no VS Code
2. Clicar direito em docs/index.html
3. "Open with Live Server"
```

<!-- tabs:end -->

---

## 🔧 Outros Problemas

### ❌ Git não encontrado

**Sintoma:**
```
'git' is not recognized as an internal or external command
```

**Solução:**
```powershell
# Baixar e instalar Git
# https://git-scm.com/download/win

# Verificar instalação
git --version
```

---

### ❌ VS Code não abre o projeto

**Sintoma:** Erro ao abrir pasta

**Solução:**
```powershell
# Executar VS Code como Administrador
# Ou verificar permissões da pasta
```

---

## 📝 Reportar Novos Problemas

**Não encontrou solução?**

1. **Verifique Issues existentes:**
   https://github.com/peudias/json2fsm/issues

2. **Abra novo issue:**
   - Descreva o problema
   - Inclua mensagens de erro
   - Especifique ambiente (Windows, Lazarus, FPC versions)
   - Passos para reproduzir

3. **Template de report:**
   ```markdown
   **Problema:**
   Descrição clara do problema

   **Passos para reproduzir:**
   1. Fazer X
   2. Fazer Y
   3. Ver erro Z

   **Esperado:**
   O que deveria acontecer

   **Ambiente:**
   - Windows: 11
   - Lazarus: 3.6
   - FPC: 3.2.2

   **Logs/Screenshots:**
   [anexar aqui]
   ```

---

## 🆘 Ajuda Adicional

- 📚 [FAQ](faq.md)
- 💬 [GitHub Discussions](https://github.com/peudias/json2fsm/discussions)
- 📧 Contato: [peudias](https://github.com/peudias)

---

**💡 Dica:** Sempre teste com arquivos de exemplo (`testes/`) antes de reportar bug!
