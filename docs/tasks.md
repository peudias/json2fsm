# ⚙️ Tasks do VS Code

Este documento descreve todas as tasks disponíveis no projeto para compilação e execução.

## 🎯 Tasks Disponíveis

### 🎨 GUI: Compilar

Compila o projeto da interface gráfica usando o Lazarus.

**Como usar:**
```powershell
# Via menu de tasks (Ctrl+Shift+B)
Selecione: "🎨 GUI: Compilar"

# Ou via terminal
C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi
```

**O que faz:**
- Compila `src/afn2afdgui.lpi` no modo Release
- Gera arquivos intermediários em `src/lib/`
- Cria o executável final em `bin/afn2afdgui.exe`
- Otimizado para performance (nível -O3)

**Saída esperada:**
```
Success: Project "afn2afdgui.lpi" compiled successfully.
```

---

### 🎨 GUI: Executar

Executa a aplicação GUI compilada.

**Como usar:**
```powershell
# Via menu de tasks
Selecione: "🎨 GUI: Executar"

# Ou diretamente
.\bin\afn2afdgui.exe
```

**Pré-requisito:**
- O projeto deve estar compilado (`bin/afn2afdgui.exe` deve existir)

---

### 🔍 GUI: Executar com Log 📋

Executa a aplicação GUI com **terminal dedicado para logs detalhados**.

**Como usar:**
```powershell
# Via menu de tasks
Selecione: "🔍 GUI: Executar com Log"
```

**Características:**
- Terminal dedicado que permanece visível
- Mostra logs detalhados de conversão AFN → AFD
- Ideal para debugging e verificação de algoritmo

**Exemplo de saída:**
```
=====================================
  AFN → AFD Converter (GUI Mode)
=====================================

[GUI] Aplicacao iniciada
[GUI] 9 arquivos de teste carregados
[GUI] Arquivo selecionado: test_nao_det.txt

[GUI] AFN DE ENTRADA:
  Alfabeto: a b
  Estados: q0 q1 q2 q3
  Estados iniciais: q0
  Estados finais: q3
  Transicoes: 8

[GUI] Iniciando conversao AFN -> AFD...
[GUI] Processando estados compostos...
[GUI] Estados gerados: 8

[GUI] AFD RESULTANTE:
  Estados: 8
  Estado inicial: {q0}
  Estados finais: 4
  Transicoes: 16

[GUI] Conversao concluida com sucesso!
[GUI] Diagrama desenhado: 8 estados, 16 transicoes

[GUI] Aplicacao encerrada
=====================================
```

**Quando usar:**
- Verificar que a conversão está correta
- Debugar problemas no algoritmo
- Contar estados e transições
- Demonstrar o processo de conversão

---

### 🔍 GUI: Compilar e Executar ⭐

**Task padrão** - Compila e executa em sequência **com logs visíveis**.

**Como usar:**
```powershell
# Atalho do VS Code
Ctrl+Shift+B

# Ou via menu
Terminal > Run Build Task > "🎨 GUI: Compilar e Executar"
```

**Fluxo:**
1. Executa "🎨 GUI: Compilar"
2. Se a compilação for bem-sucedida
3. Executa automaticamente `bin/afn2afdgui.exe` **com terminal de logs**

**✨ NOVO:** Agora mostra logs detalhados no terminal!
- Ver AFN de entrada
- Ver AFD resultante
- Acompanhar estados compostos gerados
- Verificar transições criadas

Esta é a **task padrão do projeto** (configurada com `"isDefault": true`).

---

### 🧹 Limpar arquivos compilados

Remove todos os arquivos gerados pela compilação.

**Como usar:**
```powershell
# Via menu de tasks
Selecione: "🧹 Limpar arquivos compilados"
```

**O que remove:**
- `bin/*.exe` - Todos os executáveis
- `src/*.exe`, `src/*.o`, `src/*.ppu` - Arquivos temporários
- `src/lib/` - Pasta completa de arquivos intermediários

**Quando usar:**
- Antes de fazer uma compilação "limpa"
- Para resolver problemas de compilação
- Para reduzir o tamanho do repositório

**Saída esperada:**
```
Arquivos compilados removidos!
```

---

## 🔧 Configuração das Tasks

As tasks estão definidas em `.vscode/tasks.json`:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "🎨 GUI: Compilar",
      "type": "shell",
      "command": "C:\\lazarus\\lazbuild.exe",
      "args": [
        "--build-mode=Release",
        "${workspaceFolder}\\src\\afn2afdgui.lpi"
      ],
      "group": "build"
    },
    {
      "label": "🔍 GUI: Executar com Log",
      "type": "shell",
      "command": "${workspaceFolder}\\bin\\afn2afdgui.exe",
      "presentation": {
        "reveal": "always",
        "panel": "dedicated"
      },
      "group": "build"
    },
    {
      "label": "🎨 GUI: Compilar e Executar",
      "dependsOn": ["🎨 GUI: Compilar"],
      "type": "shell",
      "command": "${workspaceFolder}\\bin\\afn2afdgui.exe",
      "presentation": {
        "reveal": "always",
        "panel": "dedicated"
      },
      "group": {
        "kind": "build",
        "isDefault": true
      }
    }
    // ... outras tasks
  ]
}
```

## 📋 Console Logging

A aplicação GUI foi configurada com **{$APPTYPE CONSOLE}** para permitir logging detalhado.

### Como Funciona

**No código Pascal:**
```pascal
{$APPTYPE CONSOLE}  // Habilita console no Windows

begin
  WriteLn('=====================================');
  WriteLn('  AFN → AFD Converter (GUI Mode)');
  WriteLn('=====================================');
  Application.Run;
end.
```

**Na conversão (MainForm.pas):**
```pascal
procedure TFormMain.ConvertAFNtoAFD;
begin
  WriteLn('[GUI] AFN DE ENTRADA:');
  WriteLn('  Alfabeto: ', alfabeto);
  WriteLn('  Estados: ', estados);
  // ... conversão ...
  WriteLn('[GUI] Conversao concluida com sucesso!');
end;
```

### Tipos de Logs

| Tipo | Exemplo | Quando |
|------|---------|--------|
| **Inicialização** | `[GUI] Aplicacao iniciada` | Ao abrir GUI |
| **Carregamento** | `[GUI] 9 arquivos de teste carregados` | LoadTestFiles() |
| **Seleção** | `[GUI] Arquivo selecionado: test_ab.txt` | OnTestFileSelected() |
| **AFN Input** | `[GUI] AFN DE ENTRADA: ...` | Antes da conversão |
| **AFD Output** | `[GUI] AFD RESULTANTE: ...` | Depois da conversão |
| **Diagrama** | `[GUI] Diagrama desenhado: 8 estados` | DrawAutomaton() |
| **Encerramento** | `[GUI] Aplicacao encerrada` | Ao fechar GUI |

### Ver Logs em Tempo Real

**Opção 1: Task com logs (RECOMENDADO)**
```powershell
Ctrl+Shift+B  # Compila e executa com logs visíveis
```

**Opção 2: Task dedicada**
```powershell
Terminal > Run Task > "🔍 GUI: Executar com Log"
```

**Opção 3: Terminal manual**
```powershell
.\bin\afn2afdgui.exe  # Logs aparecem no mesmo terminal
```

---

## 📋 Requisitos

Para que as tasks funcionem corretamente:

1. ✅ **Lazarus instalado em `C:\lazarus`**
   - Se instalou em outro local, edite o caminho em `tasks.json`

2. ✅ **Estrutura de pastas correta**
   ```
   projeto/
   ├── src/afn2afdgui.lpi
   └── bin/ (será criada automaticamente)
   ```

3. ✅ **VS Code com permissões para executar PowerShell**

## 🎮 Workflow Recomendado

### Desenvolvimento Diário

1. **Abrir projeto:**
   ```powershell
   code .
   ```

2. **Compilar e testar:**
   - Pressione `Ctrl+Shift+B` (compila + executa **com logs**)
   - Verifique os logs no terminal integrado
   - Faça alterações no código
   - Pressione `Ctrl+Shift+B` novamente

3. **Debugging com logs detalhados:**
   - Use "🔍 GUI: Executar com Log" para terminal dedicado
   - Logs mostram AFN, AFD, estados e transições
   - Ideal para validar conversões

4. **Limpar quando necessário:**
   - `Ctrl+Shift+P` → "Run Task" → "🧹 Limpar"

### Primeira Compilação

1. **Instalar Lazarus:**
   ```powershell
   .\download_install_lazarus.ps1
   ```

2. **Compilar:**
   ```powershell
   Ctrl+Shift+B
   ```

3. **Resultado:**
   ```
   ✅ bin/afn2afdgui.exe criado com sucesso
   🎉 Aplicação executando
   ```

## 🐛 Troubleshooting

### ❌ Erro: "lazbuild.exe não encontrado"

**Causa:** Lazarus não está instalado em `C:\lazarus`

**Solução:**
1. Instale o Lazarus via script:
   ```powershell
   .\download_install_lazarus.ps1
   ```

2. Ou edite `.vscode\tasks.json` com o caminho correto:
   ```json
   "command": "SEU_CAMINHO\\lazbuild.exe"
   ```

### ❌ Erro: "Fatal: Can't find unit Interfaces"

**Causa:** Tentando compilar sem Lazarus instalado

**Solução:** Instale o Lazarus IDE completo (não apenas FPC)

### ❌ Erro: "Project file not found"

**Causa:** Executando task fora da pasta raiz do projeto

**Solução:** 
```powershell
cd C:\caminho\para\json2fsm
code .
```

### ⚠️ Task não aparece no menu

**Causa:** Arquivo `tasks.json` com erro de sintaxe

**Solução:** Valide o JSON:
```powershell
Get-Content .vscode\tasks.json | ConvertFrom-Json
```

## 💡 Dicas Avançadas

### Capturar Logs em Arquivo

**PowerShell:**
```powershell
.\bin\afn2afdgui.exe > logs.txt 2>&1
```

**Analisar logs:**
```powershell
Select-String -Path logs.txt -Pattern "\[GUI\]"
```

### Comparar Conversões

**Testar múltiplos arquivos:**
```powershell
# Executar com logs e salvar
.\bin\afn2afdgui.exe | Tee-Object -FilePath conversao_$(Get-Date -Format 'yyyyMMdd_HHmmss').log
```

### Adicionar Nova Task

Edite `.vscode/tasks.json`:

```json
{
  "label": "🆕 Minha Task",
  "type": "shell",
  "command": "seu-comando.exe",
  "args": ["--arg1", "--arg2"],
  "group": "build"
}
```

### Task com Dependências

```json
{
  "label": "Task Composta",
  "dependsOn": ["Task 1", "Task 2"],
  "dependsOrder": "sequence"
}
```

### Definir Task Padrão

```json
{
  "label": "Minha Task Padrão",
  "group": {
    "kind": "build",
    "isDefault": true
  }
}
```

## 📚 Referências

- [VS Code Tasks Documentation](https://code.visualstudio.com/docs/editor/tasks)
- [Lazarus Build Modes](https://wiki.lazarus.freepascal.org/Build_Modes)
- [PowerShell em Tasks](https://code.visualstudio.com/docs/editor/tasks#_custom-tasks)
