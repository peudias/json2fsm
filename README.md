# json2fsm — Pascal com Free Pascal Local (Windows)

Este repositório contém um exemplo de projeto Pascal configurado para compilar e executar usando o **Free Pascal Compiler (FPC)** instalado localmente, com tasks pré-configuradas para o VS Code.

---

## 📋 Pré-requisitos (Windows)

### ✅ O que você precisa:

1. **Windows 10/11** com PowerShell
2. **Visual Studio Code** instalado
3. **Free Pascal Compiler (FPC)** — JÁ INCLUÍDO neste repositório em `pascalwindows/`

### 🎯 Após clonar este repositório:

**Nada!** O compilador Pascal já está incluído na pasta `pascalwindows/bin/i386-win32/fpc.exe`.

Basta abrir o projeto no VS Code e usar as tasks configuradas!

---

## 🚀 Como usar (3 formas)

### 1️⃣ **Menu Visual de Tasks** (Recomendado! 🎯)

Aperte **`Ctrl+Shift+B`** e escolha no menu:

```
┌─────────────────────────────────────────────────┐
│ Select the build task to run                  │
├─────────────────────────────────────────────────┤
│ > 🔨 Pascal: Compilar                          │
│   ▶️ Pascal: Executar                          │
│   🚀 Pascal: Compilar e Executar               │
│   🧹 Pascal: Limpar arquivos compilados        │
└─────────────────────────────────────────────────┘
```

**O que cada opção faz:**

| Opção | Descrição |
|-------|-----------|
| 🔨 **Pascal: Compilar** | Compila `src/hello.pas` → `bin/hello.exe` |
| ▶️ **Pascal: Executar** | Executa `bin/hello.exe` (sem compilar) |
| 🚀 **Pascal: Compilar e Executar** | Compila e executa automaticamente |
| 🧹 **Pascal: Limpar** | Remove arquivos `.exe`, `.o`, `.ppu` gerados |

### 2️⃣ **Atalhos de Teclado** (Mais rápido! ⚡)

| Atalho | Ação |
|--------|------|
| **`Ctrl+Shift+B`** | Abre menu de tasks |
| **`F5`** | Compila e Executa direto |
| **`Ctrl+Alt+R`** | Compila e Executa (alternativo) |

### 3️⃣ **Via PowerShell** (Manual)

```powershell
# Compilar
& .\pascalwindows\bin\i386-win32\fpc.exe .\src\hello.pas

# Executar
.\src\hello.exe

# Ou usar o script de build (cria bin/ e move o executável)
powershell -NoProfile -ExecutionPolicy Bypass -File .\.vscode\build_pascal.ps1

# Depois executar
.\bin\hello.exe
```

---

## 📁 Estrutura do Projeto

```
json2fsm/
├── src/
│   └── hello.pas              # Código fonte Pascal
├── bin/                       # Executáveis compilados (gerado automaticamente)
│   └── hello.exe
├── pascalwindows/             # Free Pascal Compiler (INCLUÍDO)
│   └── bin/
│       └── i386-win32/
│           └── fpc.exe        # Compilador
├── .vscode/
│   ├── tasks.json             # Tasks pré-configuradas
│   ├── launch.json            # Configuração de debug/run
│   ├── build_pascal.ps1       # Script de build
│   └── settings.json          # Configurações do workspace
└── README.md                  # Este arquivo
```

---

## 🎯 Guia Rápido

### Para quem está começando:

1. **Clone o repositório**
   ```bash
   git clone https://github.com/peudias/json2fsm.git
   cd json2fsm
   ```

2. **Abra no VS Code**
   ```bash
   code .
   ```

3. **Compile e execute**
   - Aperte **`Ctrl+Shift+B`**
   - Escolha: **🚀 Pascal: Compilar e Executar**
   - Pronto! Você verá "Hello, World!" no terminal

---

## ⚙️ Configurações Disponíveis

### Tasks do VS Code (`.vscode/tasks.json`)

As seguintes tasks estão pré-configuradas:

- **🔨 Pascal: Compilar** — Compila o código fonte
- **▶️ Pascal: Executar** — Executa o programa compilado
- **🚀 Pascal: Compilar e Executar** — Faz os dois automaticamente
- **🧹 Pascal: Limpar** — Remove arquivos temporários

### Launch Configurations (`.vscode/launch.json`)

- **▶️ Pascal: Compilar e Executar** — Para usar com F5
- **▶️ Pascal: Apenas Executar** — Executa sem compilar

---

## 🐛 Troubleshooting

### As tasks não aparecem no menu?

1. Feche e reabra o VS Code
2. Ou: `File` → `Close Folder` → Reabra a pasta
3. Aperte `Ctrl+Shift+B` novamente

### Erro de "Execution Policy" no PowerShell?

As tasks já estão configuradas com `-ExecutionPolicy Bypass`. Se ainda assim der erro, execute:

```powershell
Set-ExecutionPolicy -Scope CurrentUser -ExecutionPolicy RemoteSigned
```

### O compilador não foi encontrado?

Verifique se a pasta `pascalwindows/bin/i386-win32/fpc.exe` existe. Se não, você pode:

1. Baixar o Free Pascal em: https://www.freepascal.org/download.html
2. Ou ajustar o caminho em `.vscode/build_pascal.ps1` e `.vscode/tasks.json`

---

## 📚 Recursos Adicionais

- [Documentação do Free Pascal](https://www.freepascal.org/docs.html)
- [VS Code Tasks Documentation](https://code.visualstudio.com/docs/editor/tasks)

---

## 📝 Notas

- O Free Pascal Compiler (versão 3.2.2) está **incluído** neste repositório na pasta `pascalwindows/`
- Funciona apenas no **Windows** (arquivos `.exe`)
- O script `build_pascal.ps1` compila o código e move o executável para `bin/`
- Todos os arquivos temporários (`.o`, `.ppu`) são criados em `src/` e podem ser removidos com a task **🧹 Limpar**

---

## 🎉 Pronto para usar!

Aperte **`Ctrl+Shift+B`** → escolha **🚀 Compilar e Executar** → veja a mágica acontecer! ✨
