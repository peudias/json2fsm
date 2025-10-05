# json2fsm — Conversor AFN → AFD em Pascal

Este repositório contém um conversor de Autômato Finito Não-determinístico (AFN) para Autômato Finito Determinístico (AFD) implementado em Pascal, com duas versões:

- **Console (`afn2afd.exe`)**: Versão linha de comando
- **GUI (`afn2afdgui.exe`)**: Interface gráfica usando Lazarus LCL

Configurado para compilar e executar usando o **Free Pascal Compiler (FPC)** instalado localmente, com tasks pré-configuradas para o VS Code.

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

## 🚀 Como usar

### 🖥️ **Versão Console (afn2afd.exe)**

#### Via Linha de Comando:

```powershell
# Usar arquivo de exemplo (padrão)
.\bin\afn2afd.exe

# Usar arquivo específico
.\bin\afn2afd.exe caminho\para\arquivo.txt

# Entrada interativa (se arquivo não encontrado)
.\bin\afn2afd.exe
```

#### Formato do arquivo de entrada:

```
a b                  # Linha 1: Alfabeto (símbolos separados por espaço)
q0 q1 q2            # Linha 2: Estados (nomes separados por espaço)
q0                  # Linha 3: Estados iniciais
q2                  # Linha 4: Estados finais
q0 a q0             # Linhas seguintes: Transições (estado símbolo estado)
q0 b q0
q0 a q1
q1 b q2
```

### 🎨 **Versão GUI (afn2afdgui.exe)**

1. **Abrir o projeto no Lazarus:**
   ```powershell
   # Se tiver Lazarus instalado
   lazarus src\afn2afdgui.lpi
   ```

2. **Ou compilar via linha de comando:**
   ```powershell
   # Compilar versão Release
   lazbuild --build-mode=Release src\afn2afdgui.lpi
   
   # Executar
   .\bin\afn2afdgui.exe
   ```

3. **Interface:**
   - 📂 Carregar arquivo AFN via botão "Carregar Arquivo"
   - ✏️ Editar entrada manualmente na caixa de texto superior
   - 🔄 Clicar em "Converter AFN → AFD"
   - ✅ Ver resultado formatado na caixa inferior

---

## 📋 Compilação Manual

### Console (Free Pascal puro):

```powershell
# Compilar afn2afd (console)
& .\pascalwindows\bin\i386-win32\fpc.exe `
  -Fu".\pascalwindows\units\i386-win32" `
  -Fu".\pascalwindows\units\i386-win32\rtl" `
  -Fu".\pascalwindows\units\i386-win32\rtl-console" `
  .\src\afn2afd.pas

# Copiar para bin/
Copy-Item .\src\afn2afd.exe .\bin\afn2afd.exe -Force
```

### GUI (requer Lazarus):

```powershell
# Compilar via lazbuild
lazbuild --build-mode=Release src\afn2afdgui.lpi

# Ou abrir no Lazarus IDE e pressionar F9
```

---

## 📁 Estrutura do Projeto

```
json2fsm/
├── src/
│   ├── hello.pas              # Exemplo "Hello World"
│   ├── afn2afd.pas           # Conversor AFN→AFD (console)
│   ├── afn2afdgui.lpr        # Projeto Lazarus (GUI)
│   ├── MainForm.pas          # Unit do formulário principal
│   ├── MainForm.lfm          # Layout do formulário
│   └── sample_afn.txt        # Arquivo de exemplo AFN
├── bin/                       # Executáveis compilados
│   ├── afn2afd.exe           # Versão console
│   └── afn2afdgui.exe        # Versão GUI (após compilar)
├── pascalwindows/             # Free Pascal Compiler (INCLUÍDO)
│   └── bin/i386-win32/
│       └── fpc.exe
├── .vscode/
│   ├── tasks.json            # Tasks do VS Code
│   └── build_pascal.ps1      # Script de build
└── README.md
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

---

## Novo: AFN → AFD (ferramenta simples)

Adicionei um programa de exemplo `src/afn2afd.pas` que implementa uma construção por subconjuntos (sem suporte a epsilons por enquanto).

Como usar o exemplo interativo:

1. Compile o programa (usando as mesmas tasks):

```powershell
# compila o afn2afd
& .\pascalwindows\bin\i386-win32\fpc.exe .\src\afn2afd.pas
```

2. Rode o executável `src\afn2afd.exe` (ou mova para `bin` se preferir) e siga as instruções que pedem:
   - linha do alfabeto (símbolos separados por espaço),
   - linha de estados,
   - linha de estados iniciais,
   - linha de estados finais,
   - transições no formato: <from> <symbol> <to>, uma por linha, terminar com linha vazia.

3. Também incluí um `src/sample_afn.txt` com um pequeno exemplo (alfabeto: a b, estados: q0 q1 q2, iniciais: q0, finais: q2).

Observação: o código é um esqueleto didático — podemos estender para suportar epsilon-transições, minimização do AFD e leitura/parsers de arquivos.
