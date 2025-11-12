# json2fsm — Conversor AFN → AFD em Pascal com Visualização de Diagramas

Este repositório contém um conversor de Autômato Finito Não-determinístico (AFN) para Autômato Finito Determinístico (AFD) implementado em Pascal com interface gráfica usando **Lazarus LCL**.

**✨ Principais recursos:**
- 🎨 Interface gráfica intuitiva
- 📊 **Visualização gráfica de diagramas** AFN e AFD
- 📁 Carregamento de arquivos de teste
- 🔄 Conversão automática com algoritmo de construção de subconjuntos
- 📋 9 casos de teste incluídos

Configurado para compilar e executar usando o **Lazarus IDE**, com tasks pré-configuradas para o VS Code.

## 📚 Documentação Completa

🌐 **[Acesse a documentação interativa](https://peudias.github.io/json2fsm/)** com:
- 📦 Guia de instalação passo a passo
- 🎮 Tutorial completo da interface
- 🧪 Descrição detalhada dos 9 casos de teste
- 🔬 Explicação do algoritmo de conversão
- 💻 Referência da API do código
- ❓ FAQ e Troubleshooting

> 💡 Ou navegue localmente: abra `docs/index.html` no navegador

---

## ✨ Recursos da Interface GUI

### 📐 Layout da Interface:
A janela é dividida em duas áreas principais (redimensionáveis com splitter vertical):

**🔹 Painel Esquerdo (550px) - Entrada e Resultado:**
- 📂 **Carregar Arquivo**: Botão para abrir arquivos `.txt` com AFN
- ✏️ **Editor de Entrada**: Área de texto para editar AFN manualmente
- 🔄 **Converter**: Botão principal que executa a conversão AFN → AFD
- 📄 **Resultado Textual**: Exibe DFA formatado com emojis:
  - 📋 Alfabeto
  - 🔵 Estados do DFA  
  - ▶️ Estado inicial
  - 🎯 Estados finais
  - ➡️ Transições
- 🗑️ **Limpar**: Limpa entrada, resultado e diagramas
- **Auto-load**: Carrega `sample_afn.txt` automaticamente ao abrir

**🔹 Painel Direito (645px) - Visualização Gráfica:**
Duas abas com renderização nativa de diagramas:

- **📊 Aba "Diagrama AFN"**: 
  - Visualização gráfica do autômato não-determinístico de entrada
  - Estados desenhados como círculos (raio 20px)
  - Estados finais com círculo duplo e fundo amarelo
  - Seta de entrada indicando estado(s) inicial(is)
  - Transições como setas direcionadas com rótulos de símbolos
  - Self-loops (transições para si mesmo) desenhados como arcos superiores
  
- **📊 Aba "Diagrama AFD"**: 
  - Visualização gráfica do autômato determinístico resultante
  - Estados compostos com notação de conjunto (ex: `{q0,q1}`)
  - Mesmo estilo visual do AFN para fácil comparação
  - Atualização automática após cada conversão

### 🎨 Características Técnicas dos Diagramas:
- ✅ **Desenho 100% nativo** em Pascal usando TCanvas (sem dependências)
- ✅ Estados: círculos de 20px com fontes de 8pt em negrito
- ✅ Linhas: 1px de espessura para transições
- ✅ **Espaçamento fixo**: 100px horizontal, 80px vertical entre estados
- ✅ **Layout inteligente**: máximo 4 colunas para evitar diagramas muito largos
- ✅ **Posicionamento**: alinhado ao topo-esquerdo com margens de 60px (esquerda) e 40px (topo)
- ✅ **Margem esquerda aumentada**: garante que a seta de estado inicial sempre fique visível
- ✅ Atualização em tempo real ao converter ou limpar

### 🏆 Vantagens do Layout Dividido:
- 👁️ Visualização simultânea de entrada, resultado textual E diagramas
- 🔄 Compare AFN e AFD lado a lado alternando entre abas
- 📏 Splitter ajustável para priorizar texto ou gráficos conforme necessário
- 🖼️ Janela de 1200x600px otimizada para laptops e desktops

---

## 📋 Pré-requisitos (Windows)

### ✅ O que você precisa:

1. **Windows 10/11** com PowerShell
2. **Visual Studio Code** instalado
3. **Lazarus IDE** (inclui Free Pascal Compiler)
   - Download: https://www.lazarus-ide.org/
   - Versão recomendada: Lazarus 3.6 com FPC 3.2.2 (64-bit)

### 🎯 Após clonar este repositório:

1. Instale o Lazarus IDE (instruções abaixo)
2. Compile o projeto usando as tasks do VS Code

---

## 🚀 Como usar

### 🎨 **Interface Gráfica (afn2afdgui.exe)**

#### **Primeira vez - Instalar Lazarus:**

1. **Baixar Lazarus IDE:**
   - Acesse: https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%203.6/
   - Baixe: `lazarus-3.6-fpc-3.2.2-win64.exe` (~250 MB)

2. **Instalar:**
   - Execute o instalador como Administrador
   - Instale em `C:\lazarus` (caminho padrão recomendado)
   - Aguarde ~2-3 minutos

3. **Compilar projeto:**
   ```powershell
   # Via VS Code (recomendado)
   # Aperte Ctrl+Shift+B → escolha "🎨 GUI: Compilar e Executar"
   
   # Ou via linha de comando
   C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi
   ```

4. **Executar:**
   ```powershell
   .\bin\afn2afdgui.exe
   ```

#### **Recursos da Interface GUI:**
- 📂 **Carregar Arquivo**: Botão para abrir arquivos `.txt` com AFN
- ✏️ **Editor de Entrada**: Área para editar AFN manualmente
- 🔄 **Converter**: Botão principal que executa a conversão
- � **Resultado Formatado**: Exibe DFA com emojis e formatação:
  - 📋 Alfabeto
  - 🔵 Estados do DFA  
  - ▶️ Estado inicial
  - 🎯 Estados finais
  - ➡️ Transições
- 🗑️ **Limpar**: Limpa entrada e saída
- **Auto-load**: Carrega `sample_afn.txt` automaticamente ao abrir

#### **Script automatizado de compilação GUI:**
```powershell
# Execute este script após instalar o Lazarus
.\install_and_compile_gui.ps1
```

---

## 📋 Compilação Manual

### Lazarus (GUI):

```powershell
# Compilar via lazbuild
C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi

# Ou abrir no Lazarus IDE e pressionar F9
```

---

## 📁 Estrutura do Projeto

```
json2fsm/
├── src/
│   ├── afn2afdgui.lpr          # Projeto Lazarus (GUI) - arquivo principal
│   ├── afn2afdgui.lpi          # Configuração do projeto Lazarus
│   ├── MainForm.pas            # Unit do formulário principal (lógica GUI)
│   ├── MainForm.lfm            # Layout visual do formulário
│   └── lib/                    # Arquivos temporários de compilação
├── testes/                      # Casos de teste AFN→AFD
│   ├── test_ab.txt             # Strings terminando em "ab" (3→3 estados)
│   ├── test_binario.txt        # AFD já determinístico (3→3 estados)
│   ├── test_nao_det.txt        # Contém "aaa" - explosão de estados (4→8)
│   ├── test_par_a.txt          # Número par de 'a's (2→2 estados)
│   ├── test_tres_b.txt         # ≥3 'b's consecutivos (5 estados AFN)
│   ├── test_duplo_inicial.txt  # Múltiplos estados iniciais
│   ├── test_tres_simbolos.txt  # Alfabeto {a,b,c} (3 símbolos)
│   ├── test_explosao.txt       # Estados compostos complexos (6 AFN)
│   └── test_simples_nd.txt     # Não-determinismo básico
├── bin/                         # Executáveis compilados
│   └── afn2afdgui.exe          # Versão GUI (após compilar)
├── pascalwindows/               # Free Pascal Compiler (legado - não usado)
│   ├── bin/i386-win32/
│   │   └── fpc.exe
│   └── units/i386-win32/       # Units RTL
├── .vscode/
│   └── tasks.json              # Tasks do VS Code para compilação GUI
├── install_and_compile_gui.ps1 # Script auxiliar para instalar/compilar GUI
└── README.md
```

---

## 🎯 Guia Rápido

### ⚡ **Início Rápido (5 minutos):**

1. **Clone o repositório**
   ```bash
   git clone https://github.com/peudias/json2fsm.git
   cd json2fsm
   ```

2. **Abra no VS Code**
   ```bash
   code .
   ```

3. **Instale o Lazarus** (apenas na primeira vez)
   - Baixe em: https://www.lazarus-ide.org/
   - Execute o instalador
   - Instale em `C:\lazarus`

4. **Compile e execute a GUI**
   - Aperte **`Ctrl+Shift+B`**
   - Escolha: **🎨 GUI: Compilar e Executar**
   - Pronto! A interface gráfica abrirá automaticamente 🎉

### 📝 **Testando com o exemplo:**
A aplicação já vem com `sample_afn.txt` carregado automaticamente:
- Clique em **"🔄 Converter AFN → AFD"**
- Veja o resultado formatado aparecer!

---

## 🧪 Casos de Teste Incluídos

O diretório `testes/` contém 9 casos de teste cuidadosamente elaborados para demonstrar diferentes aspectos da conversão AFN→AFD:

### **Testes Básicos:**

1. **`test_ab.txt`** - Strings terminando em "ab"
   - AFN: 3 estados → DFA: 3 estados
   - Demonstra não-determinismo simples

2. **`test_binario.txt`** - Sequências binárias múltiplas de 3
   - AFN: 3 estados → DFA: 3 estados  
   - Exemplo de AFD já determinístico (sem expansão)

3. **`test_simples_nd.txt`** - Não-determinismo básico
   - Múltiplas transições com mesmo símbolo
   - Ideal para entender o algoritmo de construção de subconjuntos

### **Testes de Características Especiais:**

4. **`test_par_a.txt`** - Número par de 'a's
   - AFN: 2 estados → DFA: 2 estados
   - Autômato compacto e elegante

5. **`test_tres_b.txt`** - Pelo menos 3 'b's consecutivos
   - AFN: 5 estados
   - Contador de símbolos consecutivos

6. **`test_duplo_inicial.txt`** - Múltiplos estados iniciais
   - Demonstra como o DFA combina estados iniciais em um único conjunto
   - Recurso importante do algoritmo

7. **`test_tres_simbolos.txt`** - Alfabeto com 3 símbolos {a, b, c}
   - Teste com alfabeto maior
   - Mais transições por estado

### **Testes de Explosão de Estados:**

8. **`test_nao_det.txt`** - Contém "aaa"
   - AFN: 4 estados → **DFA: 8 estados** 💥
   - Demonstra explosão combinatória de estados compostos
   - Excelente para visualizar crescimento exponencial

9. **`test_explosao.txt`** - Estados compostos complexos
   - AFN: 6 estados → Múltiplos estados compostos
   - Testa limites do algoritmo

### **Como usar os testes:**

1. Clique em **"📂 Carregar Arquivo..."** na GUI
2. Navegue até a pasta `testes/`
3. Selecione um dos arquivos de teste
4. Clique em **"🔄 Converter AFN → AFD"**
5. Observe as **abas de diagramas** para visualização gráfica!

### **💡 Dica:** 
Use `test_nao_det.txt` e `test_explosao.txt` para ver a **explosão de estados compostos** nas abas de diagramas - é impressionante ver visualmente como o AFD cresce!

---

## ⚙️ Configurações Disponíveis

### Tasks do VS Code (`.vscode/tasks.json`)

Aperte **`Ctrl+Shift+B`** para ver o menu:

- **🎨 GUI: Compilar** — Compila a interface gráfica
- **🎨 GUI: Executar** — Executa a GUI sem compilar
- **🎨 GUI: Compilar e Executar** — Compila e abre a janela automaticamente ⭐
- **🧹 Limpar arquivos compilados** — Remove `.exe`, `.o`, `.ppu`, `lib/`

---

## 🐛 Troubleshooting

### ❌ "Lazarus não encontrado" ao compilar GUI?

**Solução:**
1. Verifique se instalou em `C:\lazarus`
2. Se instalou em outro local, edite `.vscode\tasks.json`:
   ```json
   "command": "SEU_CAMINHO\\lazbuild.exe"
   ```

### ❌ Erro "Can't find unit Interfaces"?

**Causa:** Tentando compilar sem Lazarus instalado.

**Solução:** Instale o Lazarus IDE.

### ❌ As tasks não aparecem no menu?

**Solução:**
1. Feche e reabra o VS Code
2. Ou: `File` → `Close Folder` → Reabra a pasta
3. Aperte `Ctrl+Shift+B` novamente

### ❌ Erro de "Execution Policy" no PowerShell?

**Solução:**
```powershell
Set-ExecutionPolicy -Scope CurrentUser -ExecutionPolicy RemoteSigned
```

### ❌ Erro "SetName" ao compilar GUI?

**Causa:** Conflito com função da LCL (já corrigido na versão atual).

**Solução:** Puxe a versão mais recente do repositório (`git pull`).

### ❌ GUI compila mas não abre?

**Solução:**
```powershell
# Verificar se o executável existe
Test-Path .\bin\afn2afdgui.exe

# Executar manualmente para ver erros
.\bin\afn2afdgui.exe
```

---

---

## 🔬 Como Funciona o Algoritmo de Conversão AFN → AFD

O programa usa o algoritmo clássico de **Construção de Subconjuntos** (Subset Construction) para converter AFN em AFD.

### **Conceito Principal:**
Cada estado do AFD é um **conjunto de estados do AFN**. Por exemplo:
- Estados do AFN: `q0, q1, q2`
- Estados do AFD: `{q0}`, `{q0,q1}`, `{q0,q2}`, etc.

### **Algoritmo Passo a Passo:**

#### **1️⃣ Inicialização**
- Estado inicial do AFD = conjunto de estados iniciais do AFN
- Adiciona à fila de processamento

#### **2️⃣ Loop Principal (BFS - Busca em Largura)**
Para cada conjunto na fila:
1. **Marca como final** se contém algum estado final do AFN
2. **Para cada símbolo do alfabeto:**
   - Calcula conjunto destino (união de todos os estados alcançáveis)
   - Cria transição no AFD
   - Se é novo conjunto, adiciona à fila

#### **3️⃣ Cálculo de Transições**
```
Move({q0,q1}, 'a') = {todos os estados alcançáveis de q0 ou q1 com 'a'}
```

### **Exemplo Prático:**

**AFN de Entrada:**
```
Alfabeto: a, b
Estados: q0, q1, q2
Inicial: q0
Final: q2
Transições:
  q0 --a--> q0
  q0 --b--> q0
  q0 --a--> q1  (não-determinístico!)
  q1 --b--> q2
```

**AFD Resultante:**
```
Estados: {q0}, {q0,q1}, {q0,q2}
Inicial: {q0}
Finais: {q0,q2}  (contém q2 que é final)
Transições:
  {q0} --a--> {q0,q1}
  {q0} --b--> {q0}
  {q0,q1} --a--> {q0,q1}
  {q0,q1} --b--> {q0,q2}
  {q0,q2} --a--> {q0,q1}
  {q0,q2} --b--> {q0}
```

### **Características da Implementação:**

✅ **Suporta:**
- Múltiplos estados iniciais
- Múltiplos estados finais
- Não-determinismo (múltiplas transições com mesmo símbolo)

❌ **NÃO suporta atualmente:**
- Transições epsilon (ε-transições)
- Minimização do AFD resultante

### **Estruturas de Dados:**
- **`TStringSet`**: Classe para representar conjuntos de estados
- **`dfaMap`**: Mapeia nome do conjunto → objeto TStringSet
- **`workQ`**: Fila de estados a processar (BFS)
- **`dfaTransitions`**: Array dinâmico com transições do AFD

---

## 📚 Recursos Adicionais

- [Documentação do Free Pascal](https://www.freepascal.org/docs.html)
- [Lazarus IDE Documentation](https://wiki.lazarus.freepascal.org/)
- [VS Code Tasks Documentation](https://code.visualstudio.com/docs/editor/tasks)
- [Teoria dos Autômatos - Wikipedia](https://pt.wikipedia.org/wiki/Aut%C3%B4mato_finito)

---

## 📝 Notas Técnicas

### Versões:
- **Lazarus IDE**: 3.6 (inclui Free Pascal Compiler 3.2.2)
- **Target OS**: Windows 64-bit

### Arquivos importantes:
- **`src/afn2afdgui.lpr`**: Arquivo principal do projeto Lazarus
- **`src/MainForm.pas`**: Implementação GUI + lógica de conversão + renderização de diagramas
- **`src/MainForm.lfm`**: Layout visual (split vertical, PageControl com abas)
- **`testes/`**: Diretório com 9 casos de teste diversos
- **`install_and_compile_gui.ps1`**: Script helper para primeira compilação GUI

### Renderização Gráfica dos Diagramas:
A GUI utiliza **desenho nativo em Pascal** com componentes da Lazarus LCL:

**Tecnologia:**
- **`TPaintBox`**: Componente de desenho (um para AFN, outro para AFD)
- **`TCanvas`**: API de desenho 2D (Ellipse, LineTo, MoveTo, TextOut)
- **Eventos `OnPaint`**: Redesenham diagramas quando necessário (resize, conversão)
- **`DrawAutomaton`**: Procedimento customizado que renderiza estados e transições

**Parâmetros de Layout:**
- **Raio dos estados**: 20px
- **Espaçamento horizontal**: 100px entre estados
- **Espaçamento vertical**: 80px entre linhas
- **Margens**: 60px (esquerda), 40px (topo)
- **Grid**: Máximo de 4 estados por linha
- **Alinhamento**: Top-left (evita centralização que esconde setas iniciais)

**Elementos visuais:**
- **Estados**: Círculos com nome centralizado
- **Estados finais**: Círculo duplo (amarelo)
- **Estado inicial**: Seta de 20px apontando da esquerda
- **Transições**: Setas curvas com símbolo do meio
- **Self-loops**: Arcos acima do estado
- **Cores**: Preto (estados), Vermelho (transições), Amarelo (finais)

**Vantagens sobre export externo:**
- ✅ Visualização instantânea (sem arquivos intermediários)
- ✅ Interação direta (zoom, resize da janela)
- ✅ Comparação lado-a-lado com abas AFN/AFD
- ✅ Totalmente auto-contido (sem dependências externas)

### Limitações conhecidas:
- ❌ Não suporta transições epsilon (ε-transições)
- ❌ Não realiza minimização do AFD resultante
- ❌ Diagrama não suporta zoom/pan (tamanho fixo baseado em quantidade de estados)
- ✅ Suporta múltiplos estados iniciais e finais
- ✅ Suporta não-determinismo completo
- ✅ Visualização gráfica nativa integrada

### Possíveis extensões futuras:
- [ ] Suporte a epsilon-transições
- [ ] Minimização de AFD (algoritmo de Hopcroft)
- [ ] Export para DOT/Graphviz
- [ ] Zoom/pan nos diagramas (ScrollBox + transformações)
- [ ] Simulação de entrada em AFN/AFD
- [ ] Layout automático de grafos (force-directed)

---

## 🎉 Pronto para usar!

### 🚀 **Para começar agora:**

1. **Aperte `Ctrl+Shift+B`**
2. **Escolha: "🎨 GUI: Compilar e Executar"**
3. **Clique em "🔄 Converter AFN → AFD"** na janela que abrir
4. **Veja os diagramas nas abas à direita!** ✨
5. **Experimente os arquivos de teste em `testes/`!**

### 📋 **Outros comandos úteis:**

```powershell
# Compilar e executar GUI (recomendado)
Ctrl+Shift+B → 🎨 GUI: Compilar e Executar

# Apenas executar GUI (sem compilar)
.\bin\afn2afdgui.exe

# Carregar arquivo de teste específico
# (Ou use o botão "📂 Carregar Arquivo..." na GUI)

# Limpar arquivos compilados
Ctrl+Shift+B → 🧹 Limpar arquivos compilados
```

**💡 Dica:** Na GUI, use a aba **"Diagrama AFD"** depois de converter para ver visualmente como o algoritmo combinou os estados do AFN!

---

## 👨‍💻 Autor

**Henrique** - [peudias](https://github.com/peudias)

---

## 📄 Licença

Este projeto é de código aberto e está disponível sob licença livre para fins educacionais.

---