# Documentação Completa do Código - json2fsm

**Autor:** Henrique Freitas  
**Data:** Dezembro 2025  
**Arquivo Principal:** `src/MainForm.pas`  
**Linhas de Código:** ~2500 linhas

---

## 📋 Índice

1. [Visão Geral da Arquitetura](#visão-geral)
2. [Estruturas de Dados](#estruturas-de-dados)
3. [Algoritmos Implementados](#algoritmos)
4. [Classes e Métodos Principais](#classes-métodos)
5. [Gerenciamento de Memória](#memória)
6. [Fluxo de Execução](#fluxo)
7. [Complexidade dos Algoritmos](#complexidade)

---

## 🏗️ Visão Geral da Arquitetura {#visão-geral}

### Paradigma
- **Linguagem:** Object Pascal (Free Pascal Compiler 3.2.2)
- **Framework GUI:** Lazarus LCL (Lazarus Component Library)
- **Padrão:** Event-Driven (baseado em eventos)

### Componentes Principais
```
MainForm.pas
├── TFormMain (Classe principal do formulário)
│   ├── Campos privados (dados dos autômatos)
│   ├── Métodos de conversão (algoritmos)
│   ├── Métodos de interface (UI)
│   └── Event handlers (botões, eventos)
├── TStringSet (Classe auxiliar para conjuntos)
│   ├── Operações de conjunto
│   └── Gerenciamento de estados
└── Tipos auxiliares
    ├── TTransition (Record de transição)
    └── TTransitionArray (Array dinâmico)
```

---

## 📊 Estruturas de Dados {#estruturas-de-dados}

### 1. TTransition (Record)
```pascal
type
  TTransition = record
    FromState: string;   // Estado de origem
    Symbol: string;      // Símbolo da transição
    ToState: string;     // Estado de destino
  end;
```

**Uso:** Representa uma única transição em um autômato.  
**Exemplo:** `q0 --a--> q1` = `{FromState:'q0', Symbol:'a', ToState:'q1'}`

### 2. TTransitionArray (Array Dinâmico)
```pascal
type
  TTransitionArray = array of TTransition;
```

**Uso:** Armazena todas as transições de um autômato.  
**Tamanho:** Ajustado dinamicamente com `SetLength()`.

### 3. TStringSet (Classe de Conjuntos)
```pascal
TStringSet = class
private
  FList: TStringList;  // Lista ordenada sem duplicatas
public
  // Operações principais
  procedure Add(const s: string);
  function Contains(const s: string): Boolean;
  function ToString: string;  // Formato: "{q0,q1,q2}"
  function Clone: TStringSet;
  // ... outros métodos
end;
```

**Características:**
- **Ordenação:** Elementos mantidos em ordem alfabética
- **Unicidade:** Duplicatas são automaticamente ignoradas
- **Uso:** Representar estados compostos do AFD (ex: `{q0,q1}`)

### 4. Campos da Classe TFormMain

Cada autômato é representado por **5 componentes**:

```pascal
// AFN-ε (Autômato com epsilon-transições)
EpsilonNFAAlphabet: TStringList    // Símbolos (inclui ε)
EpsilonNFAStates: TStringList      // Todos os estados
EpsilonNFAInitials: TStringList    // Estados iniciais (pode ter múltiplos)
EpsilonNFAFinals: TStringList      // Estados finais
EpsilonNFATransitions: TTransitionArray  // Todas as transições

// AFN (Autômato sem epsilon)
NFAAlphabet: TStringList
NFAStates: TStringList
NFAInitials: TStringList
NFAFinals: TStringList
NFATransitions: TTransitionArray

// AFD (Autômato Determinístico)
DFAStates: TStringList
DFAInitial: string                 // AFD tem APENAS UM inicial!
DFAFinals: TStringList
DFATransitions: TTransitionArray

// MinDFA (AFD Minimizado)
MinDFAStates: TStringList
MinDFAInitial: string
MinDFAFinals: TStringList
MinDFATransitions: TTransitionArray
```

---

## 🧮 Algoritmos Implementados {#algoritmos}

### 1. Epsilon-Closure (ε-fecho)

**Função:** `ComputeEpsilonClosure(states, transitions): TStringSet`

**O QUE FAZ:**
Calcula todos os estados alcançáveis a partir de um conjunto de estados usando **apenas** epsilon-transições.

**ALGORITMO: DFS com Pilha**
```
1. Inicializar resultado com estados fornecidos
2. Empilhar todos os estados iniciais
3. Enquanto pilha não está vazia:
   a) Desempilhar estado atual
   b) Para cada epsilon-transição do estado atual:
      - Se destino não foi visitado:
        * Adicionar ao resultado
        * Empilhar para processar
```

**EXEMPLO:**
```
AFN-ε:
  q0 --ε--> q1 --ε--> q2
  
ε-closure({q0}) = {q0, q1, q2}
```

**COMPLEXIDADE:** `O(n + m_ε)` onde:
- `n` = número de estados
- `m_ε` = número de epsilon-transições

**SÍMBOLOS EPSILON RECONHECIDOS:**
- `ε` (Unicode U+03B5) - recomendado
- `epsilon` (palavra completa)
- `e` (abreviação)
- `&` (notação alternativa)

---

### 2. Remoção de Epsilon-Transições

**Método:** `RemoveEpsilonTransitions()`

**O QUE FAZ:**
Converte AFN-ε → AFN (remove todas as epsilon-transições mantendo a mesma linguagem).

**ALGORITMO COMPLETO:**

**Fase 1: Parse da Entrada**
```
1. Ler alfabeto, estados, iniciais, finais, transições
2. Detectar símbolos epsilon no alfabeto
3. Validar formato da entrada
```

**Fase 2: Novos Estados Iniciais**
```
NFAInitials = ε-closure(EpsilonNFAInitials)
```
**Exemplo:**
```
AFN-ε: Inicial = {q0}, q0 --ε--> q1
AFN: Iniciais = {q0, q1}
```

**Fase 3: Novos Estados Finais**
```
Para cada estado q:
  Se ε-closure(q) ∩ EpsilonNFAFinals ≠ ∅:
    q é final no AFN
```
**Exemplo:**
```
AFN-ε: Final = {q2}, q1 --ε--> q2
AFN: Finais = {q1, q2}  (q1 alcança q2 por ε)
```

**Fase 4: Novas Transições (MAIS COMPLEXA!)**
```
Para cada estado p:
  Para cada símbolo a (exceto epsilon):
    targetSet = {} (conjunto vazio)
    
    Para cada estado q em ε-closure(p):
      Para cada transição q --a--> r:
        Adicionar ε-closure(r) ao targetSet
    
    Para cada estado t em targetSet:
      Adicionar transição p --a--> t ao AFN
```

**EXEMPLO DETALHADO:**
```
AFN-ε:
  q0 --a--> q1
  q1 --ε--> q2

Processando (q0, a):
  ε-closure(q0) = {q0}
  De q0 com 'a': vai para q1
  ε-closure(q1) = {q1, q2}
  
AFN resultante:
  q0 --a--> q1  (transição original)
  q0 --a--> q2  (NOVA! expandida via epsilon)
```

**COMPLEXIDADE:** `O(n² · m)` onde:
- `n` = número de estados
- `m` = número de transições

**LOOPS ANINHADOS:**
```
n estados × |Σ| símbolos × n estados closure × m transições
= O(n² · |Σ| · m)
```

---

### 3. Construção de Subconjuntos (AFN → AFD)

**Método:** `ConvertAFNtoAFD()`

**O QUE FAZ:**
Converte AFN → AFD usando algoritmo de **Subset Construction**.

**ALGORITMO: BFS (Busca em Largura)**

**Estruturas de Dados:**
```pascal
workQ: TStringList        // Fila de estados a processar
dfaMap: TStringList       // Mapeia nome → TStringSet
localDFAStates: TStringList  // Estados do AFD criados
```

**ALGORITMO PASSO-A-PASSO:**

```
1. INICIALIZAÇÃO:
   initialSet = {todos os estados iniciais do AFN}
   workQ.Add(initialSet)
   dfaMap[initialSet.ToString] := initialSet
   
2. ENQUANTO workQ NÃO ESTÁ VAZIA:
   curSet := workQ.Pop()  // Remove primeiro da fila
   
   Para cada símbolo s no alfabeto:
     nextSet := {} (vazio)
     
     // Calcular Move(curSet, s)
     Para cada estado q em curSet:
       Para cada transição q --s--> r:
         Adicionar r ao nextSet
     
     Se nextSet não é vazio:
       key := nextSet.ToString  // Ex: "{q0,q1}"
       
       Se key é novo (não está em dfaMap):
         Adicionar nextSet ao workQ
         dfaMap[key] := nextSet
       
       Adicionar transição: curSet --s--> nextSet ao AFD
   
   // Verificar se é final
   Se curSet contém algum estado final do AFN:
     Marcar curSet como final no AFD

3. ESTADOS DO AFD = Todos os conjuntos em dfaMap
```

**EXEMPLO EXECUÇÃO:**

```
AFN:
  q0 --a--> q1
  q0 --a--> q2
  q1 --b--> q3
  q2 --b--> q3
  Iniciais: {q0}
  Finais: {q3}

EXECUÇÃO:
  workQ = [{q0}]
  
  Passo 1: Processar {q0}
    - Com 'a': Move({q0}, a) = {q1, q2}
      * Novo! Adicionar à fila
      * Transição: {q0} --a--> {q1,q2}
    - Com 'b': Move({q0}, b) = {} (sem transição)
    - {q0} não é final (não contém q3)
  workQ = [{q1,q2}]
  
  Passo 2: Processar {q1,q2}
    - Com 'a': Move({q1,q2}, a) = {}
    - Com 'b': Move({q1,q2}, b) = {q3}
      * Novo! Adicionar à fila
      * Transição: {q1,q2} --b--> {q3}
    - {q1,q2} não é final
  workQ = [{q3}]
  
  Passo 3: Processar {q3}
    - Com 'a': Move({q3}, a) = {}
    - Com 'b': Move({q3}, b) = {}
    - {q3} É FINAL (contém q3)
  workQ = [] (vazio)

AFD RESULTANTE:
  Estados: {q0}, {q1,q2}, {q3}
  Inicial: {q0}
  Finais: {q3}
  Transições:
    {q0} --a--> {q1,q2}
    {q1,q2} --b--> {q3}
```

**EXPLOSÃO DE ESTADOS:**
- **Pior caso:** AFD com `2^n` estados (n = estados do AFN)
- **Exemplo:** AFN com 10 estados → AFD com até 1024 estados!
- **Na prática:** Geralmente muito menos

**COMPLEXIDADE:** `O(2^n · |Σ| · n)` onde:
- `2^n` = número máximo de estados do AFD
- `|Σ|` = tamanho do alfabeto
- `n` = cálculo de Move para cada símbolo

---

### 4. Minimização de AFD (Myhill-Nerode)

**Método:** `MinimizeDFA()`

**O QUE FAZ:**
Reduz o número de estados do AFD eliminando **estados equivalentes**.

**DEFINIÇÃO - ESTADOS EQUIVALENTES:**
Dois estados `p` e `q` são equivalentes se:
- Para TODA string `w`, δ(p, w) e δ(q, w) levam a estados com mesmo tipo (ambos finais ou ambos não-finais)

**ALGORITMO: Partição-Refinamento**

```
1. PARTIÇÃO INICIAL:
   P = {Finais, Não-Finais}
   
2. REPETIR ATÉ NÃO HAVER MUDANÇAS:
   Para cada partição B em P:
     Para cada símbolo a:
       // Verificar se estados de B são distinguíveis por 'a'
       Se existem p, q em B tal que:
         δ(p, a) e δ(q, a) estão em partições diferentes:
           Dividir B em sub-partições
           
3. CADA PARTIÇÃO FINAL = UM ESTADO DO MinDFA
```

**EXEMPLO DETALHADO:**

```
AFD:
  Estados: q0, q1, q2, q3, q4
  Inicial: q0
  Finais: {q3, q4}
  Transições:
    q0 --a--> q1, q0 --b--> q2
    q1 --a--> q3, q1 --b--> q4
    q2 --a--> q3, q2 --b--> q4
    q3 --a--> q3, q3 --b--> q3
    q4 --a--> q4, q4 --b--> q4

EXECUÇÃO:
  
  Iteração 0 (Inicial):
    P = [{q0, q1, q2}, {q3, q4}]
         (não-finais)   (finais)
  
  Iteração 1:
    Processar {q0, q1, q2}:
      Com 'a':
        δ(q0, a) = q1 (não-final)
        δ(q1, a) = q3 (final) ← DIFERENTE!
        δ(q2, a) = q3 (final)
      
      Dividir: {q0} e {q1, q2}
    
    P = [{q0}, {q1, q2}, {q3, q4}]
  
  Iteração 2:
    Processar {q3, q4}:
      Com 'a':
        δ(q3, a) = q3 (mesmo)
        δ(q4, a) = q4 (mesmo)
      Com 'b':
        δ(q3, b) = q3 (mesmo)
        δ(q4, b) = q4 (mesmo)
      
      NÃO dividir (são equivalentes!)
    
    Processar {q1, q2}:
      Com 'a' e 'b': ambos vão para mesmas partições
      NÃO dividir
    
    SEM MUDANÇAS → CONVERGIU!
  
  P_final = [{q0}, {q1, q2}, {q3, q4}]

MinDFA:
  Estados: [q0], [q1,q2], [q3,q4]
  Inicial: [q0]
  Finais: [q3,q4]
  Transições:
    [q0] --a--> [q1,q2]
    [q0] --b--> [q1,q2]
    [q1,q2] --a--> [q3,q4]
    [q1,q2] --b--> [q3,q4]
    [q3,q4] --a--> [q3,q4]
    [q3,q4] --b--> [q3,q4]
  
  REDUÇÃO: 5 estados → 3 estados!
```

**COMPLEXIDADE:** `O(n · m · log n)` onde:
- `n` = número de estados
- `m` = número de transições
- `log n` = número máximo de iterações (refinamentos)

---

## 🔧 Classes e Métodos Principais {#classes-métodos}

### TStringSet - Classe Auxiliar

```pascal
constructor Create;
// Cria lista ordenada sem duplicatas
// Sorted = True, Duplicates = dupIgnore

procedure Add(const s: string);
// Adiciona elemento (ignora duplicatas automaticamente)

function Contains(const s: string): Boolean;
// Verifica se elemento está no conjunto
// Complexidade: O(log n) - busca binária

function ToString: string;
// Retorna "{elem1,elem2,elem3}"
// Usado para nomear estados compostos do AFD

function Clone: TStringSet;
// Cria cópia independente
// IMPORTANTE: Caller deve dar Free()!

function IsEmpty: Boolean;
// Verifica se conjunto está vazio

function Count: Integer;
// Retorna número de elementos

function Item(i: Integer): string;
// Acessa elemento por índice (0-based)
```

### TFormMain - Métodos Principais

#### Inicialização

```pascal
procedure FormCreate(Sender: TObject);
// Chamado ao criar formulário
// - Inicializa todas as TStringList
// - Cria ComboBox de testes
// - Carrega arquivo exemplo
```

#### Conversões

```pascal
procedure RemoveEpsilonTransitions;
// AFN-ε → AFN
// Remove todas as epsilon-transições

procedure ConvertAFNtoAFD;
// AFN → AFD
// Construção de subconjuntos

procedure MinimizeDFA;
// AFD → MinDFA
// Algoritmo de Myhill-Nerode
```

#### Interface

```pascal
procedure DrawAutomaton(ACanvas, States, Initials, Finals, Transitions);
// Desenha diagrama de autômato em TPaintBox
// - Layout: 4 estados por linha
// - Espaçamento: 100px × 80px
// - Estados finais: círculo duplo amarelo
// - Seta de entrada para iniciais

procedure btnConvertClick(Sender: TObject);
// Event handler do botão "Converter AFN→AFD"

procedure btnRemoveEpsilonClick(Sender: TObject);
// Event handler do botão "Remover Epsilon"

procedure btnMinimizeClick(Sender: TObject);
// Event handler do botão "Minimizar AFD"

procedure btnUseAsInputClick(Sender: TObject);
// Copia resultado do AFN para entrada
// Permite workflow encadeado
```

---

## 💾 Gerenciamento de Memória {#memória}

### Conceitos Fundamentais em Pascal

**REGRA DE OURO:**
```
Todo .Create() PRECISA de correspondente .Free()
```

### Objetos que Requerem Free()

```pascal
// TStringList
var
  lista: TStringList;
begin
  lista := TStringList.Create;  // ALOCA MEMÓRIA
  try
    // usar lista
  finally
    lista.Free;  // LIBERA MEMÓRIA
  end;
end;

// TStringSet
var
  conjunto: TStringSet;
begin
  conjunto := TStringSet.Create;  // ALOCA MEMÓRIA
  try
    // usar conjunto
  finally
    conjunto.Free;  // LIBERA MEMÓRIA
  end;
end;
```

### Padrão try-finally

**SEMPRE usar para garantir limpeza:**

```pascal
// Padrão CORRETO
obj := TObject.Create;
try
  // código que pode dar exceção
finally
  obj.Free;  // Executa SEMPRE, mesmo com exceção
end;

// Padrão ERRADO (memory leak se houver exceção)
obj := TObject.Create;
// código que pode dar exceção
obj.Free;  // Pode não executar!
```

### Gerenciamento em Loops

**CRÍTICO:** Liberar objetos criados dentro de loops!

```pascal
// Memory leak MASSIVO!
for i := 0 to 1000 do
begin
  obj := TStringSet.Create;
  // usar obj
  // ESQUECEU DE DAR Free()! ← 1001 objetos vazando!
end;

// CORRETO
for i := 0 to 1000 do
begin
  obj := TStringSet.Create;
  try
    // usar obj
  finally
    obj.Free;  // Libera a cada iteração
  end;
end;
```

### Exemplos no Código

**RemoveEpsilonTransitions - 4 níveis de try-finally:**

```pascal
// Nível 1: Estruturas principais
try
  // ... código ...
finally
  Alphabet.Free;
  States.Free;
  parts.Free;
end;

// Nível 2: Loop de estados
for i := 0 to States.Count - 1 do
begin
  stateClosure := TStringSet.Create;
  try
    // ... código ...
  finally
    stateClosure.Free;
  end;
end;

// Nível 3: Loop de símbolos
for j := 0 to Alphabet.Count - 1 do
begin
  targetClosure := TStringSet.Create;
  try
    // ... código ...
  finally
    targetClosure.Free;
  end;
end;

// Nível 4: Loop de transições
tempClosure := TStringSet.Create;
try
  destClosure := ComputeEpsilonClosure(tempClosure, ...);
  try
    // ... código ...
  finally
    destClosure.Free;
  end;
finally
  tempClosure.Free;
end;
```

**ConvertAFNtoAFD - Gerenciamento de dfaMap:**

```pascal
dfaMap := TStringList.Create;
try
  // Criar TStringSet para cada estado do AFD
  for i := 0 to ... do
  begin
    setObj := TStringSet.Create;
    dfaMap.AddObject(key, TObject(setObj));
  end;
  
  // ... usar dfaMap ...
  
finally
  // Liberar TODOS os TStringSet
  for i := 0 to dfaMap.Count - 1 do
    TStringSet(dfaMap.Objects[i]).Free;
  
  dfaMap.Free;  // Liberar o container
end;
```

---

## 🔄 Fluxo de Execução {#fluxo}

### Workflow Completo

```
┌─────────────────────────────────────────────────┐
│  1. USUÁRIO CARREGA ARQUIVO                     │
│     - btnLoadFileClick                          │
│     - Lê arquivo .txt do disco                  │
│     - Preenche memoInput                        │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│  2. REMOVER EPSILON (Opcional)                  │
│     - btnRemoveEpsilonClick                     │
│     - RemoveEpsilonTransitions()                │
│     - AFN-ε → AFN                               │
│     - Resultado em memoNFAOutput                │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│  3. USAR COMO INPUT (Opcional)                  │
│     - btnUseAsInputClick                        │
│     - Copia memoNFAOutput → memoInput           │
│     - Permite encadear conversões               │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│  4. CONVERTER AFN → AFD                         │
│     - btnConvertClick                           │
│     - ConvertAFNtoAFD()                         │
│     - Subset Construction                       │
│     - Resultado em memoOutput                   │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│  5. MINIMIZAR AFD (Opcional)                    │
│     - btnMinimizeClick                          │
│     - MinimizeDFA()                             │
│     - Myhill-Nerode                             │
│     - Resultado em memoMinOutput                │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│  6. VISUALIZAÇÃO                                │
│     - PaintBoxXXXPaint event handlers           │
│     - DrawAutomaton() desenha diagramas         │
│     - 4 abas: AFN-ε, AFN, AFD, MinDFA           │
└─────────────────────────────────────────────────┘
```

### Detalhamento: RemoveEpsilonTransitions

```
RemoveEpsilonTransitions()
├─ 1. PARSE
│  ├─ Ler alfabeto (linha 0)
│  ├─ Ler estados (linha 1)
│  ├─ Ler iniciais (linha 2)
│  ├─ Ler finais (linha 3)
│  └─ Ler transições (linhas 4+)
│
├─ 2. DETECTAR EPSILON
│  └─ hasEpsilon = True se encontrou ε, epsilon, e, ou &
│
├─ 3. NOVOS INICIAIS
│  ├─ Para cada inicial i:
│  │  └─ Calcular ε-closure(i)
│  └─ NFAInitials = união de todos os closures
│
├─ 4. NOVOS FINAIS
│  ├─ Para cada estado q:
│  │  ├─ Calcular ε-closure(q)
│  │  └─ Se closure ∩ Finals ≠ ∅:
│  │     └─ q é final no AFN
│  └─ NFAFinals = todos os estados finais encontrados
│
├─ 5. NOVAS TRANSIÇÕES
│  ├─ Para cada estado p:
│  │  ├─ Calcular ε-closure(p)
│  │  ├─ Para cada símbolo a (não-epsilon):
│  │  │  ├─ targetSet = {}
│  │  │  ├─ Para cada q em ε-closure(p):
│  │  │  │  └─ Para cada transição q --a--> r:
│  │  │  │     └─ Adicionar ε-closure(r) ao targetSet
│  │  │  └─ Para cada t em targetSet:
│  │  │     └─ Adicionar p --a--> t ao AFN
│  │  └─ ...
│  └─ NFATransitions = todas as novas transições
│
├─ 6. OUTPUT
│  ├─ Escrever em memoNFAOutput
│  ├─ Atualizar PageControl
│  ├─ Habilitar btnUseAsInput
│  └─ Invalidate PaintBoxes
│
└─ 7. CLEANUP
   └─ Free() de todas as estruturas temporárias
```

### Detalhamento: ConvertAFNtoAFD

```
ConvertAFNtoAFD()
├─ 1. PARSE (igual RemoveEpsilonTransitions)
│  └─ Detectar epsilon e avisar
│
├─ 2. INICIALIZAÇÃO
│  ├─ initialSet = {estados iniciais do AFN}
│  ├─ workQ.Add(initialSet)
│  └─ dfaMap[initialSet.ToString] = initialSet
│
├─ 3. LOOP PRINCIPAL (BFS)
│  └─ Enquanto workQ não vazio:
│     ├─ curSet = workQ.Pop()
│     │
│     ├─ Para cada símbolo a:
│     │  ├─ nextSet = Move(curSet, a)
│     │  ├─ Se nextSet não vazio:
│     │  │  ├─ key = nextSet.ToString
│     │  │  ├─ Se key é novo:
│     │  │  │  └─ workQ.Add(nextSet)
│     │  │  │     dfaMap[key] = nextSet
│     │  │  └─ Adicionar transição curSet --a--> nextSet
│     │  └─ ...
│     │
│     └─ Se curSet ∩ Finals ≠ ∅:
│        └─ Marcar curSet como final
│
├─ 4. CONSTRUIR ARRAYS
│  ├─ DFAStates = keys de dfaMap
│  ├─ DFATransitions = todas as transições criadas
│  └─ DFAInitial = initialSet.ToString
│
├─ 5. OUTPUT
│  └─ Escrever em memoOutput
│
└─ 6. CLEANUP
   └─ Free() de dfaMap e TStringSets
```

---

## ⚡ Complexidade dos Algoritmos {#complexidade}

### Resumo Comparativo

| Algoritmo | Pior Caso | Caso Médio | Espaço |
|-----------|-----------|------------|--------|
| **Epsilon-Closure** | O(n + m) | O(n + m) | O(n) |
| **Remove Epsilon** | O(n² · m · \|Σ\|) | O(n · m) | O(n² · \|Σ\|) |
| **AFN → AFD** | O(2^n · \|Σ\| · n) | O(n² · \|Σ\|) | O(2^n) |
| **Minimização** | O(n · m · log n) | O(n · m · log n) | O(n²) |

**Legenda:**
- `n` = número de estados
- `m` = número de transições
- `|Σ|` = tamanho do alfabeto

### Análise Detalhada

#### 1. ComputeEpsilonClosure
```
COMPLEXIDADE: O(n + m_ε)

Justificativa:
- Cada estado é visitado no máximo 1 vez
- Cada epsilon-transição é examinada no máximo 1 vez
- Pilha: O(n) espaço

Exemplo:
  10 estados, 20 epsilon-transições
  → ~30 operações
```

#### 2. RemoveEpsilonTransitions
```
COMPLEXIDADE: O(n² · m · |Σ|)

Estrutura:
  for estado in Estados:              // n iterações
    ε-closure(estado)                 // O(n + m)
    for símbolo in Alfabeto:          // |Σ| iterações
      for q in closure:               // n iterações
        for trans in Transições:      // m iterações
          ε-closure(destino)          // O(n + m)
          
Total: n · (n+m) · |Σ| · n · m · (n+m) = O(n² · m · |Σ|)

Exemplo:
  10 estados, 30 transições, alfabeto {a,b}
  → 10 · 10 · 2 · 30 · 10 = 60,000 operações (pior caso)
  
Na prática: muito menos (closures pequenos)
```

#### 3. ConvertAFNtoAFD
```
COMPLEXIDADE: O(2^n · |Σ| · n)

Pior Caso - Explosão Total:
  Estados AFN: n
  Estados AFD: 2^n (todos os subconjuntos possíveis)
  
  for estado_dfd in 2^n:              // 2^n iterações
    for símbolo in |Σ|:               // |Σ| iterações
      Move(estado, símbolo)           // O(n · m)
      
Total: 2^n · |Σ| · n · m = O(2^n · |Σ| · n)

Exemplo Explosão:
  AFN: 20 estados, alfabeto {a,b}
  AFD: até 2^20 = 1,048,576 estados!
  Operações: 1M · 2 · 20 = 40M+ operações
  
Caso Médio:
  Geralmente: O(n² · |Σ|)
  Apenas subconjuntos alcançáveis são criados
```

#### 4. MinimizeDFA
```
COMPLEXIDADE: O(n · m · log n)

Algoritmo de Refinamento:
  Iterações: log n (número de refinamentos possíveis)
  
  for iteração in log n:              // log n iterações
    for partição in Partições:        // O(n) no total
      for símbolo in |Σ|:             // |Σ| iterações
        for transição in m:           // m iterações
          Verificar destino           // O(1) com hash
          
Total: log n · n · |Σ| · m = O(n · m · log n)

Exemplo:
  AFD: 100 estados, 500 transições, alfabeto {a,b}
  Iterações: log₂(100) ≈ 7
  Operações: 7 · 100 · 2 · 500 = 700,000
```

---

## 📊 Casos de Teste

### Teste de Epsilon (test_epsilon.txt)
```
Entrada:
  ε a b
  q0 q1 q2 q3
  q0
  q3
  q0 ε q1
  q1 a q2
  q2 ε q3

Saída AFN (sem epsilon):
  Estados Iniciais: q0, q1
  Estados Finais: q2, q3
  Transições:
    q0 --a--> q2
    q0 --a--> q3
    q1 --a--> q2
    q1 --a--> q3
```

### Teste de Explosão (test_explosao.txt)
```
AFN: 6 estados
AFD: ~15 estados compostos

Demonstra crescimento exponencial na construção de subconjuntos.
```

---

## 🎯 Pontos-Chave para Apresentação

### 1. **Estruturas de Dados**
- TStringSet para representar conjuntos
- TTransitionArray para transições
- Separação clara AFN-ε, AFN, AFD, MinDFA

### 2. **Algoritmos Clássicos**
- Epsilon-closure: DFS com pilha
- Subset Construction: BFS com fila
- Minimização: Partição-refinamento

### 3. **Gerenciamento de Memória**
- Create → Free em todos os objetos
- try-finally para garantir limpeza
- 4 níveis de aninhamento em alguns loops

### 4. **Complexidade**
- Epsilon: O(n + m) - linear
- Remove Epsilon: O(n² · m · |Σ|) - polinomial
- AFN→AFD: O(2^n · |Σ|) - exponencial (pior caso)
- Minimização: O(n · m · log n) - quase-linear

### 5. **Interface**
- Event-driven com Lazarus LCL
- 4 diagramas renderizados nativamente
- Workflow encadeado com "Usar como Input"

---

## 📖 Referências

1. **Hopcroft, Motwani, Ullman** - "Introduction to Automata Theory, Languages, and Computation" (3rd Ed)
2. **Sipser, Michael** - "Introduction to the Theory of Computation" (3rd Ed)
3. **Lazarus Documentation** - https://wiki.lazarus.freepascal.org/
4. **Free Pascal Compiler** - https://www.freepascal.org/docs.html

---

**Autor:** Henrique Freitas  
**Contato:** [GitHub](https://github.com/peudias)  
**Última Atualização:** Dezembro 2025
