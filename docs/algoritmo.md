# 🔬 Algoritmo de Conversão AFN → AFD

Este documento explica em detalhes o algoritmo de **Construção de Subconjuntos** (Subset Construction) usado para converter AFN em AFD.

## � Conversão Automática Inteligente

### Detecção e Processamento em 2 Etapas

O programa agora detecta automaticamente epsilon-transições e executa as etapas necessárias:

#### Quando clicar "AFN → AFD":

**Se o autômato TEM epsilon-transições:**
1. ✅ Detecta `ε` no input automaticamente
2. 📢 Mostra alerta informativo
3. 🔄 Executa **Etapa 1**: AFN-ε → AFN (remove epsilon)
4. 🔄 Executa **Etapa 2**: AFN → AFD (determiniza)
5. 📊 Mostra resultado final (AFD)
6. 🎯 Muda para abas do AFD automaticamente

**Se o autômato NÃO TEM epsilon:**
1. ✅ Pula remoção de epsilon
2. 🔄 Executa apenas AFN → AFD
3. 📊 Mostra resultado
4. 🎯 Muda para abas do AFD

#### Validação Inteligente

**"AFN-ε → AFN" bloqueado quando:**
- ❌ Autômato não possui epsilon-transições
- 💬 Mostra mensagem: "Use diretamente AFN → AFD"
- 🛡️ Evita processamento desnecessário

**Vantagens:**
- ⚡ **Menos cliques**: Um botão faz tudo automaticamente
- 🧠 **Inteligente**: Detecta o que é necessário
- 🛡️ **Seguro**: Bloqueia operações inválidas
- 🎯 **Intuitivo**: Navegação automática de abas

### Navegação Automática de Abas

Após cada conversão bem-sucedida:

**AFN-ε → AFN:**
- Muda para aba "📊 Resultado AFN"
- Muda para aba "📊 Diagrama AFN"

**AFN → AFD:**
- Muda para aba "📊 Resultado AFD"
- Muda para aba "📊 Diagrama AFD"

**Minimizar AFD:**
- Muda para aba "📊 AFD Minimizado"
- Muda para aba "📊 Diagrama MinDFA"

**Benefício:** Você vê imediatamente o resultado sem precisar procurar as abas!

---

## �🎯 Conceito Fundamental

A ideia central é:

> **Cada estado do AFD representa um CONJUNTO de estados do AFN**

### Exemplo Visual

```
AFN:                           AFD:
  →(q0)--a-->(q1)              →{q0}--a-->{q0,q1}
     ↓         ↓                   ↓          ↓
     a         b                   b          b
     ↓         ↓                   ↓          ↓
   (q0)     ((q2))               {q0}     {q0,q2}
```

## 📋 Algoritmo Passo a Passo

### Fase 1: Inicialização

```pascal
procedure BuildDFA;
begin
  // 1. Criar estado inicial do DFA
  initialSet := ConjuntoDeEstadosIniciaisDoAFN;
  
  // 2. Adicionar à fila de processamento
  workQueue.Add(initialSet);
  
  // 3. Marcar como visitado
  visited.Add(initialSet);
end;
```

**O que acontece:**
- ✅ Estado inicial do DFA = `{q0}` (se AFN tem q0 inicial)
- ✅ Se AFN tem múltiplos iniciais: `{q0, q1, q2, ...}`

---

### Fase 2: Loop Principal (BFS)

```pascal
while workQueue.Count > 0 do
begin
  // 1. Pegar próximo conjunto da fila
  currentSet := workQueue.Pop();
  
  // 2. Verificar se é final
  if currentSet.Contains(anyFinalStateFromNFA) then
    dfaFinals.Add(currentSet);
  
  // 3. Para cada símbolo do alfabeto...
  for symbol in alphabet do
  begin
    // 4. Calcular destino
    destSet := Move(currentSet, symbol);
    
    // 5. Criar transição
    AddTransition(currentSet, symbol, destSet);
    
    // 6. Se é novo, adicionar à fila
    if not visited.Contains(destSet) then
    begin
      workQueue.Add(destSet);
      visited.Add(destSet);
    end;
  end;
end;
```

**O que acontece:**
1. Processa cada conjunto de estados (BFS = largura primeiro)
2. Para cada símbolo, calcula onde podemos ir
3. Cria novas transições no DFA
4. Continua até fila vazia

---

### Fase 3: Função Move

```pascal
function Move(states: TSet; symbol: char): TSet;
var
  src, dest: string;
  trans: TTransition;
begin
  Result := EmptySet;
  
  // Para cada estado no conjunto...
  for src in states do
  begin
    // Para cada transição do AFN...
    for trans in nfaTransitions do
    begin
      // Se origem = estado atual E símbolo = símbolo atual
      if (trans.source = src) and (trans.symbol = symbol) then
      begin
        // Adicionar destino ao resultado
        Result.Add(trans.dest);
      end;
    end;
  end;
end;
```

**Exemplo:**
```
AFN tem:
  q0 --a--> q0
  q0 --a--> q1

Move({q0}, 'a') = {q0, q1}
```

---

## 🧮 Exemplo Completo

### AFN de Entrada

```
Alfabeto: {a, b}
Estados: {q0, q1, q2}
Inicial: q0
Final: q2

Transições:
  q0 --a--> q0
  q0 --b--> q0
  q0 --a--> q1  ← Não-determinismo!
  q1 --b--> q2
```

### Execução do Algoritmo

#### **Iteração 1:**
```
Estado atual: {q0}
workQueue: [{q0}]

Para 'a': Move({q0}, 'a') = {q0, q1}
  → Transição: {q0} --a--> {q0,q1}
  → Adicionar {q0,q1} à fila

Para 'b': Move({q0}, 'b') = {q0}
  → Transição: {q0} --b--> {q0}
  → Já visitado, não adicionar

workQueue: [{q0,q1}]
```

#### **Iteração 2:**
```
Estado atual: {q0,q1}
workQueue: [{q0,q1}]

Para 'a': 
  Move({q0}, 'a') = {q0, q1}
  Move({q1}, 'a') = ∅
  Move({q0,q1}, 'a') = {q0, q1}
  → Transição: {q0,q1} --a--> {q0,q1}
  → Já visitado

Para 'b':
  Move({q0}, 'b') = {q0}
  Move({q1}, 'b') = {q2}
  Move({q0,q1}, 'b') = {q0, q2}
  → Transição: {q0,q1} --b--> {q0,q2}
  → Adicionar {q0,q2} à fila

workQueue: [{q0,q2}]
```

#### **Iteração 3:**
```
Estado atual: {q0,q2}
workQueue: [{q0,q2}]

q2 é final? SIM!
  → {q0,q2} é estado final do DFA

Para 'a': Move({q0,q2}, 'a') = {q0, q1}
  → Transição: {q0,q2} --a--> {q0,q1}
  → Já visitado

Para 'b': Move({q0,q2}, 'b') = {q0}
  → Transição: {q0,q2} --b--> {q0}
  → Já visitado

workQueue: [] → FIM!
```

### AFD Resultante

```
Estados: {q0}, {q0,q1}, {q0,q2}
Inicial: {q0}
Finais: {q0,q2}

Transições:
  {q0}     --a--> {q0,q1}
  {q0}     --b--> {q0}
  {q0,q1}  --a--> {q0,q1}
  {q0,q1}  --b--> {q0,q2}
  {q0,q2}  --a--> {q0,q1}
  {q0,q2}  --b--> {q0}
```

---

## 🎨 Estruturas de Dados

### TStringSet

```pascal
type
  TStringSet = class
  private
    FList: TStringList;
  public
    procedure Add(const S: string);
    function Contains(const S: string): Boolean;
    function GetName: string;  // Ex: "{q0,q1}"
  end;
```

**Responsabilidade:**
- Representar conjuntos de estados
- Gerar nomes formatados: `{q0,q1,q2}`

### TTransition

```pascal
type
  TTransition = record
    source: string;  // "{q0}"
    symbol: char;    // 'a'
    dest: string;    // "{q0,q1}"
  end;
```

**Responsabilidade:**
- Armazenar uma transição do DFA

### Mapeamento de Conjuntos

```pascal
var
  dfaMap: TStringList;  // Nome → TStringSet
begin
  dfaMap.AddObject('{q0}', TStringSet.Create(['q0']));
  dfaMap.AddObject('{q0,q1}', TStringSet.Create(['q0','q1']));
end;
```

**Responsabilidade:**
- Mapear nomes de estados para objetos TStringSet
- Evitar duplicação de conjuntos

---

## ⚡ Análise de Complexidade

### Complexidade de Tempo

**Pior caso:**
- 🔴 **O(2^n × |Σ|)** onde:
  - `n` = número de estados do AFN
  - `|Σ|` = tamanho do alfabeto

**Por quê?**
- DFA pode ter até **2^n** estados (todos os subconjuntos possíveis)
- Para cada estado, processamos **|Σ|** símbolos

**Caso médio:**
- 🟡 **O(n^2 × |Σ|)**
- Na prática, poucos AFNs geram todos os 2^n estados

### Complexidade de Espaço

**Pior caso:**
- 🔴 **O(2^n)** - armazenar todos os estados do DFA

**Caso médio:**
- 🟢 **O(n)** - maioria dos AFNs não explode

### Exemplos Reais

| AFN States | Alfabeto | DFA States (pior) | DFA States (médio) |
|------------|----------|-------------------|-------------------|
| 3 | 2 | 8 | 3-4 |
| 4 | 2 | 16 | 4-8 |
| 5 | 2 | 32 | 5-10 |
| 10 | 2 | 1024 | 10-20 |

---

## 🚀 Otimizações Implementadas

### 1. BFS ao invés de DFS

```pascal
// ✅ BFS (implementado)
workQueue: TQueue;  // FIFO

// ❌ DFS (não usado)
workStack: TStack;  // LIFO
```

**Vantagem:** Estados mais "próximos" são processados primeiro

### 2. Cache de Conjuntos Visitados

```pascal
if visited.Contains(newSet) then
  Continue;  // Não processar novamente
```

**Vantagem:** Evita processamento duplicado

### 3. Early Return em Move

```pascal
function Move(states: TSet; symbol: char): TSet;
begin
  if states.IsEmpty then
    Exit(EmptySet);  // ← Retorno antecipado
  // ...
end;
```

**Vantagem:** Evita loops desnecessários

---

## 🎓 Propriedades Matemáticas

### Teorema 1: Equivalência
```
∀w ∈ Σ*: w é aceita pelo AFN ⟺ w é aceita pelo AFD
```

**Prova:** Por construção do algoritmo

### Teorema 2: Determinismo
```
∀q ∈ Q_DFA, ∀a ∈ Σ: ∃! q' tal que δ(q,a) = q'
```

**Prova:** Move sempre retorna exatamente um conjunto

### Teorema 3: Explosão Exponencial
```
No pior caso: |Q_DFA| = 2^|Q_AFN|
```

**Prova:** Cada subconjunto de Q_AFN pode ser um estado do DFA

---

## 💡 Dicas de Implementação

### ✅ Boas Práticas

1. **Use conjuntos ordenados** para nomes consistentes
   ```pascal
   "{q0,q1}" = "{q1,q0}"  // Devem ser iguais!
   ```

2. **Trate o conjunto vazio**
   ```pascal
   if Move(set, symbol).IsEmpty then
     Result := '{∅}';  // Estado morto
   ```

3. **Cache de transições**
   ```pascal
   var transitionCache: TDictionary<TPair<TSet, char>, TSet>;
   ```

### ❌ Erros Comuns

1. **Esquecer de marcar estados finais**
   ```pascal
   // ❌ ERRADO
   if currentSet = nfaFinal then...
   
   // ✅ CERTO
   if currentSet.ContainsAny(nfaFinals) then...
   ```

2. **Não tratar múltiplos iniciais**
   ```pascal
   // ❌ ERRADO
   initialState := nfaInitial[0];
   
   // ✅ CERTO
   initialState := UnionOf(nfaInitials);
   ```

3. **Comparação de conjuntos errada**
   ```pascal
   // ❌ ERRADO
   if set1.Name = set2.Name then...  // Ordem importa!
   
   // ✅ CERTO
   if set1.Equals(set2) then...  // Compara conteúdo
   ```

---

## 🔗 Próximos Passos

Agora que você entende o algoritmo:

- 📐 [Veja a Análise de Complexidade Detalhada](complexidade.md)
- 💻 [Explore a Implementação no Código](api.md)
- 🧪 [Teste com Casos Práticos](testes.md)

---

**💡 Dica:** Implemente você mesmo! É a melhor forma de entender profundamente.
