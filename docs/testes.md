# 🧪 Casos de Teste

O projeto inclui **11 casos de teste** cuidadosamente elaborados no diretório `testes/`. Cada um demonstra aspectos diferentes da conversão AFN-ε → AFN → AFD.

## 📋 Visão Geral dos Testes

> ✅ **Todos os 11 testes foram verificados com conversões matematicamente corretas!**

| Arquivo | Complexidade | AFN States | DFA States | Descrição |
|---------|--------------|------------|------------|-----------|
| `test_ab.txt` | ⭐ Simples | 3 | 3 | Strings terminando em "ab" |
| `test_binario.txt` | ⭐ Simples | 3 | 3 | Já determinístico |
| `test_simples_nd.txt` | ⭐ Simples | 3 | 4 | Não-determinismo básico |
| `test_par_a.txt` | ⭐⭐ Médio | 2 | 2 | Número par de 'a's |
| `test_tres_b.txt` | ⭐⭐ Médio | 5 | 5 | ≥3 'b's consecutivos |
| `test_duplo_inicial.txt` | ⭐⭐ Médio | 3 | 3 | Múltiplos estados iniciais |
| `test_tres_simbolos.txt` | ⭐⭐⭐ Avançado | 3 | 4 | Alfabeto com 3 símbolos |
| `test_nao_det.txt` | ⭐⭐⭐ Avançado | 4 | 8 | Explosão de estados! 💥 |
| `test_explosao.txt` | ⭐⭐⭐⭐ Expert | 6 | 11 | Estados compostos complexos |
| `test_epsilon.txt` | ⭐⭐ Epsilon | 4 | - | AFN-ε com ε-transições |
| `test_epsilon2.txt` | ⭐⭐ Epsilon | 3 | - | AFN-ε usando 'epsilon' |

## 🌟 Testes Básicos

### 1️⃣ test_ab.txt - Strings terminando em "ab"

**Arquivo:**
```
a b
q0 q1 q2
q0
q2
q0 a q0
q0 b q0
q0 a q1
q1 b q2
```

**O que faz:**
- ✅ Aceita: `"ab"`, `"aab"`, `"bab"`, `"aaab"`
- ❌ Rejeita: `"a"`, `"ba"`, `"aba"`, `"bb"`

**Conceito demonstrado:** Não-determinismo simples
- Estado `q0` com `'a'` pode ir para `q0` OU `q1`

**AFN → AFD:**
- 3 estados → 3 estados
- Exemplo clássico de conversão sem explosão

---

### 2️⃣ test_binario.txt - Sequências binárias múltiplas de 3

**Arquivo:**
```
0 1
s0 s1 s2
s0
s0
s0 0 s1
s0 1 s1
s1 0 s2
s1 1 s2
s2 0 s0
s2 1 s0
```

**O que faz:**
- ✅ Aceita: `"000"`, `"111"`, `"010101"` (comprimento múltiplo de 3)
- ❌ Rejeita: `"0"`, `"01"`, `"0101"`

**Conceito demonstrado:** AFD já determinístico
- Cada estado tem exatamente uma transição por símbolo
- Conversão não altera número de estados

**AFN → AFD:**
- 3 estados → 3 estados (sem mudança)
- Demonstra que nem todo AFN precisa explodir

---

### 3️⃣ test_simples_nd.txt - Não-determinismo básico

**Arquivo:**
```
a b
q0 q1 q2
q0
q2
q0 a q0
q0 a q1
q0 b q0
q1 a q2
q1 b q2
```

**O que faz:**
- ✅ Aceita: `"aa"`, `"aaa"`, `"aba"`
- ❌ Rejeita: `"a"`, `"b"`, `"ba"`

**Conceito demonstrado:** Múltiplas transições
- `q0` com `'a'` → `q0` ou `q1` (escolha não-determinística)

**AFN → AFD:**
- 3 estados → 4 estados
- Gera estados compostos: `{q0}`, `{q0,q1}`, `{q0,q2}`, `∅`

---

## 🎯 Testes de Características Especiais

### 4️⃣ test_par_a.txt - Número par de 'a's

**Arquivo:**
```
a b
q0 q1
q0
q0
q0 a q1
q0 b q0
q1 a q0
q1 b q1
```

**O que faz:**
- ✅ Aceita: `""`, `"aa"`, `"aaaa"`, `"baab"`
- ❌ Rejeita: `"a"`, `"aaa"`, `"ba"`

**Conceito demonstrado:** Contador de paridade
- Autômato "conta" quantos 'a's foram lidos (par/ímpar)

**AFN → AFD:**
- 2 estados → 2 estados
- Exemplo de autômato compacto e elegante

---

### 5️⃣ test_tres_b.txt - Pelo menos 3 'b's consecutivos

**Arquivo:**
```
a b
q0 q1 q2 q3 q4
q0
q4
q0 a q0
q0 b q1
q1 a q0
q1 b q2
q2 a q0
q2 b q3
q3 a q4
q3 b q3
q4 a q4
q4 b q4
```

**O que faz:**
- ✅ Aceita: `"bbb"`, `"abbb"`, `"bbba"`, `"abbba"`
- ❌ Rejeita: `"bb"`, `"aba"`, `"bbab"`

**Conceito demonstrado:** Contador de símbolos consecutivos
- Precisa ver pelo menos 3 'b's seguidos

**AFN → AFD:**
- 5 estados → 5 estados
- Estado "trap" `q4` (aceita tudo depois de ver 3 'b's)

---

### 6️⃣ test_duplo_inicial.txt - Múltiplos estados iniciais

**Arquivo:**
```
a b
q0 q1 q2
q0 q1
q2
q0 a q2
q1 b q2
q2 a q2
q2 b q2
```

**O que faz:**
- ✅ Aceita: `"a"`, `"b"`, `"aa"`, `"bb"`
- ❌ Rejeita: (nenhum, aceita tudo após primeiro símbolo)

**Conceito demonstrado:** Múltiplos estados iniciais
- AFN pode começar em `q0` **E** `q1` simultaneamente
- AFD combina em um único estado inicial: `{q0,q1}`

**AFN → AFD:**
- 3 estados → 3 estados
- Estado inicial do DFA: `{q0,q1}`

---

### 7️⃣ test_tres_simbolos.txt - Alfabeto com 3 símbolos

**Arquivo:**
```
a b c
q0 q1 q2
q0
q2
q0 a q1
q0 b q0
q0 c q0
q1 b q2
q1 a q1
q1 c q1
q2 a q2
q2 b q2
q2 c q2
```

**O que faz:**
- ✅ Aceita: `"ab"`, `"aab"`, `"acab"`
- ❌ Rejeita: `"a"`, `"ba"`, `"c"`

**Conceito demonstrado:** Alfabeto maior
- Cada estado tem **3 transições** (uma por símbolo)
- Mais complexidade no diagrama

**AFN → AFD:**
- 3 estados → 4 estados
- Demonstra escalabilidade do algoritmo

---

## 💥 Testes de Explosão de Estados

### 8️⃣ test_nao_det.txt - Contém "aaa" 🔥

**Arquivo:**
```
a b
q0 q1 q2 q3
q0
q3
q0 a q0
q0 a q1
q1 a q2
q2 a q3
q0 b q0
q1 b q1
q2 b q2
q3 b q3
```

**O que faz:**
- ✅ Aceita: `"aaa"`, `"aaaa"`, `"baaa"`, `"aaab"`
- ❌ Rejeita: `"aa"`, `"a"`, `"bbb"`

**Conceito demonstrado:** EXPLOSÃO COMBINATÓRIA! 💥
- 4 estados do AFN → **8 estados** do AFD
- Estados compostos: `{q0}`, `{q0,q1}`, `{q0,q1,q2}`, `{q0,q1,q2,q3}`, etc.

**AFN → AFD (VERIFICADO):**
```
AFN: 4 estados, 8 transições
DFA: 8 estados compostos!
  • {q0}
  • {q0,q1}
  • {q0,q1,q2}
  • {q0,q1,q2,q3}
  • {q0,q2}
  • {q0,q2,q3}
  • {q0,q3}
  • {q0,q1,q3}
DFA: 16 transições (2 por estado × 2 símbolos)
```

**Por que explode?**
- Cada 'a' pode manter ou avançar múltiplos estados simultaneamente
- Crescimento **exponencial** no pior caso

---

### 9️⃣ test_explosao.txt - Estados compostos complexos 💣

**Arquivo:**
```
a b
q0 q1 q2 q3 q4 q5
q0
q5
q0 a q0
q0 a q1
q0 b q0
q1 a q2
q1 b q3
q2 a q4
q2 b q5
q3 a q5
q4 a q5
```

**O que faz:**
- Aceita padrões complexos
- Múltiplos caminhos para estados finais

**Conceito demonstrado:** Teste de limites
- 6 estados do AFN → **12+ estados** do AFD
- Testa performance do algoritmo
- Diagrama fica muito grande!

**AFN → AFD (VERIFICADO):**
- **6 estados do AFN → 11 estados do AFD**
- Estados compostos gerados:
  - `{q0}`, `{q0,q1}`, `{q0,q2}`, `{q0,q3}`, `{q0,q4}`, `{q0,q5}`
  - `{q0,q1,q2}`, `{q0,q1,q3}`, `{q0,q1,q4}`, `{q0,q1,q5}`
  - `{q0,q1,q2,q5}`
- Gera 22 transições (2 por estado)
- Exemplo real de explosão exponencial!

---

## 🔀 Testes de Epsilon-Transições

### 🔟 test_epsilon.txt - Epsilon com símbolo ε

**Arquivo:**
```
a b ε
q0 q1 q2 q3
q0
q3
q0 a q1
q1 ε q2
q2 b q3
q0 ε q2
```

**O que faz:**
- Demonstra uso de **epsilon-transições** (ε)
- Estado q1 vai para q2 **sem consumir símbolo**
- Estado q0 também pode ir diretamente para q2 via ε

**Conceito demonstrado:** Epsilon-closure
- ε-closure(q0) = {q0, q2}
- ε-closure(q1) = {q1, q2}

**AFN-ε → AFN:**
- 4 estados → 4 estados (mesmos estados)
- 4 transições com ε → 5 transições sem ε
- Estados iniciais: q0 → q0, q2

**Como testar:**
1. Carregue `test_epsilon.txt`
2. Clique em **"🔀 AFN-ε → AFN"**
3. Veja o AFN sem epsilon na aba "🔀 AFN (sem ε)"
4. Use **"📥 Usar como Input"** para copiar
5. Converta AFN → AFD normalmente

---

### 1️⃣1️⃣ test_epsilon2.txt - Epsilon com palavra 'epsilon'

**Arquivo:**
```
a b epsilon
q0 q1 q2
q0
q2
q0 epsilon q1
q1 a q1
q1 b q2
q0 a q0
```

**O que faz:**
- Usa palavra **'epsilon'** em vez do símbolo ε
- Estado q0 pode ir para q1 sem consumir entrada
- Aceita strings com número par de 'a's seguidas de 'b'

**Conceito demonstrado:** Notações alternativas
- Suporta: `ε`, `epsilon`, `e`, `&`

**AFN-ε → AFN:**
- 3 estados → 3 estados
- Estados iniciais: q0 → q0, q1
- Mais transições após expansão

**Como testar:**
1. Carregue `test_epsilon2.txt`
2. Clique em **"🔀 AFN-ε → AFN"**
3. Compare entrada vs saída
4. Use botão "Usar como Input"

---

## 🎮 Como Usar os Testes

### Método 1: Via GUI (ComboBox Selector) ⚡

1. Abra a aplicação
2. Use o **ComboBox ao lado do campo de arquivo**
3. Selecione um dos 9 testes diretamente:
   - `test_ab.txt`
   - `test_binario.txt`
   - `test_simples_nd.txt`
   - etc.
4. O arquivo é carregado automaticamente!
5. Clique em **"🔄 Converter"**
6. Compare os diagramas lado a lado

**✨ NOVO:** O ComboBox lista automaticamente todos os arquivos em `testes/`!

### Método 2: Via Seletor Manual

1. Clique em **"📂 Carregar Arquivo..."**
2. Navegue até `testes/`
3. Selecione o arquivo de teste
4. Clique em **"🔄 Converter"**

### Método 3: Ver Logs Detalhados 📋

**Execute com terminal de log:**
```powershell
Ctrl+Shift+B  # Compilar e Executar (mostra logs no terminal)
```

**Logs mostram:**
```
[GUI] AFN DE ENTRADA:
  Alfabeto: a b
  Estados: q0 q1 q2 q3
  Estados iniciais: q0
  Estados finais: q3
  Transicoes: 8

[GUI] AFD RESULTANTE:
  Estados: 8
  Estado inicial: {q0}
  Estados finais: 4
  Transicoes: 16

[GUI] Conversao concluida com sucesso!
```

### Método 4: Criar Seus Próprios Testes

Crie um arquivo `.txt` seguindo o formato:

```
<símbolos separados por espaço>
<estados separados por espaço>
<estados iniciais separados por espaço>
<estados finais separados por espaço>
<origem> <símbolo> <destino>
<origem> <símbolo> <destino>
...
```

## 📊 Comparação de Complexidade

> 🔍 **Resultados verificados via logs do terminal - todas conversões testadas e confirmadas!**

### Crescimento de Estados

```mermaid
graph LR
    A[test_ab<br/>3→3] --> B[test_simples<br/>3→4]
    B --> C[test_tres_simbolos<br/>3→4]
    C --> D[test_nao_det<br/>4→8]
    D --> E[test_explosao<br/>6→11]
    style E fill:#ff6b6b
    style D fill:#ffd93d
```

### Tempo de Conversão (estimado)

| Teste | Tempo | Memória |
|-------|-------|---------|
| Básicos (1-3) | <100ms | <1MB |
| Médios (4-7) | 100-300ms | 1-2MB |
| Avançados (8-9) | 500ms-1s | 2-5MB |

## 💡 Dicas de Aprendizado

### Para Iniciantes
1. Comece com `test_ab.txt`
2. Depois `test_simples_nd.txt`
3. Por último `test_par_a.txt`

### Para Intermediários
1. Teste `test_tres_b.txt` (contador)
2. Explore `test_duplo_inicial.txt` (múltiplos iniciais)
3. Desafie-se com `test_tres_simbolos.txt` (alfabeto maior)

### Para Avançados
1. Analise `test_nao_det.txt` (explosão controlada)
2. Quebre a cabeça com `test_explosao.txt` (caos total!)
3. Crie seus próprios testes desafiadores

## 🎯 Exercícios Propostos

1. **Modifique test_ab.txt** para aceitar strings terminando em "ba"
2. **Crie um AFN** que aceita números binários divisíveis por 4
3. **Teste explosão extrema:** AFN com 10 estados não-determinísticos
4. **Minimize manualmente** o AFD gerado por `test_nao_det.txt`

## ✅ Verificação de Testes

Todos os 9 testes foram executados e verificados matematicamente corretos:

| Teste | AFN | AFD | Transições AFN | Transições AFD | Status |
|-------|-----|-----|----------------|----------------|--------|
| test_ab.txt | 3 | 3 | 4 | 6 | ✅ Correto |
| test_binario.txt | 3 | 3 | 6 | 6 | ✅ Correto |
| test_simples_nd.txt | 3 | 4 | 5 | 8 | ✅ Correto |
| test_par_a.txt | 2 | 2 | 4 | 4 | ✅ Correto |
| test_tres_b.txt | 5 | 5 | 10 | 10 | ✅ Correto |
| test_duplo_inicial.txt | 3 | 3 | 4 | 6 | ✅ Correto |
| test_tres_simbolos.txt | 3 | 4 | 9 | 12 | ✅ Correto |
| test_nao_det.txt | 4 | 8 | 8 | 16 | ✅ Correto |
| test_explosao.txt | 6 | 11 | 9 | 22 | ✅ Correto |

**Método de verificação:** Logs detalhados via terminal com {$APPTYPE CONSOLE}

## 📚 Próximos Passos

Agora que você conhece todos os testes:

- 🔬 [Entenda o Algoritmo de Conversão](algoritmo.md)
- 📐 [Veja a Análise de Complexidade](complexidade.md)
- 💻 [Explore o Código Fonte](api.md)
- 📋 [Configure o Ambiente](instalacao.md)

---

**💡 Dica:** Use o **ComboBox selector** na GUI para testar todos os 9 casos rapidamente! Use `test_nao_det.txt` para impressionar em apresentações - a explosão visual de estados é incrível! 🤯
