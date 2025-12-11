# Epsilon-Transições (ε-transições)

## O que são Epsilon-Transições?

**Epsilon-transições** (também chamadas de **ε-transições** ou **transições vazias**) são transições especiais em autômatos finitos não-determinísticos que permitem mudar de estado **sem consumir nenhum símbolo da entrada**.

## Notação

A transição epsilon pode ser representada de várias formas:
- `ε` (epsilon grego)
- `epsilon` (palavra completa)
- `e` (letra e)
- `&` (símbolo alternativo)

No nosso conversor, você pode usar qualquer uma dessas notações!

## Exemplo Visual

### AFN-ε (com epsilon-transições)

```
    a         ε         b
q0 ──→ q1 ──→ q2 ──→ q3
│                      │
└──────── ε ──────────┘
```

**Representação textual:**
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

### AFN (sem epsilon-transições)

Após remover as epsilon-transições, obtemos um AFN equivalente:

```
    a         b
q0 ──→ q1 ──→ q3
│              │
└──── b ───────┘
```

**Representação textual:**
```
a b
q0 q1 q2 q3
q0 q2
q3
q0 a q1
q1 b q3
q2 b q3
q0 b q3
```

## Por que usar Epsilon-Transições?

### Vantagens

1. **Facilita a construção de autômatos**: Epsilon-transições tornam mais fácil combinar autômatos menores em autômatos maiores
2. **Expressões regulares**: Útil na construção de Thompson para converter regex → AFN-ε
3. **Simplicidade conceitual**: Permite modelar "escolhas" ou "opções" de forma mais intuitiva

### Aplicações Práticas

- **Construção de Thompson**: Converte expressões regulares em AFN-ε
- **Concatenação**: Une dois autômatos facilmente
- **União**: Combina alternativas (operador `|`)
- **Fecho de Kleene**: Implementa repetições (`*`)

## Algoritmo de Remoção de Epsilon

### 1. Epsilon-Fecho (ε-closure)

O **epsilon-fecho** de um estado q é o conjunto de todos os estados alcançáveis a partir de q usando apenas epsilon-transições (incluindo o próprio q).

**Algoritmo:**
```
ε-closure(q):
    resultado = {q}
    pilha = [q]
    
    enquanto pilha não vazia:
        estado = desempilhar()
        
        para cada transição (estado, ε, destino):
            se destino não está em resultado:
                adicionar destino ao resultado
                empilhar destino
    
    retornar resultado
```

### 2. Conversão AFN-ε → AFN

**Algoritmo completo:**

1. **Novos estados iniciais**: 
   - Calcular ε-closure de cada estado inicial original
   - Os novos iniciais são todos os estados nesses fechos

2. **Novos estados finais**:
   - Um estado q é final se ε-closure(q) contém algum estado final original

3. **Novas transições**:
   - Para cada estado q e símbolo a (exceto ε):
     - Calcular ε-closure(q)
     - Para cada estado p em ε-closure(q):
       - Para cada transição (p, a, r):
         - Adicionar todos os estados de ε-closure(r) como destinos de (q, a, ...)

4. **Remover epsilon do alfabeto**: O novo alfabeto não contém ε

## Exemplo Prático

### Entrada (AFN-ε)

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

### Passo 1: Calcular ε-closure de cada estado

- ε-closure(q0) = {q0, q2}
- ε-closure(q1) = {q1, q2}
- ε-closure(q2) = {q2}
- ε-closure(q3) = {q3}

### Passo 2: Determinar novos iniciais e finais

- **Iniciais**: ε-closure({q0}) = {q0, q2}
- **Finais**: 
  - q0 é final? ε-closure(q0) = {q0, q2} não contém q3 → NÃO
  - q1 é final? ε-closure(q1) = {q1, q2} não contém q3 → NÃO
  - q2 é final? ε-closure(q2) = {q2} não contém q3 → NÃO
  - q3 é final? ε-closure(q3) = {q3} contém q3 → SIM
  - **Finais** = {q3}

### Passo 3: Construir novas transições

Para q0:
- ε-closure(q0) = {q0, q2}
- Para símbolo 'a':
  - De q0: q0 --a--> q1, então adicionar ε-closure(q1) = {q1, q2}
  - De q2: nenhuma transição com 'a'
  - **Resultado**: q0 --a--> q1, q0 --a--> q2
- Para símbolo 'b':
  - De q0: nenhuma transição com 'b'
  - De q2: q2 --b--> q3, então adicionar ε-closure(q3) = {q3}
  - **Resultado**: q0 --b--> q3

Para q1:
- ε-closure(q1) = {q1, q2}
- Para símbolo 'b':
  - De q2: q2 --b--> q3
  - **Resultado**: q1 --b--> q3

Para q2:
- ε-closure(q2) = {q2}
- Para símbolo 'b':
  - De q2: q2 --b--> q3
  - **Resultado**: q2 --b--> q3

### Resultado (AFN sem ε)

```
a b
q0 q1 q2 q3
q0 q2
q3
q0 a q1
q0 a q2
q0 b q3
q1 b q3
q2 b q3
```

## Complexidade

- **Tempo**: O(n² · m), onde:
  - n = número de estados
  - m = número de transições

- **Espaço**: O(n²) no pior caso (quando cada estado pode alcançar todos os outros via ε)

## Teorema da Equivalência

**Teorema**: Todo AFN-ε pode ser convertido em um AFN (sem epsilon) que reconhece a mesma linguagem.

**Prova**: A conversão preserva a linguagem porque:
1. Toda palavra aceita pelo AFN-ε também é aceita pelo AFN
2. O AFN não aceita nenhuma palavra adicional
3. As epsilon-transições apenas "encurtam" caminhos, não mudam a linguagem reconhecida

## Uso no Conversor

### Formato de Entrada

```
<alfabeto> ε
<estados>
<iniciais>
<finais>
<transições>
```

**Exemplo:**
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

### Processo de Conversão

1. **Remover Epsilon** (AFN-ε → AFN):
   - Clique em "🔀 AFN-ε → AFN"
   - O AFN resultante aparece na aba "🔀 AFN (sem ε)"

2. **Usar como Input**:
   - Clique em "📥 Usar AFN como Input"
   - O AFN é copiado para a área de entrada

3. **Converter para AFD** (AFN → AFD):
   - Clique em "🔄 AFN → AFD"
   - O AFD aparece na aba "📊 Resultado AFD"

4. **Minimizar** (AFD → MinDFA):
   - Clique em "⚡ Minimizar AFD"
   - O AFD minimizado aparece na aba "⚡ AFD Minimizado"

### Fluxo Completo

```
AFN-ε → AFN → AFD → MinDFA
```

## Propriedades Importantes

1. **Preservação da Linguagem**: A conversão AFN-ε → AFN preserva a linguagem reconhecida
2. **Não-determinismo**: O AFN resultante ainda pode ser não-determinístico
3. **Tamanho**: O AFN pode ter mais transições que o AFN-ε original
4. **Equivalência**: AFN-ε, AFN e AFD têm o mesmo poder expressivo

## Arquivos de Teste

No projeto, você encontra dois arquivos de teste com epsilon-transições:

### `test_epsilon.txt`
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

### `test_epsilon2.txt`
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

## FAQ

### Por que remover epsilon-transições?

Embora epsilon-transições sejam úteis para construir autômatos, elas podem complicar algoritmos de análise e matching. Muitas implementações práticas preferem trabalhar com AFN sem epsilon ou com AFD.

### O resultado é sempre um AFN válido?

Sim! A conversão garante que o AFN resultante:
- Não possui epsilon-transições
- Reconhece a mesma linguagem que o AFN-ε original
- É um autômato finito não-determinístico válido

### Posso ir direto de AFN-ε para AFD?

Tecnicamente sim, mas nosso conversor divide em duas etapas:
1. AFN-ε → AFN (remoção de epsilon)
2. AFN → AFD (subset construction)

Isso facilita a visualização e compreensão do processo!

### Quantas epsilon-transições posso ter?

Não há limite! O algoritmo funciona com qualquer número de epsilon-transições, desde que o autômato seja válido.

## Referências

- **Hopcroft, Motwani, Ullman**: "Introduction to Automata Theory, Languages, and Computation"
- **Sipser**: "Introduction to the Theory of Computation"
- **Aho, Lam, Sethi, Ullman**: "Compilers: Principles, Techniques, and Tools" (Dragon Book)

## Ver Também

- [Algoritmo de Conversão AFN → AFD](algoritmo.md)
- [Minimização de AFD](minimizacao.md)
- [Construção de Thompson](https://en.wikipedia.org/wiki/Thompson%27s_construction)
