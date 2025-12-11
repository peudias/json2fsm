# 🎮 Como Usar a Interface

Guia completo da interface gráfica do conversor AFN → AFD.

## 🖼️ Visão Geral da Interface

A janela é dividida em **duas áreas principais**:

```
┌─────────────────────────────────────────────────────────┐
│  [📂 Abrir] [▼ ComboBox Testes]                        │
├──────────────────┬──────────────────────────────────────┤
│                  │                 [🗑️ Limpar]         │
│   📝 Entrada     │     📊 AFN-ε                         │
│   (AFN-ε)        │     🔀 AFN (sem ε)                   │
│                  │     📊 AFD        (Abas)             │
│   ━━━━━━━━━━    │     ⚡ MinDFA                        │
│                  │                                      │
│ [🔀→AFN][AFN→AFD][⚡Min] (Botões em linha)            │
│                  │                                      │
│   📄 Resultado   │                                      │
│   [AFN|AFD|Min]  │                                      │
│   (Abas)         │                                      │
│        [📥 Input]│                                      │
└──────────────────┴──────────────────────────────────────┘
```

### 🔹 Painel Esquerdo (550px)
- **Entrada**: Editor para AFN-ε
- **Botões**: 3 botões de conversão em linha horizontal
- **Resultado**: 3 abas (AFN sem ε, AFD, MinDFA)
- **Botão especial**: "📥 Usar como Input" (canto inferior direito da aba AFN)

### 🔹 Painel Direito (645px)
- **Visualização Gráfica**: 4 abas de diagramas
- **Botão Limpar**: Canto superior direito de cada aba
- **Renderização nativa**: Desenho direto no canvas

## 📂 Carregando um Arquivo

### Método 1: ComboBox de Testes (⚡ Novo!)

1. Na barra superior, ao lado do campo de arquivo, há um **ComboBox**
2. Clique na seta para ver todos os arquivos de teste disponíveis
3. Selecione um arquivo (ex: `test_ab.txt`)
4. O conteúdo é carregado **automaticamente**!

**Vantagens:**
- 🚀 Acesso rápido aos 9 testes
- 📝 Lista dinâmica da pasta `testes/`
- ⚡ Carregamento instantâneo

### Método 2: Botão de Carregar

1. Clique em **"📂 Abrir"**
2. Navegue até a pasta `testes/`
3. Selecione um arquivo (ex: `test_ab.txt`)
4. O conteúdo aparecerá no editor

### Método 3: Edição Manual

Você pode digitar ou colar diretamente no editor de entrada.

**Formato do arquivo AFN:**

```
a b                  # Linha 1: Alfabeto (símbolos separados por espaço)
q0 q1 q2            # Linha 2: Estados (nomes separados por espaço)
q0                  # Linha 3: Estados iniciais (pode ter vários!)
q2                  # Linha 4: Estados finais (pode ter vários!)
q0 a q0             # Linhas 5+: Transições (origem símbolo destino)
q0 b q0
q0 a q1             # ← Não-determinismo: q0 com 'a' vai para q0 E q1
q1 b q2
```

## � Removendo Epsilon-Transições (AFN-ε → AFN)

### O que são Epsilon-Transições?

Epsilon-transições (ε) permitem mudanças de estado **sem consumir símbolos** da entrada. São úteis para construir autômatos, mas precisam ser removidas antes da conversão para AFD.

### Como Remover

1. **Carregue um AFN-ε** (ex: `test_epsilon.txt`)
2. O alfabeto deve incluir: `ε`, `epsilon`, `e` ou `&`
3. Clique em **"🔀 AFN-ε → AFN"**
4. O resultado aparece na aba **"🔀 AFN (sem ε)"**
5. Use o botão **"📥 Usar como Input"** para copiar o AFN resultante

### Exemplo

**Entrada (AFN-ε):**
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

**Resultado (AFN sem ε):**
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

### Botão "📥 Usar como Input"

Após remover epsilon:
1. O botão aparece no **canto inferior direito** da aba "AFN (sem ε)"
2. Clique para **copiar o AFN para a entrada**
3. Agora você pode converter AFN → AFD normalmente

## �🔄 Convertendo AFN → AFD

### Passo a Passo

1. **Certifique-se** que há um AFN **sem epsilon** no editor de entrada
   - ⚠️ Se houver epsilon-transições, remova-as primeiro com "🔀 AFN-ε → AFN"
2. Clique em **"🔄 AFN → AFD"**
3. **Validação automática**: Se epsilon for detectado, você receberá um aviso
4. Aguarde ~1 segundo (depende do tamanho)
5. A aba **"📊 Resultado AFD"** é ativada automaticamente
6. Veja os resultados:
   - ✅ Resultado textual na aba "Resultado AFD"
   - ✅ Diagrama do AFN-ε na aba "📊 AFN-ε" (se aplicável)
   - ✅ Diagrama do AFN na aba "🔀 AFN (sem ε)"
   - ✅ Diagrama do AFD na aba "📊 Diagrama AFD"
   - ✅ Botão **"⚡ Minimizar AFD"** é habilitado

### ⚠️ Importante: Epsilon-Transições

Se você tentar converter um AFN-ε diretamente:
- ❌ **Não recomendado**: O programa irá alertá-lo
- ✅ **Correto**: Primeiro remova epsilon (🔀 AFN-ε → AFN), depois converta (🔄 AFN → AFD)

### Logs no Terminal

Se executar via `Ctrl+Shift+B`, você verá logs detalhados:

```
-------------------------------------------
AFN DE ENTRADA:
  Alfabeto: a,b
  Estados: q0,q1,q2
  Iniciais: q0
  Finais: q2
  Transicoes: 4 transicoes
    q0 --a--> q0
    q0 --b--> q0
    q0 --a--> q1
    q1 --b--> q2
-------------------------------------------

-------------------------------------------
AFD RESULTANTE:
  Estados: 3 estados
    {q0}
    {q0,q1}
    {q0,q2}
  Estado inicial: {q0}
  Estados finais: 1
    {q0,q2}
  Transicoes: 6 transicoes
-------------------------------------------
```

## ⚡ Minimizando o AFD

### O que é Minimização?

Minimização **reduz o número de estados** do AFD sem alterar a linguagem reconhecida:

- Estados **equivalentes** (mesmo comportamento) são **mesclados**
- Resultado: AFD com **menor número possível de estados**
- Algoritmo: Particionação iterativa (Hopcroft/Myhill-Nerode)

### Como Minimizar

1. **Primeiro converta** AFN → AFD
2. Clique em **"⚡ Minimizar AFD"**
3. Aguarde o processamento
4. A aba **"⚡ AFD Minimizado"** é ativada automaticamente
5. Veja os resultados:
   - ✅ Resultado completo na aba "AFD Minimizado"
   - ✅ Resumo na aba "Resultado AFD"
   - ✅ Diagrama na aba "⚡ AFD Minimizado"
   - 📊 **Redução**: X → Y estados

### Exemplo de Minimização

**Antes (AFD com 5 estados):**
```
Estados: q0, q1, q2, q3, q4
q0 --a--> q1
q0 --b--> q2
q1 --a--> q3
q1 --b--> q4
q2 --a--> q3
q2 --b--> q4
```

**Depois (AFD minimizado com 3 estados):**
```
Estados: [q0], [q1,q2], [q3,q4]
[q0] --a--> [q1,q2]
[q0] --b--> [q1,q2]
[q1,q2] --a--> [q3,q4]
[q1,q2] --b--> [q3,q4]
```

📊 **Redução: 5 → 3 estados (40% de redução!)**

### Logs de Minimização

```
-------------------------------------------
INICIANDO MINIMIZACAO DO AFD:
  Estados do AFD: 5
  Estados finais: 2
  Transicoes: 10
-------------------------------------------

[GUI] Particao inicial: 2 grupos
[GUI] Particoes finais: 3 grupos
[GUI] Criando estados minimizados...

-------------------------------------------
AFD MINIMIZADO:
  Estados: 3 estados
    [q0]
    [q1,q2]
    [q3,q4]
  Estado inicial: [q0]
  Estados finais: 1
    [q3,q4]
  Transicoes: 6 transicoes
-------------------------------------------
  REDUCAO: 5 -> 3 estados
-------------------------------------------
```

## 📊 Visualizando os Diagramas

### Aba "Diagrama AFN"

Mostra o autômato **não-determinístico** original:

**Elementos visuais:**
- 🔵 **Estados**: Círculos com nome
- 🎯 **Estado final**: Círculo duplo amarelo
- ➡️ **Estado inicial**: Seta vinda da esquerda
- 🔀 **Transições**: Setas com rótulos
- 🔁 **Self-loops**: Arcos acima do estado

### Aba "Diagrama AFD"

Mostra o autômato **determinístico** resultante:

**Diferenças do AFN:**
- Estados podem ter nomes compostos: `{q0,q1}`
- Cada estado tem exatamente **uma** transição por símbolo
- Pode haver mais estados que no AFN (explosão de estados)

### Aba "⚡ AFD Minimizado" (✨ Novo!)

Mostra o autômato **minimizado** (reduzido):

**Características:**
- Estados são mesclados: `[q1,q2,q3]`
- **Menor número possível** de estados
- Equivalente ao AFD original
- Layout otimizado para menos estados

## 📄 Lendo o Resultado Textual

O resultado textual mostra o AFD formatado:

```
📋 Alfabeto: a, b

🔵 Estados do DFA:
   • {q0}
   • {q0,q1}
   • {q0,q2}

▶️ Estado inicial: {q0}

🎯 Estados finais:
   • {q0,q2}

➡️ Transições:
   {q0} --a--> {q0,q1}
   {q0} --b--> {q0}
   {q0,q1} --a--> {q0,q1}
   {q0,q1} --b--> {q0,q2}
   {q0,q2} --a--> {q0,q1}
   {q0,q2} --b--> {q0}
```

## 🗑️ Limpando a Interface

### Localização dos Botões Limpar

**Botões nos Diagramas** (lado direito):
- Cada aba de diagrama tem seu próprio botão **"🗑️ Limpar"** no canto superior direito
- Clique para limpar **todos os dados** da aplicação

### O que é limpo:
- ✅ Editor de entrada
- ✅ Todas as 3 abas de resultado (AFN sem ε, AFD, MinDFA)
- ✅ Todos os 4 diagramas (AFN-ε, AFN, AFD, MinDFA)
- ✅ Desabilita botões "Minimizar AFD" e "Usar como Input"

## 💁 Navegando pelas Abas

### Abas de Resultado (Esquerda)

- **� AFN (sem ε)** - Após remoção de epsilon
- **📊 Resultado AFD** - Após conversão AFN → AFD
- **⚡ AFD Minimizado** - Após minimização

A aplicação **muda automaticamente** para a aba relevante:
- Removeu epsilon? → Vai para "AFN (sem ε)"
- Converteu? → Vai para "Resultado AFD"
- Minimizou? → Vai para "AFD Minimizado"

### Abas de Diagrama (Direita)

- **📊 AFN-ε** - Mostra o AFN com epsilon-transições (se houver)
- **🔀 AFN (sem ε)** - Mostra o AFN após remoção de epsilon
- **📊 Diagrama AFD** - Mostra o AFD convertido
- **⚡ AFD Minimizado** - Mostra o AFD minimizado

Você pode **alternar livremente** entre as 4 abas para comparar.

## 🎨 Ajustando o Layout

### Redimensionar Painéis

Arraste o **splitter vertical** (barra entre os painéis) para:
- ⬅️ Priorizar visualização de diagramas
- ➡️ Priorizar leitura de texto

### Alternar entre Diagramas

Use as **abas superiores** no painel direito:
- 📊 **Diagrama AFN** - Autômato original
- 📊 **Diagrama AFD** - Autômato convertido

## 💡 Dicas de Uso

### ✅ Boas Práticas

1. **Comece com exemplos simples** (`test_ab.txt`, `test_par_a.txt`)
2. **Use o ComboBox** para acesso rápido aos testes
3. **Compare os diagramas** AFN vs AFD vs MinDFA lado a lado
4. **Minimize sempre** - veja a redução de estados!
5. **Teste explosão de estados** com `test_nao_det.txt` e `test_explosao.txt`
6. **Acompanhe os logs** via `Ctrl+Shift+B` para entender o algoritmo
7. **Alterne entre abas** para comparar resultados

### ⚠️ Cuidados

1. **Não feche a janela durante conversão/minimização**
2. **Alfabeto com muitos símbolos** gera muitas transições
3. **AFN com muitos estados** pode gerar AFD ENORME
4. **Minimização só funciona após conversão** AFN → AFD

### 🚀 Atalhos e Recursos

| Recurso | Como usar |
|---------|----------|
| Remover Epsilon | Botão "🔀 AFN-ε → AFN" (primeiro botão) |
| Usar como Input | Botão "📥 Usar como Input" (canto inferior direito, aba AFN) |
| Converter | Botão "🔄 AFN → AFD" (meio) |
| Minimizar | Botão "⚡ Minimizar AFD" (direita, após conversão) |
| Limpar | Botão "🗑️ Limpar" (canto superior direito dos diagramas) |
| ComboBox de testes | Dropdown ao lado do campo de arquivo |
| Logs detalhados | `Ctrl+Shift+B` para executar com terminal |
| Abas de resultado | 3 abas (AFN sem ε, AFD, MinDFA) |
| Abas de diagrama | 4 abas (AFN-ε, AFN, AFD, MinDFA) |

## 🧪 Testando com Exemplos

### Exemplo 1: AFN Simples

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

**O que faz:** Aceita strings que terminam em "ab"

### Exemplo 2: Explosão de Estados

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

**O que faz:** Aceita strings que contêm "aaa"
**Resultado:** AFN com 4 estados → AFD com **8 estados**! 💥

## 📐 Características dos Diagramas

### Parâmetros Visuais

| Elemento | Tamanho |
|----------|---------|
| **Raio do estado** | 20px |
| **Espaçamento horizontal** | 100px |
| **Espaçamento vertical** | 80px |
| **Margem esquerda** | 60px |
| **Margem superior** | 40px |
| **Fonte** | 8pt, negrito |
| **Espessura da linha** | 1px |

### Layout Inteligente

- ✅ Máximo de **4 colunas** por linha
- ✅ Quebra automática de linha
- ✅ Alinhamento top-left (não centralizado)
- ✅ Self-loops desenhados acima do estado

## 🎯 Casos de Uso

### 👨‍🎓 Para Estudantes

1. Carregue `test_par_a.txt`
2. Observe o AFN no diagrama
3. Converta para AFD
4. Compare os dois diagramas
5. Entenda como o algoritmo funciona

### 👨‍🏫 Para Professores

1. Crie AFNs customizados
2. Demonstre não-determinismo
3. Mostre explosão de estados
4. Use em aulas práticas

### 🔬 Para Pesquisadores

1. Teste algoritmos complexos
2. Compare complexidades
3. Valide casos extremos

## ❓ Perguntas Frequentes

**P: O diagrama está muito grande, não cabe na tela!**
> R: Ajuste o splitter ou minimize o painel esquerdo. Considere simplificar o AFN.

**P: Posso exportar os diagramas como imagem?**
> R: Não suportado ainda. Use Print Screen por enquanto.

**P: Como salvar o AFD gerado?**
> R: Copie o texto do resultado. Salvamento em arquivo virá em versão futura.

**P: Posso criar AFN com epsilon-transições?**
> R: Não suportado atualmente. Apenas transições com símbolos do alfabeto.

## 🎓 Próximos Passos

Agora que você domina a interface:

- 🧪 [Explore os Casos de Teste](testes.md)
- 🔬 [Entenda o Algoritmo](algoritmo.md)
- 💻 [Veja a Arquitetura do Código](arquitetura.md)

---

**💡 Dica:** Pratique com os 9 casos de teste incluídos para dominar a ferramenta!
