# 🎮 Como Usar a Interface

Guia completo da interface gráfica do conversor AFN → AFD.

## 🖼️ Visão Geral da Interface

A janela é dividida em **duas áreas principais**:

```
┌─────────────────────────────────────────────────────────┐
│  [📂 Carregar Arquivo]                                  │
├──────────────────┬──────────────────────────────────────┤
│                  │                                      │
│   📝 Entrada     │     📊 Diagrama AFN                  │
│   (AFN)          │                                      │
│                  │     ━━━━━━━━━━━                     │
│   ━━━━━━━━━━    │                                      │
│                  │     📊 Diagrama AFD                  │
│   [🔄 Converter] │                                      │
│                  │                                      │
│   📄 Resultado   │                                      │
│   (AFD)          │                                      │
│                  │                                      │
└──────────────────┴──────────────────────────────────────┘
```

### 🔹 Painel Esquerdo (550px)
- **Entrada e Resultado Textual**
- Botões de controle
- Editores de texto

### 🔹 Painel Direito (645px)
- **Visualização Gráfica**
- Abas com diagramas
- Renderização nativa

## 📂 Carregando um Arquivo

### Método 1: Botão de Carregar

1. Clique em **"📂 Carregar Arquivo..."**
2. Navegue até a pasta `testes/`
3. Selecione um arquivo (ex: `test_ab.txt`)
4. O conteúdo aparecerá no editor

### Método 2: Edição Manual

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

## 🔄 Convertendo AFN → AFD

1. **Certifique-se** que há um AFN válido no editor de entrada
2. Clique em **"🔄 Converter AFN → AFD"**
3. Aguarde ~1 segundo (depende do tamanho)
4. Veja os resultados:
   - ✅ Resultado textual no painel inferior esquerdo
   - ✅ Diagrama do AFN na aba "Diagrama AFN"
   - ✅ Diagrama do AFD na aba "Diagrama AFD"

## 📊 Visualizando os Diagramas

### Aba "Diagrama AFN"

Mostra o autômato **não-determinístico** original:

**Elementos visuais:**
- 🔵 **Estados**: Círculos com nome
- 🎯 **Estado final**: Círculo duplo amarelo
- ➡️ **Estado inicial**: Seta vinda da esquerda
- 🔀 **Transições**: Setas com rótulos
- 🔁 **Self-loops**: Arcos acima do estado

**Exemplo:**
```
    ┌──a──┐
    ▼     │
→ (q0)────┘
    │
    │ a
    ▼
   (q1)
    │
    │ b
    ▼
  ((q2))  ← Final
```

### Aba "Diagrama AFD"

Mostra o autômato **determinístico** resultante:

**Diferenças do AFN:**
- Estados podem ter nomes compostos: `{q0,q1}`
- Cada estado tem exatamente **uma** transição por símbolo
- Pode haver mais estados que no AFN

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

Clique em **"🗑️ Limpar"** para:
- ✅ Apagar o editor de entrada
- ✅ Limpar o resultado textual
- ✅ Limpar os diagramas

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

1. **Comece com exemplos simples** (`test_ab.txt`)
2. **Compare os diagramas** AFN vs AFD lado a lado
3. **Use o auto-load** - arquivo padrão carrega sozinho
4. **Teste explosão de estados** com `test_nao_det.txt`

### ⚠️ Cuidados

1. **Não feche a janela durante conversão** (pode travar)
2. **Alfabeto com muitos símbolos** gera muitas transições
3. **AFN com muitos estados** pode gerar AFD ENORME

### 🚀 Atalhos

| Atalho | Ação |
|--------|------|
| `Ctrl+O` | Abrir arquivo |
| `Ctrl+S` | Salvar entrada (futuro) |
| `F5` | Converter |
| `Esc` | Limpar |

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
