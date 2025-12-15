# 📝 Formato JSON

## Visão Geral

O programa aceita autômatos em formato JSON, oferecendo uma alternativa moderna e estruturada ao formato de texto tradicional.

## Vantagens do JSON

- ✅ **Mais legível** e estruturado
- ✅ **Fácil de gerar** programaticamente
- ✅ **Padrão web** (compatível com APIs)
- ✅ **Validação automática** de sintaxe
- ✅ **Suporte em editores** modernos (VS Code, etc)
- ✅ **Permanece visível** no editor (não é convertido visualmente)

## Formato Completo

### Estrutura Básica

```json
{
  "alfabeto": ["símbolo1", "símbolo2", ...],
  "estados": ["estado1", "estado2", ...],
  "estadosI": ["inicial1", ...],
  "estadosF": ["final1", "final2", ...],
  "transicoes": [
    ["origem", "destino", "símbolo"],
    ...
  ]
}
```

### Campos Obrigatórios

#### `alfabeto` (array de strings)
Lista de símbolos do alfabeto.

```json
"alfabeto": ["a", "b"]
```

**Exemplos:**
- Binário: `["0", "1"]`
- Letras: `["a", "b", "c"]`
- Com epsilon: `["a", "b", "ε"]`
- Múltiplos caracteres: `["ab", "cd"]`

#### `estados` (array de strings)
Lista de todos os estados do autômato.

```json
"estados": ["q0", "q1", "q2"]
```

**Convenções:**
- Nomes: `q0`, `q1`, `q2`, ... (recomendado)
- Também aceita: `s0`, `estado1`, `A`, `B`, etc

#### `estadosI` (array de strings)
Estados iniciais do autômato.

```json
"estadosI": ["q0"]
```

**Notas:**
- AFN pode ter múltiplos: `["q0", "q1"]`
- AFD tem apenas um: `["q0"]`

#### `estadosF` (array de strings)
Estados finais (de aceitação).

```json
"estadosF": ["q2"]
```

**Exemplos:**
- Um final: `["q3"]`
- Múltiplos: `["q2", "q3", "q4"]`
- Nenhum: `[]` (aceita linguagem vazia)

#### `transicoes` (array de arrays)
Lista de transições do autômato.

**Formato:** `[origem, destino, símbolo]`

```json
"transicoes": [
  ["q0", "q1", "a"],
  ["q1", "q2", "b"]
]
```

**Importante:** A ordem é `[origem, destino, símbolo]`, não `[origem, símbolo, destino]`

## Exemplos Completos

### Exemplo 1: AFN Simples

Aceita strings terminadas em "ab":

```json
{
  "alfabeto": ["a", "b"],
  "estados": ["q0", "q1", "q2"],
  "estadosI": ["q0"],
  "estadosF": ["q2"],
  "transicoes": [
    ["q0", "q0", "a"],
    ["q0", "q0", "b"],
    ["q0", "q1", "a"],
    ["q1", "q2", "b"]
  ]
}
```

### Exemplo 2: AFN com Epsilon

Aceita "ab" com transições epsilon:

```json
{
  "alfabeto": ["a", "b", "ε"],
  "estados": ["q0", "q1", "q2", "q3"],
  "estadosI": ["q0"],
  "estadosF": ["q3"],
  "transicoes": [
    ["q0", "q1", "a"],
    ["q1", "q2", "ε"],
    ["q2", "q3", "b"],
    ["q0", "q2", "ε"]
  ]
}
```

### Exemplo 3: Autômato Binário

Aceita números binários pares:

```json
{
  "alfabeto": ["0", "1"],
  "estados": ["par", "impar"],
  "estadosI": ["par"],
  "estadosF": ["par"],
  "transicoes": [
    ["par", "impar", "1"],
    ["par", "par", "0"],
    ["impar", "par", "1"],
    ["impar", "impar", "0"]
  ]
}
```

## Conversão Automática

### Como Funciona

1. **Detecção**: Programa verifica se input começa com `{`
2. **Conversão**: Parser JSON extrai campos
3. **Formato Interno**: Converte para formato TXT temporariamente
4. **Processamento**: Algoritmos trabalham normalmente
5. **Restauração**: JSON original volta ao editor

### Formato TXT Equivalente

O JSON acima é convertido internamente para:

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

**Estrutura TXT:**
- Linha 1: Alfabeto (separado por espaços)
- Linha 2: Estados (separados por espaços)
- Linha 3: Estados iniciais
- Linha 4: Estados finais
- Linhas seguintes: `origem símbolo destino`

**Diferença na ordem:** JSON usa `[origem, destino, símbolo]`, TXT usa `origem símbolo destino`

## Usando JSON no Programa

### Método 1: Colar JSON (Recomendado)

1. Copie o JSON
2. Cole no campo de entrada
3. Clique em "AFN → AFD" ou "AFN-ε → AFN"
4. **JSON permanece no editor!**

### Método 2: Carregar Arquivo

1. Clique "Carregar Arquivo"
2. Selecione arquivo `.json`
3. JSON aparece no editor
4. Processe normalmente

### Método 3: Selecionar da Lista

1. Escolha arquivo `.json` na lista
2. JSON é carregado automaticamente
3. Pronto para processar

## Validação

### Erros Comuns

❌ **Falta vírgula:**
```json
{
  "alfabeto": ["a", "b"]
  "estados": ["q0"]  // Erro: falta vírgula
}
```

✅ **Correto:**
```json
{
  "alfabeto": ["a", "b"],
  "estados": ["q0"]
}
```

❌ **Vírgula extra:**
```json
{
  "alfabeto": ["a", "b",],  // Erro: vírgula extra
}
```

❌ **Aspas simples:**
```json
{
  'alfabeto': ['a']  // Erro: use aspas duplas
}
```

### Mensagens de Erro

Se o JSON for inválido, você verá:

> "Erro ao converter JSON: JSON inválido: ..."

Verifique:
- ✅ Todas as chaves entre aspas duplas
- ✅ Vírgulas entre elementos
- ✅ Nenhuma vírgula sobrando
- ✅ Colchetes e chaves fechados

## Boas Práticas

### Formatação

✅ **Indentação clara:**
```json
{
  "alfabeto": ["a", "b"],
  "estados": ["q0", "q1"]
}
```

❌ **Tudo em uma linha:**
```json
{"alfabeto":["a","b"],"estados":["q0","q1"]}
```

### Nomenclatura

✅ **Estados descritivos:**
```json
"estados": ["inicio", "lendo_a", "final"]
```

✅ **Estados numerados:**
```json
"estados": ["q0", "q1", "q2"]
```

### Comentários

JSON não suporta comentários nativamente, mas você pode:

```json
{
  "_comentario": "Este autômato aceita strings terminadas em ab",
  "alfabeto": ["a", "b"],
  ...
}
```

## Convertendo TXT → JSON

### Manualmente

**TXT:**
```
a b
q0 q1
q0
q1
q0 a q1
```

**JSON equivalente:**
```json
{
  "alfabeto": ["a", "b"],
  "estados": ["q0", "q1"],
  "estadosI": ["q0"],
  "estadosF": ["q1"],
  "transicoes": [
    ["q0", "q1", "a"]
  ]
}
```

**Atenção:** Inverter ordem! TXT é `origem símbolo destino`, JSON é `[origem, destino, símbolo]`

## FAQ

### O JSON é convertido no editor?

**Não!** O JSON permanece visível no formato original. A conversão é feita **internamente** apenas durante o processamento.

### Posso misturar JSON e TXT?

**Não.** Use um formato por vez. O programa detecta automaticamente pelo `{` inicial.

### Posso salvar o resultado em JSON?

Atualmente não. O resultado é sempre em formato TXT. Você pode converter manualmente se necessário.

### Epsilon funciona em JSON?

**Sim!** Use `"ε"` no alfabeto e transições:

```json
{
  "alfabeto": ["a", "ε"],
  "transicoes": [
    ["q0", "q1", "ε"]
  ]
}
```

### Qual formato é melhor?

**Depende:**
- JSON: Mais estruturado, ideal para programação
- TXT: Mais compacto, ideal para digitação rápida

O programa aceita ambos igualmente!

---

**Próximos passos:** [Usar o programa](uso.md) | [Exemplos](testes.md)