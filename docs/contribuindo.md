# 🤝 Como Contribuir

Adoramos receber contribuições! Este guia te mostrará como contribuir para o projeto.

## 🎯 Formas de Contribuir

Você pode contribuir de várias maneiras:

- 🐛 **Reportar bugs**
- 💡 **Sugerir novos recursos**
- 📝 **Melhorar documentação**
- 💻 **Enviar código**
- 🧪 **Adicionar testes**
- 🎨 **Melhorar UI/UX**
- 🌐 **Traduzir para outros idiomas**

## 🐛 Reportando Bugs

### Antes de Reportar

1. ✅ Verifique se já não foi reportado: [Issues](https://github.com/peudias/json2fsm/issues)
2. ✅ Teste com a versão mais recente
3. ✅ Colete informações relevantes

### Como Reportar

Use nosso [template de bug report](https://github.com/peudias/json2fsm/issues/new?template=bug_report.md):

```markdown
**Descrição do Bug:**
Uma descrição clara do que aconteceu.

**Passos para Reproduzir:**
1. Vá para '...'
2. Clique em '...'
3. Veja o erro

**Comportamento Esperado:**
O que deveria acontecer.

**Screenshots:**
Se aplicável, adicione capturas de tela.

**Ambiente:**
- OS: Windows 11
- Lazarus: 3.6
- FPC: 3.2.2
```

## 💡 Sugerindo Recursos

Use nosso [template de feature request](https://github.com/peudias/json2fsm/issues/new?template=feature_request.md):

```markdown
**Descrição do Recurso:**
Uma descrição clara do recurso desejado.

**Problema que Resolve:**
Qual problema este recurso resolve?

**Alternativas Consideradas:**
Outras soluções que você considerou.

**Contexto Adicional:**
Qualquer outra informação relevante.
```

## 💻 Contribuindo com Código

### Configurando o Ambiente

1. **Fork o repositório**
   ```bash
   # No GitHub: clique em "Fork"
   ```

2. **Clone seu fork**
   ```bash
   git clone https://github.com/SEU_USUARIO/json2fsm.git
   cd json2fsm
   ```

3. **Adicione o upstream**
   ```bash
   git remote add upstream https://github.com/peudias/json2fsm.git
   ```

4. **Instale o Lazarus**
   - Baixe: [https://www.lazarus-ide.org/](https://www.lazarus-ide.org/)
   - Instale em `C:\lazarus`

5. **Compile o projeto**
   ```bash
   C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi
   ```

### Fluxo de Trabalho

1. **Crie uma branch**
   ```bash
   git checkout -b feature/minha-feature
   # ou
   git checkout -b fix/meu-bugfix
   ```

2. **Faça suas alterações**
   - Siga o [guia de estilo](#-guia-de-estilo)
   - Adicione testes se aplicável
   - Atualize documentação

3. **Commit suas mudanças**
   ```bash
   git add .
   git commit -m "feat: adiciona suporte a epsilon-transições"
   ```

4. **Mantenha atualizado**
   ```bash
   git fetch upstream
   git rebase upstream/master
   ```

5. **Push para seu fork**
   ```bash
   git push origin feature/minha-feature
   ```

6. **Abra um Pull Request**
   - Vá para o GitHub
   - Clique em "New Pull Request"
   - Preencha o template

### Convenção de Commits

Usamos [Conventional Commits](https://www.conventionalcommits.org/):

```bash
feat: adiciona novo recurso
fix: corrige bug
docs: atualiza documentação
style: formatação de código
refactor: refatoração sem mudar funcionalidade
test: adiciona ou corrige testes
chore: tarefas de manutenção
```

**Exemplos:**
```bash
feat: adiciona suporte a epsilon-transições
fix: corrige bug na função Move()
docs: atualiza README com novos exemplos
refactor: simplifica algoritmo de conversão
test: adiciona testes para estados compostos
```

## 📝 Guia de Estilo

### Pascal/Object Pascal

```pascal
// ✅ BOM
procedure CalcularProximoEstado(const Estado: string; Simbolo: char);
var
  Resultado: TStringSet;
  i: Integer;
begin
  // Comentário claro
  Resultado := TStringSet.Create;
  try
    for i := 0 to Lista.Count - 1 do
    begin
      // Código indentado
      ProcessarItem(i);
    end;
  finally
    Resultado.Free;
  end;
end;

// ❌ RUIM
procedure calc(e:string;s:char);
var r:TStringSet;i:Integer;
begin
r:=TStringSet.Create;
for i:=0 to Lista.Count-1 do ProcessarItem(i);
r.Free;
end;
```

**Convenções:**
- ✅ PascalCase para identificadores
- ✅ Comentários em português
- ✅ Indentação: 2 espaços
- ✅ Use `try..finally` para gerenciamento de memória
- ✅ Nomes descritivos

### Markdown

```markdown
<!-- ✅ BOM -->
# Título Principal

## Subtítulo

Parágrafo com **negrito** e *itálico*.

- Lista
- Com itens
- Claros

```pascal
// Código com syntax highlighting
```

<!-- ❌ RUIM -->
# titulo
parágrafo sem espaçamento
-lista
-sem espaços
```

## 🧪 Testes

### Adicionando Testes

1. **Crie arquivo de teste** em `testes/`
   ```
   testes/
     test_seu_caso.txt
   ```

2. **Formato:**
   ```
   a b
   q0 q1 q2
   q0
   q2
   q0 a q1
   ...
   ```

3. **Documente** em `docs/testes.md`

### Executando Testes

```powershell
# Testar manualmente
.\bin\afn2afdgui.exe

# Carregar cada arquivo de testes/
# Verificar se conversão está correta
```

## 📚 Documentação

### Atualizando Docs

Documentação fica em `docs/`:
```
docs/
  README.md          # Home
  instalacao.md      # Guia de instalação
  uso.md             # Como usar
  algoritmo.md       # Explicação do algoritmo
  testes.md          # Casos de teste
  faq.md             # FAQ
  contribuindo.md    # Este arquivo
```

### Docsify

Testamos localmente:
```bash
# Instalar docsify-cli
npm i docsify-cli -g

# Servir localmente
docsify serve docs

# Abrir: http://localhost:3000
```

## 🔍 Code Review

Seu PR será revisado quanto a:

- ✅ **Funcionalidade:** Código funciona conforme esperado?
- ✅ **Qualidade:** Código é limpo e legível?
- ✅ **Testes:** Mudanças estão testadas?
- ✅ **Documentação:** Docs estão atualizadas?
- ✅ **Performance:** Não degrada performance?
- ✅ **Compatibilidade:** Não quebra código existente?

## 🎯 Áreas que Precisam de Ajuda

### 🔴 Alta Prioridade

- [ ] Suporte a epsilon-transições
- [ ] Minimização de AFD
- [ ] Export para DOT/Graphviz
- [ ] Testes automatizados

### 🟡 Média Prioridade

- [ ] Zoom/pan nos diagramas
- [ ] Simulação de entrada
- [ ] Undo/Redo
- [ ] Salvar/Carregar AFD

### 🟢 Baixa Prioridade

- [ ] Temas da interface
- [ ] Atalhos de teclado customizáveis
- [ ] Regex → AFN
- [ ] Versão web

## 💬 Comunicação

### Issues

- Use [GitHub Issues](https://github.com/peudias/json2fsm/issues)
- Seja claro e objetivo
- Adicione labels apropriadas

### Pull Requests

- Descreva o que foi mudado
- Referencie issues relacionadas: `Closes #123`
- Inclua screenshots se for UI

### Discussões

- Use [GitHub Discussions](https://github.com/peudias/json2fsm/discussions)
- Para ideias e perguntas gerais

## 📜 Licença

Ao contribuir, você concorda que suas contribuições serão licenciadas sob a mesma licença do projeto (Licença Livre para fins educacionais).

## 🙏 Reconhecimento

Todos os contribuidores são listados em:
- `README.md` principal
- Release notes
- Seção "Contribuidores" no site

## 🎉 Primeiros Passos

**Primeira vez contribuindo?** Comece aqui:

1. 📝 Melhore documentação (typos, clareza)
2. 🐛 Resolva issues marcadas como `good first issue`
3. 🧪 Adicione casos de teste
4. 🎨 Melhore mensagens de erro

## ❓ Dúvidas?

- 📧 Abra uma issue
- 💬 Use GitHub Discussions
- 👤 Entre em contato: [peudias](https://github.com/peudias)

---

**Obrigado por contribuir!** 🎉

Cada contribuição, por menor que seja, faz diferença! ❤️
