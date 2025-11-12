# ❓ FAQ - Perguntas Frequentes

Respostas para as dúvidas mais comuns sobre o conversor AFN → AFD.

## 🎯 Uso Geral

### Como faço para começar?

1. Instale o [Lazarus IDE](instalacao.md)
2. Clone o repositório
3. Compile com `Ctrl+Shift+B` no VS Code
4. Execute `.\bin\afn2afdgui.exe`

### Onde encontro exemplos de AFN?

Na pasta `testes/` há 9 exemplos prontos. Comece com `test_ab.txt`.

### Posso usar no Linux ou Mac?

Atualmente apenas Windows é suportado. Contribuições para portar são bem-vindas!

---

## 📝 Formato de Entrada

### Qual o formato do arquivo AFN?

```
<alfabeto: símbolos separados por espaço>
<estados: nomes separados por espaço>
<iniciais: estados iniciais separados por espaço>
<finais: estados finais separados por espaço>
<transições: origem símbolo destino (uma por linha)>
```

### Posso ter múltiplos estados iniciais?

✅ Sim! Basta listar todos na linha 3:
```
a b
q0 q1 q2
q0 q1          ← Dois iniciais
q2
...
```

### Posso ter múltiplos estados finais?

✅ Sim! Liste todos na linha 4:
```
a b
q0 q1 q2
q0
q1 q2          ← Dois finais
...
```

### Nomes de estados podem ter números/letras?

✅ Sim! Use qualquer nome:
```
s0 s1 s2
state_a state_b
q0 q1 q2
```

### Posso usar espaços nos nomes?

❌ Não. Espaços são delimitadores. Use underscores:
```
estado_inicial estado_final  ✅
estado inicial estado final  ❌
```

---

## 🔬 Algoritmo e Teoria

### O que é um AFN?

**Autômato Finito Não-determinístico:** Pode ter múltiplas transições com o mesmo símbolo e múltiplos estados iniciais.

### O que é um AFD?

**Autômato Finito Determinístico:** Para cada estado e símbolo, há exatamente uma transição.

### Por que converter AFN em AFD?

- ✅ AFD é mais fácil de implementar
- ✅ AFD tem execução mais rápida (O(n) vs O(n²))
- ✅ AFD é mais fácil de minimizar
- ✅ Alguns algoritmos só funcionam com AFD

### Qual o tamanho máximo do AFD?

No pior caso: **2^n estados**, onde n = estados do AFN.

Exemplo:
- AFN com 10 estados → até 1024 estados no AFD! 💥

### Suporta epsilon-transições?

❌ Não atualmente. Apenas transições com símbolos do alfabeto.

---

## 🎨 Interface Gráfica

### O diagrama está muito grande!

**Soluções:**
1. Ajuste o splitter para dar mais espaço ao diagrama
2. Minimize o painel esquerdo
3. Simplifique o AFN (menos estados)

### Posso exportar os diagramas?

❌ Não suportado ainda. Use Print Screen por enquanto.

Futuro: Export para PNG, SVG, DOT.

### Posso fazer zoom nos diagramas?

❌ Não atualmente. Tamanho é fixo baseado em quantidade de estados.

Futuro: Zoom/pan com ScrollBox.

### Como salvo o AFD gerado?

Atualmente: Copie o texto do resultado.

Futuro: Botão "Salvar AFD" virá em versão futura.

### A janela não cabe na minha tela!

Arraste os cantos para redimensionar. Tamanho padrão: 1200x600px.

---

## ⚙️ Compilação e Desenvolvimento

### Preciso instalar o Free Pascal separadamente?

❌ Não! O Lazarus IDE já inclui o FPC.

### Posso compilar sem Lazarus?

❌ Não para a versão GUI. A LCL (Lazarus Component Library) é necessária.

### Quanto tempo demora para compilar?

- **Primeira compilação:** ~30 segundos
- **Recompilações:** ~5-10 segundos

### O executável é grande!

Sim, ~15-20 MB. É normal para aplicações Lazarus.

Para reduzir: compile com `--build-mode=Release`.

### Posso usar Delphi ao invés de Lazarus?

⚠️ Possível, mas requer adaptações. LCL != VCL.

---

## 🐛 Troubleshooting

### Erro: "Lazarus não encontrado"

**Causa:** Lazarus não está em `C:\lazarus`

**Solução:** Edite `.vscode\tasks.json` e ajuste o caminho:
```json
"command": "SEU_CAMINHO\\lazbuild.exe"
```

### Erro: "Can't find unit Interfaces"

**Causa:** Tentando compilar sem Lazarus

**Solução:** Instale o Lazarus IDE

### Erro: "SetName already defined"

**Causa:** Versão antiga do código (bug já corrigido)

**Solução:** 
```bash
git pull origin master
```

### GUI compila mas não abre

**Debug:**
```powershell
# Executar manualmente no terminal para ver erros
.\bin\afn2afdgui.exe
```

### Tasks do VS Code não aparecem

**Solução:**
1. Feche e reabra o VS Code
2. Ou: `File` → `Close Folder` → Reabra

---

## 🎓 Aprendizado e Educação

### Para que serve este projeto?

- 📚 Aprender Teoria da Computação
- 👨‍🏫 Ensinar conversão de autômatos
- 🔬 Testar algoritmos
- 💻 Estudar Lazarus/Pascal

### Posso usar em trabalhos acadêmicos?

✅ Sim! Projeto é livre para fins educacionais.

Por favor, cite:
```
Henrique (2025). json2fsm - Conversor AFN → AFD em Pascal.
GitHub: https://github.com/peudias/json2fsm
```

### Posso modificar e redistribuir?

✅ Sim! Licença livre.

Pedimos apenas:
1. Mantenha os créditos originais
2. Compartilhe melhorias (pull requests)

---

## 🚀 Recursos Futuros

### Quais recursos estão planejados?

- [ ] Suporte a epsilon-transições
- [ ] Minimização de AFD
- [ ] Export para DOT/Graphviz
- [ ] Zoom/pan nos diagramas
- [ ] Simulação de entrada
- [ ] Regex → AFN
- [ ] Versão web (WASM)

### Como posso contribuir?

Veja [Como Contribuir](contribuindo.md)

### Quando sai a próxima versão?

Sem prazo definido. Projeto é mantido voluntariamente.

---

## 💻 Desenvolvimento

### Qual versão do Pascal é usada?

**Object Pascal** (Free Pascal 3.2.2)

### Posso usar bibliotecas externas?

✅ Sim, mas prefira manter dependências mínimas.

### Como reporto bugs?

[GitHub Issues](https://github.com/peudias/json2fsm/issues)

### Como submeto melhorias?

[GitHub Pull Requests](https://github.com/peudias/json2fsm/pulls)

---

## 📊 Performance

### Quantos estados o programa aguenta?

**Testado até:**
- AFN: 20 estados
- AFD: 100 estados

**Teoricamente:**
- Limitado por memória RAM disponível

### Quanto tempo demora uma conversão?

| AFN States | Tempo Médio |
|------------|-------------|
| 1-5 | <100ms |
| 5-10 | 100-500ms |
| 10-20 | 0.5-2s |
| 20+ | 2s+ |

### Posso processar arquivos grandes?

⚠️ Não recomendado. GUI é para fins educacionais/demonstração.

Para processamento em lote, use a versão console (futura).

---

## 🌐 Web e Cloud

### Haverá versão web?

🔮 Planejado! Usando WebAssembly + Free Pascal.

### Posso usar via API REST?

❌ Não atualmente. Apenas desktop.

Futuro: Servidor HTTP com endpoints /convert.

---

## 📱 Mobile

### Funciona em Android/iOS?

❌ Não. Apenas Windows desktop.

Lazarus tem suporte Android, mas não implementado ainda.

---

## 🎯 Mais Perguntas?

**Não encontrou sua resposta?**

1. Veja o [Troubleshooting](troubleshooting.md)
2. Abra uma [Issue no GitHub](https://github.com/peudias/json2fsm/issues)
3. Entre em contato: [peudias](https://github.com/peudias)

---

**💡 Dica:** Esta FAQ é atualizada regularmente. Marque nos favoritos!
