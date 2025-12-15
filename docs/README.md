# json2fsm — Conversor AFN-ε → AFN → AFD em Pascal

> Conversor completo de Autômatos Finitos: AFN com Epsilon-Transições → AFN → AFD → AFD Minimizado, com visualização gráfica interativa.

![Version](https://img.shields.io/badge/version-3.0.0-blue.svg)
![Pascal](https://img.shields.io/badge/Pascal-FPC%203.2.2-orange.svg)
![Lazarus](https://img.shields.io/badge/Lazarus-4.4-green.svg)
![License](https://img.shields.io/badge/license-Educacional-lightgrey.svg)

## ✨ Principais Recursos

### 🆕 Novidades da Versão 3.1

#### Suporte Completo a JSON
- ✅ **Colar JSON direto** no editor (recomendado)
- ✅ **Carregar arquivos .json** da lista ou botão
- ✅ **JSON preservado** no editor (conversão interna)
- ✅ **Conversão automática** durante processamento
- ✅ Suporta epsilon-transições em JSON

#### Conversão Automática Inteligente
- 🚀 **AFN-ε → AFD em um clique**: Detecta epsilon automaticamente e faz as 2 etapas
- 🛡️ **Validação inteligente**: Bloqueia operações inválidas (ex: remover epsilon de AFN sem ε)
- 🎯 **Navegação automática**: Muda para abas corretas após cada conversão
- ⚡ **Workflow otimizado**: Menos cliques, mais produtividade

### Recursos Principais

- 🎨 **Interface Gráfica Intuitiva** - GUI desenvolvida com Lazarus LCL
- 📊 **Visualização de Diagramas** - Renderização nativa de AFN-ε, AFN, AFD e AFD Minimizado
- 🔀 **Remoção de Epsilon** - Conversão de AFN-ε para AFN (sem epsilon-transições)
- 🔄 **Conversão Automática** - Algoritmo de construção de subconjuntos (AFN → AFD)
- ⚡ **Minimização de AFD** - Redução de estados equivalentes (Myhill-Nerode)
- 📋 **Abas Organizadas** - Resultados separados por tipo (AFN, AFD e AFD Minimizado)
- 📥 **Usar como Input** - Copie o AFN resultante para continuar o processo
- 📝 **Logs Detalhados** - Acompanhe cada etapa no terminal
- 📁 **Casos de Teste** - 11+ exemplos incluídos (TXT e JSON)
- ⚡ **Performance** - Implementação otimizada em Pascal nativo
- 🖼️ **Layout Dividido** - Compare entrada, resultado textual e diagramas simultaneamente

## 🎯 O que você pode fazer?

```mermaid
graph LR
    A[Arquivo AFN-ε] --> B[Interface GUI]
    B --> C[Remoção Epsilon]
    C --> D[AFN sem ε]
    D --> E[Usar como Input]
    E --> F[Conversão AFN→AFD]
    F --> G[Resultado AFD]
    G --> H[Minimização]
    H --> I[AFD Minimizado]
    C --> J[Diagrama AFN-ε]
    D --> K[Diagrama AFN]
    F --> L[Diagrama AFD]
    H --> M[Diagrama MinDFA]
    style B fill:#42b983
    style C fill:#9b59b6
    style F fill:#ff6b6b
    style H fill:#ffd93d
```

1. **Carregar** arquivos `.txt` com AFN ou AFN-ε
2. **Remover epsilon** - Converter AFN-ε → AFN
3. **Usar como Input** - Copiar AFN para entrada
4. **Converter** AFN → AFD com um clique
5. **Minimizar** o AFD gerado (reduzir estados)
6. **Visualizar** graficamente AFN-ε, AFN, AFD e AFD Minimizado
7. **Comparar** em abas separadas
8. **Testar** com 11 casos de teste incluídos
9. **Acompanhar logs** detalhados no terminal

## 🚀 Início Rápido

<!-- tabs:start -->

#### **Windows**

```powershell
# 1. Clone o repositório
git clone https://github.com/peudias/json2fsm.git
cd json2fsm

# 2. Instale o Lazarus IDE (arquivo incluído no repo)
.\download_install_lazarus.ps1

# 3. Compile e execute
# No VS Code: Ctrl+Shift+B → "🎨 GUI: Compilar e Executar"
```

#### **Desenvolvimento**

```powershell
# Abrir no VS Code
code .

# Compilar manualmente
C:\lazarus\lazbuild.exe --build-mode=Release src\afn2afdgui.lpi

# Executar
.\bin\afn2afdgui.exe
```

<!-- tabs:end -->

## 📸 Screenshots

### Interface Principal
![Interface GUI](https://via.placeholder.com/800x450?text=Interface+GUI+AFN2AFD)

### Visualização de Diagramas
![Diagrama AFN](https://via.placeholder.com/400x300?text=Diagrama+AFN) ![Diagrama AFD](https://via.placeholder.com/400x300?text=Diagrama+AFD)

## 🎓 Para quem é este projeto?

- 📚 **Estudantes** de Ciência da Computação aprendendo Teoria da Computação
- 👨‍🏫 **Professores** que querem demonstrar conversão de autômatos visualmente
- 🔬 **Pesquisadores** testando algoritmos de conversão
- 💻 **Desenvolvedores** interessados em Lazarus/Pascal

## 📚 Documentação

Explore a documentação completa:

### 🚀 Começando
- [📦 Instalação](instalacao.md) - Guia completo de instalação do Lazarus 4.4
- [🎮 Como Usar](uso.md) - Tutorial detalhado da interface GUI
- [📂 Estrutura do Projeto](estrutura.md) - Organização de arquivos e pastas
- [⚙️ Tasks do VS Code](tasks.md) - Comandos de compilação e execução

### 🧪 Testes e Teoria
- [🧪 Casos de Teste](testes.md) - Descrição dos 9 testes incluídos
- [🔬 Algoritmo de Conversão](algoritmo.md) - Explicação do algoritmo AFN → AFD
- [⚡ Minimização de AFD](minimizacao.md) - Como funciona a redução de estados

### 💬 Suporte
- [🤝 Contribuindo](contribuindo.md) - Como contribuir com o projeto
- [❓ FAQ](faq.md) - Perguntas frequentes
- [🔧 Troubleshooting](troubleshooting.md) - Solução de problemas

## 🌟 Características Técnicas

| Característica | Detalhe |
|----------------|---------|
| **Linguagem** | Free Pascal (Object Pascal) |
| **IDE** | Lazarus 3.6 |
| **Compilador** | FPC 3.2.2 |
| **GUI Framework** | Lazarus LCL (Lazarus Component Library) |
| **Plataforma** | Windows 64-bit |
| **Algoritmo** | Subset Construction (BFS) |

## 🎯 Próximos Passos

<div class="grid">
  <div>
    <a href="#/instalacao" class="button">📦 Instalar Agora</a>
  </div>
  <div>
    <a href="#/uso" class="button">🎮 Ver Tutorial</a>
  </div>
  <div>
    <a href="#/testes" class="button">🧪 Explorar Testes</a>
  </div>
</div>

---

<p align="center">
  <sub>Desenvolvido com ❤️ por <a href="https://github.com/peudias">Henrique</a></sub>
</p>
