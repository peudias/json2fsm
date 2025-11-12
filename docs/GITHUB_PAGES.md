# 🚀 Como Publicar a Documentação no GitHub Pages

Guia rápido para publicar a documentação Docsify no GitHub Pages.

## 📋 Pré-requisitos

- ✅ Repositório no GitHub
- ✅ Arquivos da documentação em `docs/`
- ✅ Permissões de admin no repositório

## 🎯 Passo a Passo

### 1️⃣ Fazer Push dos Arquivos

```bash
# Adicionar arquivos de documentação
git add docs/

# Commit
git commit -m "docs: adiciona documentação completa com Docsify"

# Push para GitHub
git push origin master
```

### 2️⃣ Configurar GitHub Pages

1. Vá para seu repositório no GitHub
2. Clique em **Settings** (⚙️)
3. No menu lateral, clique em **Pages**
4. Em **Source**:
   - Branch: `master` (ou `main`)
   - Folder: `/docs`
5. Clique em **Save**

### 3️⃣ Aguardar Deploy

- GitHub Pages leva ~1-2 minutos para fazer deploy
- Você verá uma mensagem: "Your site is ready to be published"
- Depois: "Your site is published at https://peudias.github.io/json2fsm/"

### 4️⃣ Acessar Documentação

Sua documentação estará disponível em:
```
https://SEU_USUARIO.github.io/json2fsm/
```

## 🔧 Testar Localmente Antes de Publicar

### Opção 1: Com Docsify CLI (Recomendado)

```bash
# Instalar docsify-cli globalmente
npm i docsify-cli -g

# Servir documentação localmente
cd json2fsm
docsify serve docs

# Abrir navegador em: http://localhost:3000
```

### Opção 2: Com Python

```bash
# Usar servidor HTTP do Python
cd docs
python -m http.server 3000

# Abrir navegador em: http://localhost:3000
```

### Opção 3: Com VS Code Live Server

1. Instalar extensão "Live Server"
2. Clicar direito em `docs/index.html`
3. Selecionar "Open with Live Server"

## 🎨 Personalizando URL (Opcional)

Se você tem um domínio customizado:

1. Crie arquivo `docs/CNAME` com seu domínio:
   ```
   docs.seudominio.com
   ```

2. Configure DNS do seu domínio:
   ```
   Type: CNAME
   Name: docs
   Value: peudias.github.io
   ```

## 🔄 Atualizando Documentação

Sempre que fizer mudanças:

```bash
# Editar arquivos em docs/

# Commit e push
git add docs/
git commit -m "docs: atualiza seção X"
git push origin master

# GitHub Pages atualiza automaticamente em ~1 minuto
```

## ✅ Verificar se Funcionou

Teste estes links após deploy:

- ✅ Home: https://peudias.github.io/json2fsm/
- ✅ Instalação: https://peudias.github.io/json2fsm/#/instalacao
- ✅ Uso: https://peudias.github.io/json2fsm/#/uso
- ✅ Testes: https://peudias.github.io/json2fsm/#/testes
- ✅ Algoritmo: https://peudias.github.io/json2fsm/#/algoritmo

## 🐛 Troubleshooting

### Página 404

**Causa:** GitHub Pages ainda não terminou de fazer deploy

**Solução:** Aguarde 2-3 minutos e tente novamente

### Página em branco

**Causa:** Arquivo `.nojekyll` pode estar faltando

**Solução:**
```bash
# Criar arquivo vazio .nojekyll
cd docs
New-Item -ItemType File -Name .nojekyll

git add docs/.nojekyll
git commit -m "docs: adiciona .nojekyll"
git push
```

### Estilos não carregam

**Causa:** Links CDN bloqueados

**Solução:** Verifique se há erro no console do navegador (F12)

### Sidebar não aparece

**Causa:** Arquivo `_sidebar.md` não foi encontrado

**Solução:** Verifique se `docs/_sidebar.md` existe

## 📊 Analytics (Opcional)

Para adicionar Google Analytics:

```html
<!-- No docs/index.html, adicione: -->
<script>
  window.$docsify = {
    // ... configurações existentes
    
    // Google Analytics
    ga: 'UA-XXXXXXXX-X'  // Seu tracking ID
  }
</script>
```

## 🎯 Próximos Passos

Depois de publicar:

1. ✅ Adicione link da documentação no README
2. ✅ Compartilhe nas redes sociais
3. ✅ Adicione ao About do repositório no GitHub
4. ✅ Considere adicionar no LinkedIn/portfólio

## 📝 Checklist Final

Antes de fazer deploy:

- [ ] Todos os arquivos .md estão em `docs/`
- [ ] `index.html` está configurado
- [ ] `_sidebar.md` tem navegação correta
- [ ] Imagens/assets estão em `docs/_media/` (se houver)
- [ ] Links internos funcionam localmente
- [ ] Arquivo `.nojekyll` existe
- [ ] Testou com `docsify serve docs`
- [ ] Fez commit e push
- [ ] Configurou GitHub Pages
- [ ] Aguardou deploy (1-2 min)
- [ ] Testou URL pública

---

**🎉 Pronto!** Sua documentação está online e acessível para todos!
