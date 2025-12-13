unit MainForm;

{******************************************************************************
 * Unit: MainForm
 * Descrição: Formulário principal do conversor de autômatos finitos
 * 
 * Este programa implementa conversões completas entre autômatos:
 *   AFN-ε → AFN → AFD → MinDFA
 * 
 * Algoritmos implementados:
 *   1. Remoção de epsilon-transições (ε-closure)
 *   2. Conversão AFN→AFD (construção de subconjuntos)
 *   3. Minimização de AFD (algoritmo de Myhill-Nerode)
 * 
 * Autor: Henrique Freitas
 * Data: Dezembro 2025
 ******************************************************************************}

{$mode objfpc}{$H+}  // Modo Object Pascal com strings longas ativadas

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Math, ComCtrls;

type
  {****************************************************************************
   * TTransition - Record para representar uma transição de autômato
   * 
   * Campos:
   *   - FromState: Estado de origem
   *   - Symbol: Símbolo que dispara a transição (pode ser ε, epsilon, e, &)
   *   - ToState: Estado de destino
   * 
   * Exemplo: q0 --a--> q1 seria representado como:
   *   FromState = 'q0', Symbol = 'a', ToState = 'q1'
   ****************************************************************************}
  TTransition = record
    FromState, Symbol, ToState: string;
  end;
  
  {****************************************************************************
   * TTransitionArray - Array dinâmico de transições
   * 
   * Usado para armazenar todas as transições de um autômato.
   * O tamanho do array é ajustado dinamicamente com SetLength().
   ****************************************************************************}
  TTransitionArray = array of TTransition;
  
  {****************************************************************************
   * TFormMain - Classe do formulário principal da aplicação
   * 
   * Esta classe gerencia toda a interface gráfica e a lógica de conversão
   * de autômatos. Contém:
   * 
   * COMPONENTES VISUAIS:
   *   - Botões de conversão (Remover Epsilon, Converter AFN→AFD, Minimizar)
   *   - Áreas de texto para entrada/saída
   *   - 4 abas de diagramas (AFN-ε, AFN, AFD, MinDFA)
   *   - PaintBoxes para desenhar diagramas
   * 
   * ESTRUTURAS DE DADOS PRIVADAS:
   *   - EpsilonNFAXXX: Dados do AFN com epsilon-transições (entrada original)
   *   - NFAXXX: Dados do AFN sem epsilon (após remoção de ε)
   *   - DFAXXX: Dados do AFD (após conversão)
   *   - MinDFAXXX: Dados do AFD minimizado (após minimização)
   * 
   * MÉTODOS PRINCIPAIS:
   *   - RemoveEpsilonTransitions: Remove ε-transições do AFN-ε
   *   - ConvertAFNtoAFD: Converte AFN em AFD (subset construction)
   *   - MinimizeDFA: Minimiza o AFD (Myhill-Nerode)
   *   - DrawAutomaton: Desenha o diagrama de um autômato
   ****************************************************************************}
  TFormMain = class(TForm)
    btnConvert: TButton;
    btnLoadFile: TButton;
    btnClear: TButton;
    btnMinimize: TButton;
    btnRemoveEpsilon: TButton;
    btnUseAsInput: TButton;
    edtFilePath: TEdit;
    lblInput: TLabel;
    lblFile: TLabel;
    memoInput: TMemo;
    memoOutput: TMemo;
    memoMinOutput: TMemo;
    memoNFAOutput: TMemo;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Panel3: TPanel;
    PageControl2: TPageControl;
    TabOutput: TTabSheet;
    TabMinOutput: TTabSheet;
    TabNFAOutput: TTabSheet;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    TabEpsilonNFA: TTabSheet;
    TabNFA: TTabSheet;
    TabDFA: TTabSheet;
    TabMinDFA: TTabSheet;
    PaintBoxEpsilonNFA: TPaintBox;
    PaintBoxNFA: TPaintBox;
    PaintBoxDFA: TPaintBox;
    PaintBoxMinDFA: TPaintBox;
    procedure btnClearClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnRemoveEpsilonClick(Sender: TObject);
    procedure btnUseAsInputClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxEpsilonNFAPaint(Sender: TObject);
    procedure PaintBoxNFAPaint(Sender: TObject);
    procedure PaintBoxDFAPaint(Sender: TObject);
    procedure PaintBoxMinDFAPaint(Sender: TObject);
  private
    {****************************************************************************
     * ESTRUTURAS DE DADOS DOS AUTÔMATOS
     * 
     * Cada autômato é representado por 5 componentes:
     * 1. Alphabet (TStringList): Símbolos do alfabeto (ex: ['a', 'b'])
     * 2. States (TStringList): Todos os estados (ex: ['q0', 'q1', 'q2'])
     * 3. Initials (TStringList): Estados iniciais (pode ser múltiplos no AFN)
     * 4. Finals (TStringList): Estados finais/de aceitação
     * 5. Transitions (TTransitionArray): Todas as transições do autômato
     * 
     * IMPORTANTE: Initial pode ser string (AFD) ou TStringList (AFN) pois:
     * - AFN pode ter múltiplos estados iniciais
     * - AFD tem exatamente UM estado inicial
     ****************************************************************************}
    
    // Dados do AFN-ε (Autômato com epsilon-transições - entrada original)
    EpsilonNFAAlphabet, EpsilonNFAStates, EpsilonNFAInitials, EpsilonNFAFinals: TStringList;
    EpsilonNFATransitions: TTransitionArray;
    
    // Dados do AFN (Autômato sem epsilon - resultado da remoção de ε)
    NFAAlphabet, NFAStates, NFAInitials, NFAFinals: TStringList;
    NFATransitions: TTransitionArray;
    
    // Dados do AFD (Autômato Determinístico - resultado da conversão)
    DFAStates: TStringList;
    DFATransitions: TTransitionArray;
    DFAFinals: TStringList;
    DFAInitial: string;  // AFD tem apenas UM estado inicial
    
    // Dados do AFD Minimizado (resultado final após minimização)
    MinDFAStates: TStringList;
    MinDFATransitions: TTransitionArray;
    MinDFAFinals: TStringList;
    MinDFAInitial: string;  // AFD minimizado também tem apenas UM estado inicial
    
    // ComboBox para selecionar arquivos de teste do diretório testes/
    cmbTestFiles: TComboBox;
    
    {****************************************************************************
     * MÉTODOS PRINCIPAIS DE CONVERSÃO
     ****************************************************************************}
    
    {**
     * RemoveEpsilonTransitions - Remove epsilon-transições de um AFN-ε
     * 
     * Algoritmo:
     * 1. Para cada estado, calcula seu epsilon-closure (estados alcançáveis por ε)
     * 2. Para cada transição normal (não-epsilon), expande para incluir ε-closure
     * 3. Ajusta estados finais: um estado é final se seu ε-closure contém algum final
     * 
     * Complexidade: O(n² · m) onde n = número de estados, m = número de transições
     * 
     * Entrada: AFN-ε armazenado em EpsilonNFAXXX
     * Saída: AFN sem epsilon armazenado em NFAXXX
     *}
    procedure RemoveEpsilonTransitions;
    
    {**
     * ConvertAFNtoAFD - Converte AFN em AFD usando construção de subconjuntos
     * 
     * Algoritmo (Subset Construction):
     * 1. Estado inicial do AFD = conjunto dos estados iniciais do AFN
     * 2. Para cada estado do AFD (que é um conjunto de estados do AFN):
     *    - Para cada símbolo do alfabeto:
     *      - Calcula o conjunto de estados alcançáveis
     *      - Cria novo estado do AFD se necessário
     *      - Adiciona transição
     * 3. Um estado do AFD é final se contém algum estado final do AFN
     * 
     * Complexidade: O(2^n · |Σ|) no pior caso (explosão exponencial)
     * 
     * Entrada: AFN armazenado em NFAXXX
     * Saída: AFD armazenado em DFAXXX
     *}
    procedure ConvertAFNtoAFD;
    
    {**
     * MinimizeDFA - Minimiza um AFD usando algoritmo de Myhill-Nerode
     * 
     * Algoritmo (Partição-Refinamento):
     * 1. Partição inicial: (finais, não-finais)
     * 2. Repetir até não haver mudanças:
     *    - Para cada partição P e símbolo a:
     *      - Se estados de P vão para partições diferentes com 'a', divide P
     * 3. Cada partição final vira um estado do MinDFA
     * 
     * Complexidade: O(n · m · log n) onde n = estados, m = transições
     * 
     * Entrada: AFD armazenado em DFAXXX
     * Saída: MinDFA armazenado em MinDFAXXX
     *}
    procedure MinimizeDFA;
    
    {**
     * LoadSampleFile - Carrega arquivo de exemplo sample_afn.txt automaticamente
     * 
     * Chamado em FormCreate para carregar um exemplo inicial na interface.
     *}
    procedure LoadSampleFile;
    
    {**
     * LoadTestFiles - Carrega lista de arquivos .txt do diretório testes/
     * 
     * Popula o ComboBox cmbTestFiles com todos os arquivos de teste disponíveis.
     * Procura em: bin/../testes/ ou ./testes/
     *}
    procedure LoadTestFiles;
    
    {**
     * OnTestFileSelected - Event handler para seleção de arquivo de teste
     * 
     * Chamado quando o usuário seleciona um arquivo no ComboBox.
     * Carrega o arquivo automaticamente no editor de entrada.
     *}
    procedure OnTestFileSelected(Sender: TObject);
    
    {**
     * DrawAutomaton - Desenha o diagrama de um autômato em um TCanvas
     * 
     * Parâmetros:
     *   ACanvas: Superfície de desenho (de um TPaintBox)
     *   States: Lista de estados para desenhar
     *   Initials: Estados iniciais (desenha seta de entrada)
     *   Finals: Estados finais (desenha círculo duplo)
     *   Transitions: Array com todas as transições
     * 
     * Layout:
     *   - Máximo 4 estados por linha
     *   - Espaçamento: 100px horizontal, 80px vertical
     *   - Margens: 60px (esquerda), 40px (topo)
     *   - Estados: círculos de raio 20px
     *   - Estados finais: círculo duplo com fundo amarelo
     *   - Transições: linhas com setas e rótulos
     *}
    procedure DrawAutomaton(ACanvas: TCanvas; States, Initials, Finals: TStringList;
      const Transitions: TTransitionArray);
  public
  end;

  {****************************************************************************
   * TStringSet - Classe auxiliar para representar conjuntos de strings
   * 
   * Implementa um conjunto (set) matemático usando TStringList internamente.
   * Garante que não há duplicatas e mantém elementos ordenados.
   * 
   * USADA PRINCIPALMENTE PARA:
   * - Representar conjuntos de estados durante conversões
   * - Calcular epsilon-closure (conjunto de estados alcançáveis)
   * - Estados compostos do AFD (ex: (q0,q1,q2))
   * 
   * MÉTODOS PRINCIPAIS:
   * - Add: Adiciona elemento ao conjunto (ignora duplicatas)
   * - Contains: Verifica se elemento está no conjunto
   * - ToString: Retorna representação "(q0,q1,q2)"
   * - Clone: Cria cópia independente do conjunto
   * - IsEmpty: Verifica se conjunto está vazio
   * - Count: Retorna número de elementos
   * - Item: Acessa elemento por índice
   ****************************************************************************}
  TStringSet = class
  private
    FList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const s: string);
    function Contains(const s: string): Boolean;
    function ToString: string; override;
    function Clone: TStringSet;
    function IsEmpty: Boolean;
    function Count: Integer;
    function Item(i: Integer): string;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TStringSet - Implementação da classe auxiliar de conjuntos }

{**
 * Create - Construtor da classe TStringSet
 * 
 * Inicializa a TStringList interna com propriedades especiais:
 * - Sorted = True: Mantém elementos ordenados automaticamente
 * - Duplicates = dupIgnore: Ignora tentativas de adicionar duplicatas
 * 
 * Essas configurações garantem comportamento matemático de conjunto.
 *}
constructor TStringSet.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupIgnore;
end;

{**
 * Destroy - Destrutor da classe TStringSet
 * 
 * IMPORTANTE: Libera a memória da FList para evitar memory leak.
 * Todo objeto criado com Create DEVE ter Free chamado manualmente em Pascal.
 *}
destructor TStringSet.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{**
 * Add - Adiciona um elemento ao conjunto
 * 
 * Parâmetro:
 *   s: String a ser adicionada
 * 
 * Comportamento:
 * - Ignora strings vazias
 * - Ignora duplicatas automaticamente (devido a FList.Duplicates = dupIgnore)
 * - Mantém conjunto ordenado alfabeticamente
 *}
procedure TStringSet.Add(const s: string);
begin
  if s = '' then Exit;
  FList.Add(s);
end;

{**
 * Contains - Verifica se um elemento está no conjunto
 * 
 * Parâmetro:
 *   s: String a ser procurada
 * 
 * Retorno:
 *   True se o elemento existe no conjunto, False caso contrário
 * 
 * Complexidade: O(log n) devido à busca binária em lista ordenada
 *}
function TStringSet.Contains(const s: string): Boolean;
begin
  Result := FList.IndexOf(s) >= 0;
end;

{**
 * Clone - Cria uma cópia independente do conjunto
 * 
 * Retorno:
 *   Novo objeto TStringSet com os mesmos elementos
 * 
 * IMPORTANTE: O caller (quem chama) é responsável por dar Free() no objeto retornado!
 * 
 * Uso típico:
 *   conjunto2 := conjunto1.Clone;
 *   try
 *     // usar conjunto2
 *   finally
 *     conjunto2.Free;  // NÃO ESQUECER!
 *   end;
 *}
function TStringSet.Clone: TStringSet;
var
  r: TStringSet;
  i: Integer;
begin
  r := TStringSet.Create;
  for i := 0 to FList.Count - 1 do
    r.Add(FList[i]);
  Result := r;
end;

{**
 * IsEmpty - Verifica se o conjunto está vazio
 * 
 * Retorno: True se não há elementos, False caso contrário
 *}
function TStringSet.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

{**
 * ToString - Converte o conjunto para representação textual
 * 
 * Retorno: String no formato "(elem1,elem2,elem3)"
 * 
 * Exemplo:
 *   Conjunto vazio: "()"
 *   Um elemento: "(q0)"
 *   Múltiplos: "(q0,q1,q2)"
 * 
 * Este formato é usado para nomear estados compostos do AFD.
 *}
function TStringSet.ToString: string;
var
  i: Integer;
begin
  Result := '{';
  for i := 0 to FList.Count - 1 do
  begin
    if i > 0 then Result += ',';
    Result += FList[i];
  end;
  Result += '}';
end;

{**
 * Count - Retorna o número de elementos no conjunto
 * 
 * Retorno: Quantidade de elementos (inteiro >= 0)
 *}
function TStringSet.Count: Integer;
begin
  Result := FList.Count;
end;

{**
 * Item - Acessa elemento do conjunto por índice
 * 
 * Parâmetro:
 *   i: Índice do elemento (0-based, ou seja, 0 até Count-1)
 * 
 * Retorno: String do elemento na posição i
 * 
 * IMPORTANTE: Não verifica bounds! Caller deve garantir 0 <= i < Count
 *}
function TStringSet.Item(i: Integer): string;
begin
  Result := FList[i];
end;

{ TFormMain - Implementação do formulário principal }

{****************************************************************************
 * FormCreate - Event handler chamado ao criar o formulário
 * 
 * Este método é executado ANTES da janela aparecer na tela.
 * Responsável por inicializar todas as estruturas de dados e componentes.
 * 
 * INICIALIZAÇÕES:
 * 1. Configura título da janela
 * 2. Cria todas as TStringList (IMPORTANTE: evita access violations)
 * 3. Desabilita botão "Usar como Input" (só habilita após conversão)
 * 4. Cria ComboBox de seleção de testes
 * 5. Carrega lista de arquivos de teste do diretório testes/
 * 6. Carrega arquivo de exemplo inicial (sample_afn.txt)
 * 
 * NOTA: Cada TStringList.Create PRECISA ter correspondente .Free no destrutor!
 ****************************************************************************}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Define o título da janela
  Caption := 'Conversor AFN-ε → AFN → AFD → MinDFA';
  memoInput.Lines.Clear;
  
  {**
   * Inicializar TODAS as listas de dados
   * 
   * CRÍTICO: TStringList.Create aloca memória dinamicamente.
   * Se não criarmos aqui, ao tentar acessar estas variáveis,
   * teremos "Access Violation" (erro de ponteiro nulo).
   * 
   * Cada autômato precisa de suas próprias listas:
   * - EpsilonNFAXXX: Para o AFN com epsilon original
   * - NFAXXX: Para o AFN após remover epsilon
   * - DFAXXX: Para o AFD após conversão
   * - MinDFAXXX: Para o AFD minimizado
   *}
  EpsilonNFAStates := TStringList.Create;
  EpsilonNFAAlphabet := TStringList.Create;
  EpsilonNFAInitials := TStringList.Create;
  EpsilonNFAFinals := TStringList.Create;
  NFAStates := TStringList.Create;
  NFAAlphabet := TStringList.Create;
  NFAInitials := TStringList.Create;
  NFAFinals := TStringList.Create;
  MinDFAStates := TStringList.Create;
  MinDFAFinals := TStringList.Create;
  
  {**
   * Desabilitar botão "Usar como Input" no início
   * 
   * Este botão só deve estar ativo APÓS uma conversão bem-sucedida,
   * para permitir que o usuário copie o AFN resultante para a entrada.
   *}
  if Assigned(btnUseAsInput) then
    btnUseAsInput.Enabled := False;
  
  {**
   * Criar ComboBox para seleção de arquivos de teste
   * 
   * Este componente é criado programaticamente (não visualmente no .lfm)
   * para permitir seleção rápida dos arquivos em testes/.
   * 
   * Posicionamento:
   * - Parent = Panel1: Contém o ComboBox no painel superior
   * - Left/Top: Posicionado entre edtFilePath e btnLoadFile
   * - Width: Calcula espaço disponível entre componentes
   * - Style = csDropDownList: Não permite edição, apenas seleção
   * - OnChange: Evento dispara ao selecionar arquivo
   *}
  cmbTestFiles := TComboBox.Create(Self);
  cmbTestFiles.Parent := Panel1;
  cmbTestFiles.Left := edtFilePath.Left + edtFilePath.Width + 8;
  cmbTestFiles.Top := edtFilePath.Top;
  cmbTestFiles.Width := btnLoadFile.Left - (edtFilePath.Left + edtFilePath.Width) - 16;
  cmbTestFiles.Style := csDropDownList;
  cmbTestFiles.OnChange := @OnTestFileSelected;
  
  // Carregar lista de arquivos de teste disponíveis
  LoadTestFiles;
  
  // Carregar arquivo exemplo padrão (se existir)
  LoadSampleFile;
end;

{****************************************************************************
 * LoadTestFiles - Carrega lista de arquivos .txt do diretório testes/
 * 
 * Este método popula o ComboBox cmbTestFiles com todos os arquivos .txt
 * encontrados no diretório testes/.
 * 
 * ALGORITMO:
 * 1. Limpa ComboBox e adiciona item padrão
 * 2. Determina caminho do diretório testes/
 *    - Primeiro tenta: bin/../testes/ (executável compilado)
 *    - Se não existe, tenta: ./testes/ (executando da raiz)
 * 3. Usa FindFirst/FindNext para iterar arquivos .txt
 * 4. Adiciona cada arquivo encontrado ao ComboBox
 * 
 * ESTRUTURA DE DADOS:
 * - TSearchRec: Record do sistema com info do arquivo
 * - searchRec.Name: Nome do arquivo encontrado
 * - faAnyFile: Flag para buscar qualquer tipo de arquivo
 * 
 * IMPORTANTE: Sempre chamar FindClose após FindFirst para liberar recursos!
 ****************************************************************************}
procedure TFormMain.LoadTestFiles;
var
  searchRec: TSearchRec;
  testPath: string;
  fileName: string;
begin
  cmbTestFiles.Items.Clear;
  cmbTestFiles.Items.Add('-- Selecione um teste --');
  
  testPath := ExtractFilePath(Application.ExeName) + '..\testes\';
  if not DirectoryExists(testPath) then
    testPath := 'testes\';
  
  if DirectoryExists(testPath) then
  begin
    WriteLn('[GUI] Carregando arquivos de teste de: ', testPath);
    if FindFirst(testPath + '*.txt', faAnyFile, searchRec) = 0 then
    begin
      repeat
        if (searchRec.Name <> '.') and (searchRec.Name <> '..') then
        begin
          fileName := ExtractFileName(searchRec.Name);
          cmbTestFiles.Items.AddObject(fileName, TObject(PtrInt(cmbTestFiles.Items.Count)));
          WriteLn('[GUI] Encontrado: ', fileName);
        end;
      until FindNext(searchRec) <> 0;
      FindClose(searchRec);
    end;
  end
  else
    WriteLn('[GUI] Pasta de testes nao encontrada: ', testPath);
  
  cmbTestFiles.ItemIndex := 0;
end;

procedure TFormMain.OnTestFileSelected(Sender: TObject);
var
  testPath, fullPath: string;
begin
  if (cmbTestFiles.ItemIndex > 0) and (cmbTestFiles.ItemIndex < cmbTestFiles.Items.Count) then
  begin
    testPath := ExtractFilePath(Application.ExeName) + '..\testes\';
    if not DirectoryExists(testPath) then
      testPath := 'testes\';
    
    fullPath := testPath + cmbTestFiles.Text;
    
    if FileExists(fullPath) then
    begin
      WriteLn('[GUI] Carregando arquivo de teste: ', fullPath);
      try
        memoInput.Lines.LoadFromFile(fullPath);
        edtFilePath.Text := fullPath;
        WriteLn('[GUI] Arquivo de teste carregado: ', memoInput.Lines.Count, ' linhas');
      except
        on E: Exception do
        begin
          WriteLn('[ERRO] Falha ao carregar teste: ', E.Message);
          ShowMessage('Erro ao carregar arquivo de teste: ' + E.Message);
        end;
      end;
    end
    else
      WriteLn('[ERRO] Arquivo nao encontrado: ', fullPath);
  end;
end;

{**
 * LoadSampleFile - Carrega arquivo de exemplo inicial na interface
 * 
 * QUANDO É CHAMADO:
 * - Automaticamente em FormCreate (ao abrir o programa)
 * - Garante que interface sempre inicia com exemplo válido
 * 
 * OBJETIVO:
 * Fornecer exemplo funcional para o usuário testar o programa
 * sem precisar criar arquivo manualmente.
 * 
 * ESTRATÉGIA DE BUSCA:
 * 1. Tentar carregar sample_afn.txt do diretório src/
 * 2. Se não encontrar, criar exemplo hardcoded na memória
 * 
 * EXEMPLO HARDCODED:
 * AFN que aceita strings sobre (a,b) terminadas em "ab"
 * 
 * Formato:
 *   Linha 0: Alfabeto → "a b"
 *   Linha 1: Estados → "q0 q1 q2"
 *   Linha 2: Inicial → "q0"
 *   Linha 3: Final → "q2"
 *   Linhas 4+: Transições
 *     q0 --a--> q0  (loop: consome 'a')
 *     q0 --b--> q0  (loop: consome 'b')
 *     q0 --a--> q1  (começa sequência "ab")
 *     q1 --b--> q2  (completa "ab" → aceita)
 * 
 * LINGUAGEM ACEITA:
 *   L = ( w ∈ (a,b)* | w termina em "ab" )
 *   Exemplos aceitos: "ab", "aab", "bab", "aaab", "abab"
 *   Exemplos rejeitados: "a", "b", "aa", "ba", "aba"
 * 
 * DIAGRAMA DO AUTÔMATO:
 *   ┌─────┐  a,b  ┌─────┐   b   ┌─────┐
 *   │ →q0 │◄─────┤  q0 │      │ *q2 │
 *   └──┬──┘       └──┬──┘       └─────┘
 *      │ a           │
 *      └────►┌─────┐ │
 *            │  q1 │◄┘
 *            └─────┘
 * 
 * POR QUE EXEMPLO HARDCODED?
 * - Funciona mesmo se arquivo sample_afn.txt não existir
 * - Usuário pode testar imediatamente após abrir programa
 * - Demonstra formato correto de entrada
 * - Facilita debugging (entrada conhecida e previsível)
 **}
procedure TFormMain.LoadSampleFile;
var
  samplePath: string;
begin
  {**
   * BUSCAR ARQUIVO sample_afn.txt
   * 
   * TENTATIVA 1: bin/../src/sample_afn.txt
   *   - Estrutura após compilação
   *   - Executável em bin/, fonte em src/
   * 
   * TENTATIVA 2: src/sample_afn.txt
   *   - Executando da raiz do projeto
   *   - Útil durante desenvolvimento
   **}
  samplePath := ExtractFilePath(Application.ExeName) + '..\src\sample_afn.txt';
  if not FileExists(samplePath) then
    samplePath := 'src\sample_afn.txt';
  
  {** CASO 1: Arquivo encontrado → carregar **}
  if FileExists(samplePath) then
  begin
    memoInput.Lines.LoadFromFile(samplePath);
    edtFilePath.Text := samplePath;
  end
  else
  begin
    {**
     * CASO 2: Arquivo não encontrado → usar exemplo hardcoded
     * 
     * FORMATO DO AUTÔMATO:
     * 
     * LINHA 0 - ALFABETO: "a b"
     *   Símbolos: (a, b)
     * 
     * LINHA 1 - ESTADOS: "q0 q1 q2"
     *   Q = (q0, q1, q2)
     *   Total: 3 estados
     * 
     * LINHA 2 - INICIAL: "q0"
     *   q₀ = q0
     * 
     * LINHA 3 - FINAIS: "q2"
     *   F = (q2)
     * 
     * LINHAS 4-7 - TRANSIÇÕES:
     *   δ(q0, a) = (q0, q1)  (não-determinístico!)
     *   δ(q0, b) = (q0)
     *   δ(q1, b) = (q2)
     * 
     * ANÁLISE:
     * - AFN não-determinístico (q0 com 'a' vai para 2 estados)
     * - Estado q0: Loop enquanto não vê "ab"
     * - Estado q1: Viu 'a', esperando 'b'
     * - Estado q2: Viu "ab" completo → aceita
     * 
     * TRACE DE EXECUÇÃO:
     * String "aab":
     *   q0 --a--> (q0,q1)
     *   Caminho 1: q0 --a--> q0 --b--> q0 (rejeita)
     *   Caminho 2: q0 --a--> q1 --a--> ∅ (rejeita)
     *   Caminho 3: q0 --a--> q0 --a--> (q0,q1) --b--> (q0,q2)
     *              └─ atinge q2 → ACEITA!
     **}
    memoInput.Lines.Add('a b');          // Alfabeto
    memoInput.Lines.Add('q0 q1 q2');     // Estados
    memoInput.Lines.Add('q0');           // Inicial
    memoInput.Lines.Add('q2');           // Final
    memoInput.Lines.Add('q0 a q0');      // Self-loop com 'a'
    memoInput.Lines.Add('q0 b q0');      // Self-loop com 'b'
    memoInput.Lines.Add('q0 a q1');      // Começa "ab"
    memoInput.Lines.Add('q1 b q2');      // Completa "ab" → aceita
    edtFilePath.Text := '(exemplo padrão)';
  end;
end;

{**
 * btnLoadFileClick - Event handler para botão "Carregar Arquivo"
 * 
 * QUANDO É CHAMADO:
 * - Usuário clica no botão de carregar arquivo
 * - Permite selecionar arquivo .txt do sistema de arquivos
 * 
 * OBJETIVO:
 * Abrir diálogo nativo do OS para seleção de arquivo,
 * carregar conteúdo no editor de entrada.
 * 
 * COMPONENTE TOpenDialog:
 * - Diálogo padrão do Windows/Linux/macOS
 * - Configurações:
 *   Filter: Define tipos de arquivo visíveis
 *   DefaultExt: Extensão padrão se usuário não especificar
 *   FileName: Caminho completo do arquivo selecionado
 * 
 * FORMATO DO FILTER:
 * Sintaxe: "Descrição1|Padrão1|Descrição2|Padrão2|..."
 * 
 * Exemplo usado:
 * "Arquivos de texto (*.txt)|*.txt|Todos os arquivos (*.*)|*.*"
 * 
 * Cria dois filtros:
 * 1. "Arquivos de texto (*.txt)" → mostra apenas .txt
 * 2. "Todos os arquivos (*.*)" → mostra todos
 * 
 * PADRÕES WILDCARD:
 * *.txt: Qualquer nome terminado em .txt
 * *.*: Qualquer arquivo com qualquer extensão
 * test*.txt: Arquivos começando com "test" e terminando em .txt
 * 
 * MÉTODO Execute:
 * Retorno: Boolean
 * - True: Usuário selecionou arquivo e clicou OK
 * - False: Usuário cancelou (clicou Cancel ou fechou janela)
 * 
 * Se True:
 * - OpenDialog.FileName contém caminho completo selecionado
 * - Exemplo: "C:\\Users\\João\\Documents\\teste.txt"
 * 
 * TRATAMENTO DE ERROS:
 * Same as OnTestFileSelected - captura exceções de I/O
 * 
 * DIFERENÇA vs OnTestFileSelected:
 * - btnLoadFileClick: Permite escolher QUALQUER arquivo do sistema
 * - OnTestFileSelected: Apenas arquivos pré-listados em testes/
 * 
 * FLUXO TÍPICO:
 * 1. Usuário clica botão "Carregar"
 * 2. Janela de diálogo abre
 * 3. Usuário navega no sistema de arquivos
 * 4. Usuário seleciona arquivo .txt
 * 5. Usuário clica "Abrir"
 * 6. Execute retorna True
 * 7. Arquivo é carregado em memoInput
 * 8. Caminho é mostrado em edtFilePath
 **}
procedure TFormMain.btnLoadFileClick(Sender: TObject);
begin
  {**
   * CONFIGURAR FILTROS DO DIÁLOGO
   * 
   * Filter: String com pares "descrição|padrão"
   * Formato visual no diálogo:
   *   ┌─ Tipo de arquivo: ────────┐
   *   │ Arquivos de texto (*.txt) │
   *   │ Todos os arquivos (*.*)   │
   *   └───────────────────────────┘
   * 
   * DefaultExt: Extensão adicionada automaticamente
   * Se usuário digitar "teste" → salva como "teste.txt"
   * Se usuário digitar "teste.txt" → mantém "teste.txt"
   **}
  OpenDialog.Filter := 'Arquivos de texto (*.txt)|*.txt|Todos os arquivos (*.*)|*.*';
  OpenDialog.DefaultExt := 'txt';
  
  {**
   * EXECUTAR DIÁLOGO E PROCESSAR RESULTADO
   * 
   * Execute: Abre janela modal (bloqueia programa até usuário decidir)
   * Retorna:
   * - True: Arquivo selecionado (OpenDialog.FileName válido)
   * - False: Cancelado (não fazer nada)
   * 
   * IMPORTANTE: Execute não lança exceções!
   * Possíveis problemas:
   * - Usuário fecha janela: Execute → False
   * - Usuário cancela: Execute → False
   * - Erro de permissão: Execute → False
   * - Arquivo não existe: Execute → False
   * 
   * Apenas se Execute = True, tentar carregar arquivo.
   **}
  if OpenDialog.Execute then
  begin
    WriteLn('[GUI] Carregando arquivo: ', OpenDialog.FileName);
    
    {**
     * CARREGAR ARQUIVO COM PROTEÇÃO DE EXCEÇÕES
     * 
     * try-except: Captura erros de I/O durante leitura
     * 
     * OPERAÇÕES PROTEGIDAS:
     * 1. LoadFromFile: Pode falhar se:
     *    - Arquivo foi deletado após seleção
     *    - Sem permissão de leitura
     *    - Arquivo travado por outro processo
     *    - Arquivo corrompido
     *    - Sem espaço em memória
     * 
     * 2. Atualizar edtFilePath: Mostra caminho ao usuário
     * 
     * 3. Logar contagem de linhas: Validação básica
     * 
     * Se exceção ocorrer:
     * - Logar no console (debugging)
     * - Alertar usuário (ShowMessage)
     * - NÃO crashar programa (handled)
     * - memoInput mantém conteúdo anterior (transação atômica)
     **}
    try
      memoInput.Lines.LoadFromFile(OpenDialog.FileName);
      edtFilePath.Text := OpenDialog.FileName;
      WriteLn('[GUI] Arquivo carregado: ', memoInput.Lines.Count, ' linhas');
    except
      on E: Exception do
      begin
        WriteLn('[ERRO] Falha ao carregar: ', E.Message);
        ShowMessage('Erro ao carregar arquivo: ' + E.Message);
      end;
    end;
  end;
end;

{**
 * btnClearClick - Limpa completamente a interface e todos os dados
 * 
 * QUANDO É CHAMADO:
 * - Usuário clica botão "Limpar Tudo"
 * - Reset completo do programa para estado inicial
 * 
 * OBJETIVO:
 * Restaurar programa ao estado limpo, como se tivesse acabado de abrir
 * (exceto exemplo padrão, que não é recarregado).
 * 
 * ALGORITMO DE LIMPEZA:
 * 1. LIMPAR MEMOS (editores de texto):
 *    - memoInput: Entrada do usuário
 *    - memoOutput: Resultado AFD
 *    - memoMinOutput: Resultado AFD minimizado
 *    - memoNFAOutput: Resultado AFN sem ε
 * 
 * 2. LIMPAR CAMPO DE CAMINHO:
 *    - edtFilePath: Mostrar que nenhum arquivo está carregado
 * 
 * 3. LIMPAR ESTRUTURAS DE DADOS DO AFN-ε:
 *    - EpsilonNFAAlphabet: TStringList com alfabeto
 *    - EpsilonNFAStates: TStringList com estados
 *    - EpsilonNFAInitials: TStringSet com estados iniciais
 *    - EpsilonNFAFinals: TStringSet com estados finais
 *    - EpsilonNFATransitions: Array dinâmico de transições
 * 
 * 4. LIMPAR ESTRUTURAS DO AFN (sem ε):
 *    - NFAAlphabet, NFAStates, NFAInitials, NFAFinals
 *    - NFATransitions: Array de transições
 * 
 * 5. LIMPAR ESTRUTURAS DO AFD:
 *    - DFAStates: TStringList com estados (subconjuntos)
 *    - DFAFinals: TStringSet com estados finais
 *    - DFATransitions: Array de transições determinísticas
 *    - DFAInitial: String com estado inicial
 * 
 * 6. LIMPAR ESTRUTURAS DO AFD MINIMIZADO:
 *    - MinDFAStates, MinDFAFinals, MinDFATransitions
 *    - MinDFAInitial: Estado inicial pós-minimização
 * 
 * 7. DESABILITAR BOTÕES DEPENDENTES:
 *    - btnMinimize: Só ativo após conversão AFN→AFD
 *    - btnUseAsInput: Só ativo após remoção de ε
 * 
 * 8. FORÇAR REDESENHO DOS DIAGRAMAS:
 *    - Invalidate: Marca painel como "sujo", força repaint
 *    - Diagramas ficam vazios (sem estados/transições)
 * 
 * SEGURANÇA DE PONTEIROS:
 * Sempre verificar Assigned antes de acessar objetos!
 * 
 * Assigned(obj): Verifica se ponteiro <> nil
 * Se nil: Objeto foi destruído ou nunca criado
 * Se <> nil: Seguro acessar
 * 
 * POR QUE VERIFICAR?
 * - Objetos podem estar nil durante inicialização
 * - Componentes opcionais podem não existir
 * - Previne Access Violation (crash)
 * 
 * GESTÃO DE MEMÓRIA:
 * 
 * 1. TStringList.Clear:
 *    - Remove todos os itens da lista
 *    - NÃO libera a lista (objeto continua existindo)
 *    - Uso: lista.Count = 0 após Clear
 * 
 * 2. TStringSet.Clear:
 *    - Remove todos os elementos do conjunto
 *    - NÃO destrói o conjunto
 *    - Uso: conjunto vazio, mas válido
 * 
 * 3. SetLength(array, 0):
 *    - Redimensiona array dinâmico para tamanho 0
 *    - Libera memória alocada
 *    - Array fica vazio mas válido
 *    - Uso: Length(array) = 0 após
 * 
 * 4. String := '':
 *    - Limpa string (comprimento 0)
 *    - Libera memória da string antiga
 *    - Pascal gerencia automaticamente
 * 
 * WORKFLOW PÓS-LIMPEZA:
 * Usuário precisa:
 * 1. Carregar novo arquivo OU digitar entrada
 * 2. Clicar "AFN-ε → AFN" (se houver ε-transições)
 * 3. Clicar "AFN → AFD"
 * 4. (Opcional) Clicar "Minimizar AFD"
 * 
 * EXEMPLO DE USO:
 * 1. Usuário processa test_ab.txt
 * 2. Vê resultados nos 4 diagramas
 * 3. Quer processar test_binario.txt
 * 4. Clica "Limpar Tudo"
 * 5. Todos os memos ficam vazios
 * 6. Todos os diagramas ficam brancos
 * 7. Botões voltam ao estado inicial
 * 8. Pronto para novo processamento
 **}
procedure TFormMain.btnClearClick(Sender: TObject);
begin
  WriteLn('[GUI] Limpando interface e dados...');
  
  {**
   * FASE 1: Limpar componentes visuais (memos e campos de texto)
   **}
  memoInput.Lines.Clear;      {** Editor de entrada **}
  memoOutput.Lines.Clear;     {** Resultado AFD **}
  memoMinOutput.Lines.Clear;  {** Resultado AFD minimizado **}
  memoNFAOutput.Lines.Clear;  {** Resultado AFN sem ε **}
  edtFilePath.Text := '';     {** Caminho do arquivo carregado **}
  
  {**
   * FASE 2: Limpar estruturas de dados do AFN-ε
   * 
   * Assigned: Verifica se objeto existe antes de acessar
   * Previne Access Violation se objeto foi destruído.
   **}
  if Assigned(EpsilonNFAAlphabet) then EpsilonNFAAlphabet.Clear;
  if Assigned(EpsilonNFAStates) then EpsilonNFAStates.Clear;
  if Assigned(EpsilonNFAInitials) then EpsilonNFAInitials.Clear;
  if Assigned(EpsilonNFAFinals) then EpsilonNFAFinals.Clear;
  
  {**
   * FASE 3: Limpar estruturas do AFN (sem ε-transições)
   **}
  if Assigned(NFAAlphabet) then NFAAlphabet.Clear;
  if Assigned(NFAStates) then NFAStates.Clear;
  if Assigned(NFAInitials) then NFAInitials.Clear;
  if Assigned(NFAFinals) then NFAFinals.Clear;
  
  {**
   * FASE 4: Limpar estruturas do AFD
   **}
  if Assigned(DFAStates) then DFAStates.Clear;
  if Assigned(DFAFinals) then DFAFinals.Clear;
  
  {**
   * FASE 5: Limpar estruturas do AFD minimizado
   **}
  if Assigned(MinDFAStates) then MinDFAStates.Clear;
  if Assigned(MinDFAFinals) then MinDFAFinals.Clear;
  
  {**
   * FASE 6: Liberar arrays dinâmicos de transições
   * 
   * SetLength(array, 0): Redimensiona array para 0 elementos
   * - Libera toda a memória alocada
   * - Array fica válido mas vazio
   * - Length(array) = 0 após operação
   **}
  SetLength(EpsilonNFATransitions, 0);  {** Transições AFN-ε **}
  SetLength(NFATransitions, 0);         {** Transições AFN **}
  SetLength(DFATransitions, 0);         {** Transições AFD **}
  SetLength(MinDFATransitions, 0);      {** Transições AFD minimizado **}
  
  {**
   * FASE 7: Limpar estados iniciais (strings)
   * 
   * String := '': Limpa string e libera memória
   **}
  DFAInitial := '';     {** Estado inicial do AFD **}
  MinDFAInitial := '';  {** Estado inicial do AFD minimizado **}
  
  {**
   * FASE 8: Desabilitar botões que dependem de processamento
   * 
   * Botões só ficam ativos após processamento bem-sucedido:
   * - btnMinimize: Requer AFD pronto
   * - btnUseAsInput: Requer AFN sem ε pronto
   **}
  if Assigned(btnMinimize) then btnMinimize.Enabled := False;
  if Assigned(btnUseAsInput) then btnUseAsInput.Enabled := False;
  
  {**
   * FASE 9: Forçar redesenho dos diagramas
   * 
   * Invalidate: Marca painel como "sujo"
   * - Sistema operacional agendará repaint
   * - OnPaint será chamado automaticamente
   * - Diagramas serão redesenhados (vazios)
   **}
  if Assigned(PaintBoxEpsilonNFA) then PaintBoxEpsilonNFA.Invalidate;
  if Assigned(PaintBoxNFA) then PaintBoxNFA.Invalidate;
  if Assigned(PaintBoxDFA) then PaintBoxDFA.Invalidate;
  if Assigned(PaintBoxMinDFA) then PaintBoxMinDFA.Invalidate;
  WriteLn('[GUI] Limpeza concluida.');
end;

{**
 * btnRemoveEpsilonClick - Executa remoção de ε-transições do AFN-ε
 * 
 * QUANDO É CHAMADO:
 * - Usuário clica botão "AFN-ε → AFN" na interface
 * - Primeiro passo do pipeline de conversão
 * 
 * OBJETIVO:
 * Invocar algoritmo RemoveEpsilonTransitions que:
 * 1. Lê AFN-ε da entrada (memoInput)
 * 2. Calcula ε-fechos de todos os estados
 * 3. Remove todas as transições-ε
 * 4. Ajusta estados finais (se ε-fecho atinge estado final)
 * 5. Exibe resultado em memoNFAOutput e PaintBoxNFA
 * 
 * TEORIA: Remoção de ε-Transições
 * 
 * AFN-ε: Autômato Finito Não-Determinístico com ε-transições
 * - Pode fazer transições sem consumir símbolo
 * - Mais expressivo mas mais complexo de simular
 * 
 * AFN: Autômato Finito Não-Determinístico (sem ε)
 * - Toda transição consome símbolo do alfabeto
 * - Equivalente em poder computacional ao AFN-ε
 * - Mais simples de implementar simulação
 * 
 * TEOREMA:
 * Para todo AFN-ε M, existe AFN M' tal que L(M) = L(M')
 * (mesma linguagem aceita)
 * 
 * ALGORITMO (simplificado):
 * Para cada estado q e símbolo a:
 *   1. Calcular ε-fecho(q): Estados alcançáveis só com ε
 *   2. Para cada estado p ∈ ε-fecho(q):
 *      - Se δ(p, a) = r, adicionar transição q --a--> ε-fecho(r)
 *   3. Ajustar finais: q é final se ε-fecho(q) contém estado final
 * 
 * EXEMPLO:
 * AFN-ε:
 *   q0 --ε--> q1 --a--> q2
 * 
 * ε-fechos:
 *   ε-fecho(q0) = (q0, q1)
 *   ε-fecho(q1) = (q1)
 *   ε-fecho(q2) = (q2)
 * 
 * AFN resultante:
 *   q0 --a--> q2  (pois q1 ∈ ε-fecho(q0) e q1 --a--> q2)
 *   q1 --a--> q2  (transição original)
 * 
 * TRATAMENTO DE ERROS:
 * try-except captura exceções de:
 * - Parse de entrada (formato inválido)
 * - Cálculos matemáticos (grafos cíclicos complexos)
 * - Alocação de memória (autômatos grandes)
 * 
 * Se erro ocorrer:
 * - Logar no console (WriteLn)
 * - Limpar saída e mostrar mensagem de erro
 * - Programa continua executando
 * 
 * COMPLEXIDADE:
 * - Temporal: O(|Q|^3) no pior caso (cálculo de ε-fechos)
 * - Espacial: O(|Q|^2) (matriz de ε-alcance)
 * 
 * FLUXO TÍPICO:
 * 1. Usuário carrega AFN-ε com transições-ε
 * 2. Clica "AFN-ε → AFN"
 * 3. RemoveEpsilonTransitions é chamado
 * 4. Resultado aparece em memoNFAOutput
 * 5. Diagrama visual em PaintBoxNFA
 **}
procedure TFormMain.btnRemoveEpsilonClick(Sender: TObject);
begin
  WriteLn('[GUI] Iniciando remocao de epsilon-transicoes...');
  
  {**
   * INVOCAR ALGORITMO COM PROTEÇÃO DE EXCEÇÕES
   * 
   * try-except: Captura erros durante processamento
   * 
   * ERROS POSSÍVEIS:
   * 1. Parse error: Formato de entrada inválido
   *    - Falta alfabeto/estados/iniciais/finais
   *    - Transição com estado inexistente
   *    - Símbolo não no alfabeto
   * 
   * 2. Runtime error: Problema durante cálculo
   *    - Pilha de recursão estourou (grafo muito profundo)
   *    - Memória insuficiente (muitos estados)
   * 
   * 3. Access violation: Ponteiro nil
   *    - Componente GUI não inicializado
   *    - Estrutura de dados corrompida
   **}
  try
    {**
     * CHAMAR ALGORITMO DE REMOÇÃO
     * 
     * RemoveEpsilonTransitions: Procedure definida anteriormente
     * - Lê entrada de memoInput
     * - Processa AFN-ε
     * - Gera AFN equivalente sem ε
     * - Escreve resultado em memoNFAOutput
     * - Atualiza PaintBoxNFA com diagrama
     * - Pode lançar Exception se falhar
     **}
    RemoveEpsilonTransitions;
    WriteLn('[GUI] Remocao de epsilon concluida com sucesso!');
  except
    on E: Exception do
    begin
      {**
       * TRATAMENTO DE ERRO
       * 
       * E: Exception: Objeto com informações do erro
       * E.Message: Descrição textual do problema
       * E.ClassName: Tipo da exceção
       * 
       * AÇÕES:
       * 1. Logar no console (debugging / log file)
       * 2. Limpar saída para evitar dados parciais
       * 3. Mostrar mensagem de erro ao usuário
       * 4. NÃO re-lançar exceção (handled gracefully)
       **}
      WriteLn('[ERRO] Falha na remocao de epsilon: ', E.Message);
      memoNFAOutput.Lines.Clear;
      memoNFAOutput.Lines.Add('ERRO: ' + E.Message);
    end;
  end;
end;

{**
 * btnUseAsInputClick - Copia AFN resultante para entrada
 * 
 * QUANDO É CHAMADO:
 * - Usuário clica botão "Usar como entrada" após remover ε
 * - Permite workflow sequencial de transformações
 * 
 * OBJETIVO:
 * Copiar AFN sem ε-transições (de memoNFAOutput) para memoInput,
 * permitindo aplicar próxima transformação (AFN → AFD).
 * 
 * WORKFLOW TÍPICO:
 * 1. Carregar AFN-ε inicial
 * 2. Clicar "AFN-ε → AFN" (remove epsilon)
 * 3. Clicar "Usar como entrada" (copia resultado)
 * 4. Clicar "AFN → AFD" (determinização)
 * 5. Clicar "Minimizar AFD" (minimização)
 * 
 * ESTRUTURA DOS DADOS COPIADOS:
 * 
 * NFAAlphabet: TStringList com símbolos do alfabeto
 *   Exemplo: ("a", "b")
 * 
 * NFAStates: TStringList com nomes dos estados
 *   Exemplo: ("q0", "q1", "q2")
 * 
 * NFAInitials: TStringSet com estados iniciais
 *   Exemplo: (q0)
 * 
 * NFAFinals: TStringSet com estados finais
 *   Exemplo: (q2)
 * 
 * NFATransitions: Array de TTransition
 *   Record com: FromState, Symbol, ToState
 *   Exemplo: [(q0, a, q1), (q1, b, q2)]
 * 
 * FORMATO DE SAÍDA (memoInput):
 * Linha 0: Alfabeto separado por espaços
 *   "a b"
 * 
 * Linha 1: Estados separados por espaços
 *   "q0 q1 q2"
 * 
 * Linha 2: Estados iniciais separados por espaços
 *   "q0"
 * 
 * Linha 3: Estados finais separados por espaços
 *   "q2"
 * 
 * Linhas 4+: Transições (FromState Symbol ToState)
 *   "q0 a q1"
 *   "q1 b q2"
 * 
 * MÉTODO CommaText:
 * TStringList.CommaText: Concatena itens com vírgulas
 *   Exemplo: ["a", "b"] → "a,b"
 * 
 * Replace(',', ' '): Substitui vírgulas por espaços
 *   "a,b" → "a b"
 * 
 * POR QUE CONVERTER FORMATO?
 * - TStringList armazena internamente com vírgulas
 * - Formato de arquivo usa espaços
 * - Replace garante compatibilidade
 * 
 * ITERAÇÃO DE TRANSIÇÕES:
 * for i := 0 to High(NFATransitions):
 *   High(array): Retorna maior índice válido
 *   Array[0..High] contém todos os elementos
 * 
 * EXEMPLO DE EXECUÇÃO:
 * 
 * Entrada (NFAAlphabet, NFAStates, etc.):
 *   Alfabeto: (a, b)
 *   Estados: (q0, q1, q2)
 *   Inicial: q0
 *   Finais: (q2)
 *   Transições:
 *     (q0, a, q0)
 *     (q0, b, q0)
 *     (q0, a, q1)
 *     (q1, b, q2)
 * 
 * Saída (memoInput após cópia):
 *   a b
 *   q0 q1 q2
 *   q0
 *   q2
 *   q0 a q0
 *   q0 b q0
 *   q0 a q1
 *   q1 b q2
 * 
 * MENSAGEM AO USUÁRIO:
 * ShowMessage com LineEnding:
 * - Informa que cópia foi bem-sucedida
 * - Instrui próximo passo (clicar "AFN → AFD")
 * - LineEnding: Constante multiplataforma para quebra de linha
 *   Windows: CR+LF (\r\n)
 *   Linux/Mac: LF (\n)
 **}
procedure TFormMain.btnUseAsInputClick(Sender: TObject);
var
  i: Integer;  {** Índice para iterar transições **}
begin
  WriteLn('[GUI] Usando AFN resultante como entrada...');
  
  {**
   * FASE 1: Limpar entrada atual
   * Remove conteúdo anterior para evitar mistura de dados.
   **}
  memoInput.Lines.Clear;
  
  {**
   * FASE 2: Copiar alfabeto (linha 0)
   * 
   * CommaText: Retorna string com itens separados por vírgula
   * Replace(',', ' '): Converte vírgulas em espaços
   * 
   * Exemplo:
   * NFAAlphabet = ["a", "b"]
   * CommaText = "a,b"
   * Replace = "a b"
   **}
  memoInput.Lines.Add(NFAAlphabet.CommaText.Replace(',', ' '));
  
  {**
   * FASE 3: Copiar estados (linha 1)
   * 
   * NFAStates.CommaText: Lista de estados separados por vírgula
   * Replace: Converte para formato com espaços
   **}
  memoInput.Lines.Add(NFAStates.CommaText.Replace(',', ' '));
  
  {**
   * FASE 4: Copiar estados iniciais (linha 2)
   * 
   * NFAInitials: TStringSet (conjunto de strings)
   * CommaText: Converte conjunto em string separada por vírgula
   **}
  memoInput.Lines.Add(NFAInitials.CommaText.Replace(',', ' '));
  
  {**
   * FASE 5: Copiar estados finais (linha 3)
   * 
   * NFAFinals: TStringSet com estados de aceitação
   **}
  memoInput.Lines.Add(NFAFinals.CommaText.Replace(',', ' '));
  
  {**
   * FASE 6: Copiar transições (linhas 4+)
   * 
   * High(NFATransitions): Retorna maior índice do array
   * Se array tem 4 elementos: índices 0,1,2,3 → High = 3
   * 
   * Para cada transição:
   * - FromState: Estado origem
   * - Symbol: Símbolo consumido
   * - ToState: Estado destino
   * 
   * Formato: "FromState Symbol ToState"
   * Exemplo: "q0 a q1"
   **}
  for i := 0 to High(NFATransitions) do
    memoInput.Lines.Add(NFATransitions[i].FromState + ' ' + 
                       NFATransitions[i].Symbol + ' ' + 
                       NFATransitions[i].ToState);
  
  WriteLn('[GUI] AFN copiado para entrada. Pronto para converter AFN -> AFD.');
  
  {**
   * Informar usuário sobre sucesso e próximo passo
   * 
   * LineEnding: Constante do sistema (Windows: \r\n, Unix: \n)
   * ShowMessage: Diálogo modal (bloqueia até usuário clicar OK)
   **}
  ShowMessage('AFN sem epsilon copiado para a entrada!' + LineEnding +
              'Agora você pode clicar em "AFN → AFD" para converter.');
end;

{**
 * btnConvertClick - Executa conversão AFN → AFD (determinização)
 * 
 * QUANDO É CHAMADO:
 * - Usuário clica botão "AFN → AFD" na interface
 * - Pode ser usado diretamente após carregar AFN
 * - Ou após "Usar como entrada" (workflow sequencial)
 * 
 * OBJETIVO:
 * Invocar algoritmo ConvertAFNtoAFD (Subset Construction) que:
 * 1. Lê AFN da entrada (memoInput)
 * 2. Aplica construção de subconjuntos
 * 3. Gera AFD equivalente
 * 4. Exibe resultado em memoOutput e PaintBoxDFA
 * 5. Habilita botão "Minimizar AFD"
 * 
 * TEORIA: Construção de Subconjuntos (Subset Construction)
 * 
 * AFN: Autômato Finito Não-Determinístico
 * - Um estado pode ter múltiplas transições com mesmo símbolo
 * - δ(q, a) retorna CONJUNTO de estados
 * - Simulação requer backtracking ou busca em paralelo
 * 
 * AFD: Autômato Finito Determinístico
 * - Cada estado tem EXATAMENTE UMA transição por símbolo
 * - δ(q, a) retorna UM ÚNICO estado
 * - Simulação linear (sem backtracking)
 * 
 * TEOREMA DE EQUIVALÊNCIA:
 * Para todo AFN M, existe AFD M' tal que L(M) = L(M')
 * (mesma linguagem aceita)
 * 
 * ALGORITMO DE CONVERSÃO:
 * 1. Estado inicial do AFD = (q0) (conjunto com inicial do AFN)
 * 2. Para cada conjunto S de estados (BFS):
 *    Para cada símbolo a:
 *      a) Calcular T = ⋃(q∈S) δ(q, a)  (união dos destinos)
 *      b) Criar transição S --a--> T no AFD
 *      c) Se T é novo, adicionar à fila
 * 3. Estado final do AFD: Contém algum estado final do AFN
 * 
 * EXEMPLO:
 * AFN:
 *   q0 --a--> (q0, q1)
 *   q0 --b--> (q0)
 *   q1 --b--> (q2)
 *   Final: (q2)
 * 
 * AFD:
 *   (q0) --a--> (q0,q1)
 *   (q0) --b--> (q0)
 *   (q0,q1) --a--> (q0,q1)
 *   (q0,q1) --b--> (q0,q2)  (final!)
 *   (q0,q2) --a--> (q0,q1)
 *   (q0,q2) --b--> (q0)
 *   Final: (q0,q2), (q0,q2)
 * 
 * EXPLOSÃO EXPONENCIAL:
 * No pior caso, AFD pode ter 2^n estados (n = estados do AFN)
 * - AFN com 10 estados → AFD com até 1024 estados
 * - Na prática, muitos subconjuntos não são alcançáveis
 * 
 * TRATAMENTO DE ERROS:
 * try-except captura exceções de:
 * - Parse de entrada (formato inválido)
 * - Explosão combinatória (muitos estados)
 * - Memória insuficiente
 * 
 * Se erro ocorrer:
 * - Logar no console
 * - Limpar saída e mostrar erro
 * - Programa continua executando
 * 
 * SE SUCESSO:
 * - Resultado em memoOutput
 * - Diagrama em PaintBoxDFA
 * - Botão "Minimizar AFD" fica disponível
 * 
 * COMPLEXIDADE:
 * - Temporal: O(2^n * |Σ|) no pior caso
 * - Espacial: O(2^n)
 * n = estados do AFN, |Σ| = tamanho do alfabeto
 * 
 * FLUXO TÍPICO:
 * 1. Usuário carrega AFN (com ou sem ε)
 * 2. Clica "AFN → AFD"
 * 3. ConvertAFNtoAFD é chamado
 * 4. Resultado aparece em memoOutput
 * 5. Diagrama visual em PaintBoxDFA
 * 6. Botão "Minimizar AFD" fica disponível
 **}
procedure TFormMain.btnConvertClick(Sender: TObject);
begin
  WriteLn('[GUI] Iniciando conversao AFN -> AFD...');
  
  {**
   * INVOCAR ALGORITMO COM PROTEÇÃO DE EXCEÇÕES
   * 
   * try-except: Captura erros durante processamento
   * 
   * ERROS POSSÍVEIS:
   * 1. Parse error: Formato de entrada inválido
   *    - Falta alfabeto/estados/iniciais/finais
   *    - Transição com estado inexistente
   *    - Símbolo não no alfabeto
   * 
   * 2. Runtime error: Explosão combinatória
   *    - Muitos subconjuntos gerados (2^n)
   *    - Memória insuficiente
   *    - Timeout (processamento muito longo)
   * 
   * 3. Access violation: Ponteiro nil
   *    - Componente GUI não inicializado
   *    - Estrutura de dados corrompida
   **}
  try
    {**
     * CHAMAR ALGORITMO DE DETERMINIZAÇÃO
     * 
     * ConvertAFNtoAFD: Procedure definida anteriormente
     * - Lê AFN de memoInput
     * - Aplica Subset Construction (BFS)
     * - Gera AFD equivalente
     * - Escreve resultado em memoOutput
     * - Atualiza PaintBoxDFA com diagrama
     * - Habilita botão "Minimizar AFD"
     * - Pode lançar Exception se falhar
     **}
    ConvertAFNtoAFD;
    WriteLn('[GUI] Conversao concluida com sucesso!');
  except
    on E: Exception do
    begin
      {**
       * TRATAMENTO DE ERRO
       * 
       * E: Exception: Objeto com informações do erro
       * E.Message: Descrição textual do problema
       * 
       * AÇÕES:
       * 1. Logar no console (debugging)
       * 2. Limpar saída para evitar dados parciais
       * 3. Mostrar mensagem de erro ao usuário
       * 4. NÃO re-lançar exceção (handled gracefully)
       * 5. Programa continua executando
       **}
      WriteLn('[ERRO] Falha na conversao: ', E.Message);
      memoOutput.Lines.Clear;
      memoOutput.Lines.Add('ERRO: ' + E.Message);
    end;
  end;
end;

{**
 * GetSetName - Converte TStringSet para representação textual
 * 
 * TIPO: Função helper / utilitária
 * 
 * OBJETIVO:
 * Converter objeto TStringSet (conjunto de strings) em string legível
 * para exibição em memos, logs e diálogos.
 * 
 * TStringSet: Classe genérica que implementa conjunto (set)
 * - Estrutura: Hash set ou árvore balanceada
 * - Operações: Add, Remove, Contains, Union, Intersection
 * - Sem elementos duplicados
 * - Sem ordem garantida (dependendo da implementação)
 * 
 * MÉTODO ToString:
 * TStringSet.ToString: Método built-in que retorna representação textual
 * 
 * FORMATO TÍPICO:
 * - Elementos separados por vírgula: "q0,q1,q2"
 * - Ou entre chaves: "(q0, q1, q2)"
 * - Ou entre parênteses: "(q0, q1, q2)"
 * - Depende da implementação de TStringSet
 * 
 * EXEMPLO DE USO:
 * 
 * setEstados: TStringSet com (q0, q1, q2)
 * GetSetName(setEstados) → "(q0, q1, q2)"
 * 
 * setFinais: TStringSet com (q2)
 * GetSetName(setFinais) → "(q2)"
 * 
 * setVazio: TStringSet vazio ()
 * GetSetName(setVazio) → "()" ou "[]" ou "()"
 * 
 * ONDE É USADO:
 * 
 * 1. Formatação de estados do AFD:
 *    Estados do AFD são subconjuntos do AFN
 *    Exemplo: Estado "(q0,q1)" representa conjunto (q0, q1)
 * 
 * 2. Logging e debugging:
 *    WriteLn('Estado atual: ', GetSetName(estado));
 * 
 * 3. Exibição em memos:
 *    memoOutput.Lines.Add('Finais: ' + GetSetName(finais));
 * 
 * 4. Geração de nomes únicos:
 *    Converter conjunto em string para usar como chave em dicionário
 * 
 * POR QUE NÃO USAR ToString DIRETAMENTE?
 * - Abstração: Facilita mudanças futuras na representação
 * - Consistência: Ponto único para formatar conjuntos
 * - Legibilidade: Nome da função expressa intenção
 * - Compatibilidade: Pode adicionar lógica extra (ordenação, escape)
 * 
 * ALTERNATIVA MAIS COMPLEXA (não implementada aqui):
 * function GetSetName(setObj: TStringSet): string;
 * var
 *   items: TStringList;
 * begin
 *   items := TStringList.Create;
 *   try
 *     items.AddStrings(setObj);  // Copiar elementos
 *     items.Sort;                // Ordenar alfabeticamente
 *     Result := '(' + items.CommaText + ')';  // Formatar
 *   finally
 *     items.Free;
 *   end;
 * end;
 * 
 * COMPLEXIDADE:
 * - Temporal: O(n) onde n = número de elementos no conjunto
 *   ToString precisa iterar todos os elementos para concatenar
 * - Espacial: O(n) para armazenar string resultante
 * 
 * IMPORTANTE:
 * - Result: Variável especial que armazena retorno da função
 * - Em Pascal, Result := valor define o que a função retornará
 * - Equivalente a return em outras linguagens
 **}
function GetSetName(setObj: TStringSet): string;
begin
  {**
   * Delegar para método ToString do objeto
   * 
   * setObj.ToString: Converte conjunto para string
   * Result: Variável de retorno da função
   * 
   * Implementação simples e direta:
   * - Confia na implementação de TStringSet
   * - Sem formatação adicional
   * - Retorna exatamente o que ToString produz
   **}
  Result := setObj.ToString;
end;

{******************************************************************************
 * ComputeEpsilonClosure - Calcula o epsilon-fecho (ε-closure) de estados
 * 
 * O EPSILON-CLOSURE (ou ε-fecho) de um conjunto de estados é o conjunto de
 * TODOS os estados alcançáveis a partir desses estados usando APENAS
 * epsilon-transições (transições vazias).
 * 
 * EXEMPLO:
 *   Se temos: q0 --ε--> q1 --ε--> q2
 *   Então: ε-closure((q0)) = (q0, q1, q2)
 * 
 * ALGORITMO: DFS (Depth-First Search) usando pilha
 * 1. Inicializa resultado com os estados fornecidos
 * 2. Empilha todos os estados iniciais
 * 3. Enquanto pilha não está vazia:
 *    a) Desempilha um estado
 *    b) Para cada epsilon-transição desse estado:
 *       - Se o estado destino ainda não foi visitado:
 *         * Adiciona ao resultado
 *         * Empilha para processar suas transições
 * 
 * SÍMBOLOS EPSILON RECONHECIDOS:
 * - 'ε' (Unicode)
 * - 'epsilon' (palavra completa)
 * - 'e' (abreviação)
 * - '&' (notação alternativa)
 * 
 * PARÂMETROS:
 *   states: Conjunto de estados iniciais
 *   transitions: Array com todas as transições do autômato
 * 
 * RETORNO:
 *   TStringSet contendo todos os estados no ε-closure
 *   IMPORTANTE: Caller deve dar Free() no objeto retornado!
 * 
 * COMPLEXIDADE:
 *   O(n + m) onde n = número de estados, m = número de ε-transições
 * 
 * NOTA: Usa pilha estática de tamanho 1000 (suficiente para casos práticos)
 ******************************************************************************}
function ComputeEpsilonClosure(const states: TStringSet; 
                               const transitions: TTransitionArray): TStringSet;
var
  stack: array of string;
  stackTop: Integer;
  current, target: string;
  i: Integer;
begin
  // Criar conjunto resultado
  Result := TStringSet.Create;
  
  {**
   * PASSO 1: Inicializar resultado com os estados fornecidos
   * 
   * Um estado sempre pertence ao seu próprio epsilon-closure.
   * Então começamos adicionando todos os estados iniciais.
   *}
  for i := 0 to states.Count - 1 do
  begin
    Result.Add(states.Item(i));
  end;
  
  {**
   * PASSO 2: Inicializar pilha para DFS
   * 
   * Usamos uma pilha implementada com array estático.
   * stackTop aponta para a próxima posição livre (0-based).
   * 
   * Tamanho 1000 é arbitrário mas suficiente para autômatos práticos.
   * Se precisar mais, aumentar SetLength.
   *}
  SetLength(stack, 1000);
  stackTop := 0;
  
  // Adicionar todos os estados iniciais à pilha para processamento
  for i := 0 to states.Count - 1 do
  begin
    stack[stackTop] := states.Item(i);
    Inc(stackTop);  // Incrementa topo da pilha
  end;
  
  {**
   * PASSO 3: Processar pilha (DFS)
   * 
   * Enquanto houver estados para processar:
   * 1. Remove estado da pilha (pop)
   * 2. Procura todas suas epsilon-transições
   * 3. Para cada destino não visitado:
   *    - Adiciona ao resultado
   *    - Empilha para processar (push)
   * 
   * Este loop garante que TODOS os estados alcançáveis por
   * epsilon-transições serão encontrados, não importa quão
   * distantes estejam (pode ter cadeias: q0->q1->q2->q3...).
   *}
  while stackTop > 0 do
  begin
    // Pop: remove estado do topo da pilha
    Dec(stackTop);
    current := stack[stackTop];
    
    {**
     * Procurar TODAS as epsilon-transições do estado atual
     * 
     * Uma epsilon-transição é uma transição cujo símbolo é:
     * - 'ε' (Unicode U+03B5)
     * - 'epsilon' (palavra completa)
     * - 'e' (abreviação simples)
     * - '&' (notação alternativa, usado em algumas literaturas)
     *}
    for i := 0 to High(transitions) do
    begin
      if (transitions[i].FromState = current) and 
         ((transitions[i].Symbol = 'ε') or (transitions[i].Symbol = 'epsilon') or 
          (transitions[i].Symbol = 'e') or (transitions[i].Symbol = '&')) then
      begin
        target := transitions[i].ToState;
        
        {**
         * Se o estado destino ainda NÃO está no epsilon-closure,
         * precisamos:
         * 1. Adicioná-lo ao resultado
         * 2. Empilhá-lo para processar SUAS epsilon-transições
         * 
         * A verificação Contains evita loops infinitos em
         * epsilon-cycles como: q0 --ε--> q1 --ε--> q0
         *}
        if not Result.Contains(target) then
        begin
          Result.Add(target);
          stack[stackTop] := target;
          Inc(stackTop);  // Push na pilha
        end;
      end;
    end;
  end;
end;

{******************************************************************************
 * RemoveEpsilonTransitions - Remove epsilon-transições de um AFN-ε
 * 
 * Este método converte um AFN com epsilon-transições (AFN-ε) em um AFN
 * equivalente SEM epsilon-transições, mantendo a mesma linguagem aceita.
 * 
 * INTUIÇÃO DO ALGORITMO:
 * - No AFN-ε: q0 --ε--> q1 --a--> q2
 * - No AFN: adiciona q0 --a--> q2 (expande a transição considerando o epsilon)
 * 
 * ALGORITMO COMPLETO:
 * 1. PARSE: Lê arquivo de entrada e extrai AFN-ε
 * 2. EPSILON-CLOSURE: Para cada estado, calcula quais estados alcança por ε
 * 3. EXPANSÃO DE TRANSIÇÕES:
 *    Para cada transição p --a--> q (onde 'a' NÃO é epsilon):
 *      Para cada estado r no ε-closure(q):
 *        Adiciona transição p --a--> r no AFN
 * 4. ESTADOS FINAIS: Um estado é final se seu ε-closure contém algum final
 * 5. ESTADOS INICIAIS: Expande com ε-closure dos iniciais originais
 * 
 * ENTRADA:
 *   - memoInput contém texto do AFN-ε no formato:
 *     Alfabeto: a, b, ...
 *     Estados: q0, q1, ...
 *     Inicial: q0, ...
 *     Final: qf, ...
 *     Transições:
 *       q0 a q1
 *       q1 ε q2  (epsilon-transição)
 *       ...
 * 
 * SAÍDA:
 *   - NFAAlphabet, NFAStates, etc.: AFN sem epsilon
 *   - memoNFAOutput: Texto formatado do AFN
 *   - PaintBoxNFA: Diagrama renderizado
 *   - btnUseAsInput habilitado
 * 
 * COMPLEXIDADE:
 *   O(n² · m) onde n = estados, m = transições
 *   - n iterações para calcular ε-closure de cada estado
 *   - Cada ε-closure: O(n + m_epsilon)
 *   - Expansão de transições: O(n · m)
 * 
 * CRÍTICO: Gerenciamento de memória!
 * - Diversos TStringSet.Create são chamados
 * - TODOS precisam de correspondente .Free para evitar memory leak
 * - Usamos try-finally para garantir limpeza mesmo com erros
 ******************************************************************************}
procedure TFormMain.RemoveEpsilonTransitions;
var
  {** Variáveis para parsing do AFN-ε de entrada **}
  Alphabet, States, Initials, Finals: TStringList;  // Componentes do AFN-ε
  Transitions: array of TTransition;                 // Transições originais
  parts: TStringList;                                // Parser auxiliar
  i, j, k, m, tStart: Integer;                       // Contadores de loop
  line: string;                                      // Linha atual sendo processada
  
  {** Variáveis para construção do novo AFN **}
  newInitials, newFinals: TStringList;               // Novos conjuntos de estados
  initialClosure, stateClosure, tempClosure, destClosure: TStringSet;  // Epsilon-closures
  newTransitions: array of TTransition;              // Transições sem epsilon
  transCount: Integer;                               // Contador de novas transições
  state, symbol, targetState: string;                // Estados e símbolos temporários
  targetClosure: TStringSet;                         // Closure de estado destino
  hasEpsilon: Boolean;                               // Flag: autômato tem epsilon?
begin
  memoNFAOutput.Lines.Clear;
  
  {**
   * PASSO 0: Limpeza de dados anteriores
   * 
   * IMPORTANTE: Sempre verificar Assigned() antes de Free()!
   * Se tentarmos Free() em ponteiro não inicializado, teremos Access Violation.
   * 
   * Esta limpeza garante que não haverá memory leak se o método
   * for chamado múltiplas vezes (usuário clica no botão várias vezes).
   **}
  if Assigned(EpsilonNFAAlphabet) then EpsilonNFAAlphabet.Free;
  if Assigned(EpsilonNFAStates) then EpsilonNFAStates.Free;
  if Assigned(EpsilonNFAInitials) then EpsilonNFAInitials.Free;
  if Assigned(EpsilonNFAFinals) then EpsilonNFAFinals.Free;
  
  // Limpar dados anteriores do AFN (sem epsilon)
  if Assigned(NFAAlphabet) then NFAAlphabet.Free;
  if Assigned(NFAStates) then NFAStates.Free;
  if Assigned(NFAInitials) then NFAInitials.Free;
  if Assigned(NFAFinals) then NFAFinals.Free;
  
  {** Criar estruturas para armazenar AFN-ε (entrada) **}
  EpsilonNFAAlphabet := TStringList.Create;
  EpsilonNFAStates := TStringList.Create;
  EpsilonNFAInitials := TStringList.Create;
  EpsilonNFAFinals := TStringList.Create;
  
  {** Criar estruturas para armazenar AFN (saída sem epsilon) **}
  NFAAlphabet := TStringList.Create;
  NFAStates := TStringList.Create;
  NFAInitials := TStringList.Create;
  NFAFinals := TStringList.Create;
  
  {**
   * VALIDAÇÃO: Verificar formato mínimo da entrada
   * 
   * Formato esperado:
   *   Linha 0: Alfabeto (ex: a b ε)
   *   Linha 1: Estados (ex: q0 q1 q2)
   *   Linha 2: Estados iniciais (ex: q0)
   *   Linha 3: Estados finais (ex: q2)
   *   Linhas 4+: Transições (ex: q0 a q1)
   * 
   * Se não tiver ao menos 4 linhas, entrada é inválida.
   **}
  if memoInput.Lines.Count < 4 then
  begin
    ShowMessage('Entrada inválida! São necessárias pelo menos 4 linhas:' + LineEnding +
                '1. Alfabeto' + LineEnding +
                '2. Estados' + LineEnding +
                '3. Estados iniciais' + LineEnding +
                '4. Estados finais');
    Exit;
  end;
  
  Alphabet := TStringList.Create;
  States := TStringList.Create;
  Initials := TStringList.Create;
  Finals := TStringList.Create;
  parts := TStringList.Create;
  
  try
    parts.Delimiter := ' ';
    parts.StrictDelimiter := True;
    
    {**
     * PASSO 1: Parsing do ALFABETO (Linha 0)
     * 
     * Formato: "a b c ε" (símbolos separados por espaço)
     * 
     * IMPORTANTE:
     * - Alphabet: Contém TODOS os símbolos (incluindo epsilon)
     * - EpsilonNFAAlphabet: Cópia para o AFN-ε original
     * - NFAAlphabet: Contém APENAS símbolos não-epsilon (para o AFN resultante)
     * - hasEpsilon: Flag para avisar se não detectamos epsilon
     * 
     * SÍMBOLOS EPSILON RECONHECIDOS:
     * - 'ε' (Unicode U+03B5 - recomendado)
     * - 'epsilon' (palavra completa)
     * - 'e' (abreviação simples)
     * - '&' (notação alternativa da literatura)
     **}
    parts.DelimitedText := Trim(memoInput.Lines[0]);
    hasEpsilon := False;
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        Alphabet.Add(parts[i]);
        EpsilonNFAAlphabet.Add(parts[i]);
        
        // Detectar epsilon e marcá-lo (não será adicionado ao novo alfabeto)
        if (parts[i] = 'ε') or (parts[i] = 'epsilon') or 
           (parts[i] = 'e') or (parts[i] = '&') then
          hasEpsilon := True
        else
          NFAAlphabet.Add(parts[i]); // Adicionar ao AFN (sem epsilon)
      end;
    end;
    
    {**
     * PASSO 2: Parsing dos ESTADOS (Linha 1)
     * 
     * Formato: "q0 q1 q2 q3" (nomes separados por espaço)
     * 
     * Os estados são os mesmos no AFN-ε e no AFN resultante.
     * Apenas as transições mudam (epsilon é removido).
     **}
    parts.DelimitedText := Trim(memoInput.Lines[1]);
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        States.Add(parts[i]);
        EpsilonNFAStates.Add(parts[i]);
        NFAStates.Add(parts[i]);
      end;
    end;
    
    {**
     * PASSO 3: Parsing dos ESTADOS INICIAIS (Linha 2)
     * 
     * Formato: "q0 q1" (pode ter múltiplos em AFN)
     * 
     * NOTA: Estes são os iniciais do AFN-ε.
     * Os iniciais do AFN serão calculados depois usando epsilon-closure.
     **}
    parts.DelimitedText := Trim(memoInput.Lines[2]);
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        Initials.Add(parts[i]);
        EpsilonNFAInitials.Add(parts[i]);
      end;
    end;
    
    {**
     * PASSO 4: Parsing dos ESTADOS FINAIS (Linha 3)
     * 
     * Formato: "q2 q3" (pode ter múltiplos)
     * 
     * NOTA: Estes são os finais do AFN-ε.
     * Os finais do AFN serão calculados depois considerando epsilon-closure.
     **}
    parts.DelimitedText := Trim(memoInput.Lines[3]);
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        Finals.Add(parts[i]);
        EpsilonNFAFinals.Add(parts[i]);
      end;
    end;
    
    {**
     * PASSO 5: Parsing das TRANSIÇÕES (Linhas 4+)
     * 
     * Formato de cada linha: "q0 a q1" (origem símbolo destino)
     * 
     * Podem ser:
     * - Transições normais: q0 a q1
     * - Epsilon-transições: q0 ε q1 (ou epsilon, e, &)
     * 
     * SetLength aloca memória para o array dinâmico.
     * Tamanho = número de linhas restantes (pode ter linhas vazias).
     * 
     * IMPORTANTE:
     * - Transitions: Array local para processamento
     * - EpsilonNFATransitions: Campo da classe para armazenar original
     **}
    tStart := 4;  // Primeira linha de transição
    SetLength(Transitions, Max(0, memoInput.Lines.Count - tStart));
    SetLength(EpsilonNFATransitions, Max(0, memoInput.Lines.Count - tStart));
    for i := tStart to memoInput.Lines.Count - 1 do
    begin
      line := Trim(memoInput.Lines[i]);
      if line = '' then Continue;  // Ignorar linhas vazias
      
      parts.DelimitedText := line;
      if parts.Count >= 3 then  // Precisa ter origem, símbolo e destino
      begin
        Transitions[i - tStart].FromState := parts[0];
        Transitions[i - tStart].Symbol := parts[1];
        Transitions[i - tStart].ToState := parts[2];
        
        // Guardar cópia no campo da classe
        EpsilonNFATransitions[i - tStart].FromState := parts[0];
        EpsilonNFATransitions[i - tStart].Symbol := parts[1];
        EpsilonNFATransitions[i - tStart].ToState := parts[2];
      end;
    end;
    
    {**
     * LOGGING: Imprimir AFN-ε de entrada no console
     * 
     * WriteLn escreve no console de debug (visível ao compilar no IDE).
     * Útil para debugging e para o professor entender o processamento.
     **}
    WriteLn('-------------------------------------------');
    WriteLn('AFN-EPSILON DE ENTRADA:');
    WriteLn('  Alfabeto: ', Alphabet.CommaText);
    WriteLn('  Estados: ', States.CommaText);
    WriteLn('  Iniciais: ', Initials.CommaText);
    WriteLn('  Finais: ', Finals.CommaText);
    WriteLn('  Transicoes: ', Length(Transitions), ' transicoes');
    for i := 0 to High(Transitions) do
      WriteLn('    ', Transitions[i].FromState, ' --', Transitions[i].Symbol, '--> ', Transitions[i].ToState);
    WriteLn('-------------------------------------------');
    WriteLn('');
    
    {**
     * Aviso se não detectamos epsilon-transições
     * 
     * Se hasEpsilon = False, significa que o alfabeto não contém
     * nenhum símbolo epsilon reconhecido.
     * 
     * Mesmo assim, continuamos com a conversão (pode ser AFN comum).
     **}
    if not hasEpsilon then
    begin
      ShowMessage('Aviso: O autômato não possui epsilon-transições!' + LineEnding +
                  'Use símbolos: ε, epsilon, e, ou &');
      // Mesmo assim, continuar com a conversão
    end;
    
    {**
     * PASSO 6: Calcular NOVOS ESTADOS INICIAIS
     * 
     * TEORIA:
     * Os estados iniciais do AFN são os estados alcançáveis a partir
     * dos estados iniciais do AFN-ε usando APENAS epsilon-transições.
     * 
     * Ou seja: NFAInitials = ε-closure(EpsilonNFAInitials)
     * 
     * EXEMPLO:
     *   AFN-ε: Inicial = (q0), q0 --ε--> q1
     *   AFN: Iniciais = (q0, q1)
     * 
     * GERENCIAMENTO DE MEMÓRIA CRÍTICO:
     * - Criamos TStringSet temporário para passar ao ComputeEpsilonClosure
     * - ComputeEpsilonClosure retorna NOVO TStringSet (initialClosure)
     * - Ambos PRECISAM de Free() para evitar memory leak
     * - Usamos try-finally para garantir limpeza mesmo se houver exceção
     **}
    newInitials := TStringList.Create;
    tempClosure := TStringSet.Create;
    try
      // Adicionar todos os estados iniciais ao conjunto temporário
      for i := 0 to Initials.Count - 1 do
        tempClosure.Add(Initials[i]);
      
      // Calcular epsilon-closure dos iniciais
      initialClosure := ComputeEpsilonClosure(tempClosure, Transitions);
      tempClosure.Free;  // Já não precisamos mais
      
      try
        // Copiar resultado para newInitials e NFAInitials
        for i := 0 to initialClosure.Count - 1 do
        begin
          newInitials.Add(initialClosure.Item(i));
          NFAInitials.Add(initialClosure.Item(i));
        end;
        
        WriteLn('NOVOS ESTADOS INICIAIS (com epsilon-fecho): ', newInitials.CommaText);
      finally
        initialClosure.Free;  // IMPORTANTE: Liberar memória do closure!
      end;
    except
      tempClosure.Free;  // Se houve exceção antes do primeiro Free
      raise;  // Re-lançar exceção
    end;
    
    {**
     * PASSO 7: Calcular NOVOS ESTADOS FINAIS
     * 
     * TEORIA:
     * Um estado q é final no AFN se seu epsilon-closure contém
     * algum estado final do AFN-ε original.
     * 
     * Em outras palavras:
     *   q ∈ NFAFinals  ⇔  ε-closure(q) ∩ EpsilonNFAFinals ≠ ∅
     * 
     * EXEMPLO:
     *   AFN-ε: Final = (q2), q1 --ε--> q2
     *   AFN: Finais = (q1, q2)  (q1 também é final pois alcanca q2 por ε)
     * 
     * ALGORITMO:
     * Para cada estado q:
     *   1. Calcular ε-closure(q)
     *   2. Se o closure contém algum estado final original:
     *      - Marcar q como final no AFN
     * 
     * GERENCIAMENTO DE MEMÓRIA:
     * - Para CADA estado, criamos 2 TStringSet (tempClosure e stateClosure)
     * - Ambos precisam de Free() a cada iteração do loop
     * - try-finally garante limpeza mesmo com exceção
     **}
    newFinals := TStringList.Create;
    try
      // Para cada estado do autômato
      for i := 0 to States.Count - 1 do
      begin
        state := States[i];
        
        // Calcular epsilon-closure deste estado
        tempClosure := TStringSet.Create;
        tempClosure.Add(state);
        stateClosure := ComputeEpsilonClosure(tempClosure, Transitions);
        tempClosure.Free;  // Já podemos liberar
        
        try
          {**
           * Verificar se o epsilon-closure contém algum estado final
           * 
           * Se contém, o estado atual é final no AFN.
           * IndexOf < 0 evita duplicatas na lista.
           **}
          for j := 0 to Finals.Count - 1 do
          begin
            if stateClosure.Contains(Finals[j]) then
            begin
              if newFinals.IndexOf(state) < 0 then
              begin
                newFinals.Add(state);
                NFAFinals.Add(state);
              end;
              Break;  // Já encontramos um final, não precisa continuar
            end;
          end;
        finally
          stateClosure.Free;  // IMPORTANTE: Liberar a cada iteração!
        end;
      end;
      
      WriteLn('NOVOS ESTADOS FINAIS: ', newFinals.CommaText);
    finally
      // newFinals será liberado no finally externo
    end;
    
    {******************************************************************************
     * PASSO 8: Construir NOVAS TRANSIÇÕES (sem epsilon)
     * 
     * Esta é a parte MAIS COMPLEXA do algoritmo!
     * 
     * TEORIA:
     * Para cada transição p --a--> q no AFN-ε (onde 'a' NÃO é epsilon):
     *   Para cada estado r alcançável de q por epsilon-transições:
     *     Adicionar transição p --a--> r no AFN
     * 
     * EXEMPLO DETALHADO:
     *   AFN-ε tem:
     *     q0 --a--> q1
     *     q1 --ε--> q2
     *   
     *   Processamento:
     *     1. Estado q0, símbolo 'a'
     *     2. ε-closure(q0) = (q0) (nenhum epsilon saindo)
     *     3. De q0 com 'a' vamos para q1
     *     4. ε-closure(q1) = (q1, q2) (tem ε-transição para q2)
     *     5. Adicionar ao AFN:
     *        - q0 --a--> q1
     *        - q0 --a--> q2  (NOVA! expandida via epsilon)
     * 
     * ALGORITMO DETALHADO (3 loops aninhados):
     * Para cada estado p do autômato:
     *   1. Calcular ε-closure(p) = conjunto de estados alcançáveis de p por ε
     *   
     *   Para cada símbolo a do alfabeto (excluindo epsilon):
     *     2. Inicializar targetClosure = () (estados destino finais)
     *     
     *     Para cada estado q em ε-closure(p):
     *       3. Procurar transições q --a--> r
     *       4. Para cada r encontrado:
     *          - Calcular ε-closure(r)
     *          - Adicionar todos ao targetClosure
     *     
     *     Para cada estado t em targetClosure:
     *       5. Adicionar transição p --a--> t ao AFN
     * 
     * EXEMPLO DE EXECUÇÃO:
     * Estado: q0, Símbolo: a
     *   ε-closure(q0) = (q0, q1)
     *   
     *   De q0 com 'a':
     *     q0 --a--> q2  →  ε-closure(q2) = (q2, q3)
     *   
     *   De q1 com 'a':
     *     q1 --a--> q4  →  ε-closure(q4) = (q4)
     *   
     *   targetClosure = (q2, q3, q4)
     *   
     *   Adicionar ao AFN:
     *     q0 --a--> q2
     *     q0 --a--> q3
     *     q0 --a--> q4
     * 
     * COMPLEXIDADE:
     *   O(n² · |Σ| · m) no pior caso
     *   - n estados
     *   - |Σ| símbolos do alfabeto
     *   - m transições por estado
     * 
     * GERENCIAMENTO DE MEMÓRIA EXTREMAMENTE CRÍTICO:
     * - stateClosure: Um por iteração externa (liberado no finally externo)
     * - targetClosure: Um por (estado, símbolo) (liberado no finally interno)
     * - tempClosure e destClosure: Criados MUITAS vezes (loops aninhados)
     * - TODOS precisam de Free() para evitar memory leak massivo!
     ******************************************************************************}
    SetLength(newTransitions, States.Count * NFAAlphabet.Count * States.Count);
    transCount := 0;
    
    // LOOP 1: Para cada estado do autômato
    for i := 0 to States.Count - 1 do
    begin
      state := States[i];
      
      {**
       * Calcular epsilon-closure do estado atual
       * 
       * Este conjunto contém todos os estados que podemos alcançar
       * de 'state' usando apenas epsilon-transições.
       * 
       * GERENCIAMENTO DE MEMÓRIA:
       * - Criamos stateClosure temporário para o cálculo
       * - ComputeEpsilonClosure retorna NOVO objeto (tempClosure)
       * - Liberamos o temporário e guardamos o resultado
       * - stateClosure será liberado no finally no final do loop
       **}
      stateClosure := TStringSet.Create;
      stateClosure.Add(state);
      tempClosure := ComputeEpsilonClosure(stateClosure, Transitions);
      stateClosure.Free;  // Liberar o temporário
      stateClosure := tempClosure;  // Guardar o resultado do closure
      
      try
        // LOOP 2: Para cada símbolo do alfabeto (sem epsilon)
        for j := 0 to NFAAlphabet.Count - 1 do
        begin
          symbol := NFAAlphabet[j];
          
          {**
           * targetClosure: Conjunto de TODOS os estados alcançáveis
           * de 'state' consumindo 'symbol' e considerando epsilon-transições
           * 
           * Este conjunto será construído iterativamente no próximo loop.
           **}
          targetClosure := TStringSet.Create;
          try
            {**
             * LOOP 3: Para cada estado no epsilon-closure do estado atual
             * 
             * Lembre-se: stateClosure contém todos os estados alcançáveis
             * de 'state' por epsilon. Precisamos verificar transições
             * com 'symbol' de CADA um desses estados.
             **}
            for k := 0 to stateClosure.Count - 1 do
            begin
              targetState := stateClosure.Item(k);
              
              {**
               * LOOP 4: Procurar TODAS as transições de targetState com 'symbol'
               * 
               * NOTA: Reutilizamos variável 'tStart' (antes usada para parsing).
               * Este loop percorre TODO o array de transições do AFN-ε.
               * 
               * Para cada transição encontrada targetState --symbol--> destino:
               *   1. Calcular ε-closure(destino)
               *   2. Adicionar todos os estados do closure ao targetClosure
               **}
              for tStart := 0 to High(Transitions) do
              begin
                if (Transitions[tStart].FromState = targetState) and 
                   (Transitions[tStart].Symbol = symbol) then
                begin
                  {**
                   * Encontramos transição: targetState --symbol--> ToState
                   * 
                   * Agora precisamos adicionar ε-closure(ToState) ao resultado.
                   * 
                   * GERENCIAMENTO DE MEMÓRIA - NESTED try-finally:
                   * - tempClosure: conjunto temporário com apenas ToState
                   * - destClosure: resultado do ComputeEpsilonClosure
                   * - Ambos PRECISAM de Free() - loops aninhados criam MUITOS objetos!
                   * - Se não liberarmos, haverá MEMORY LEAK MASSIVO
                   **}
                  tempClosure := TStringSet.Create;
                  tempClosure.Add(Transitions[tStart].ToState);
                  try
                    destClosure := ComputeEpsilonClosure(tempClosure, Transitions);
                    try
                      // Adicionar todos os estados do closure ao targetClosure
                      for m := 0 to destClosure.Count - 1 do
                        targetClosure.Add(destClosure.Item(m));
                    finally
                      destClosure.Free;  // CRÍTICO: Liberar destClosure!
                    end;
                  finally
                    tempClosure.Free;  // CRÍTICO: Liberar tempClosure!
                  end;
                end;
              end;
            end;
            
            {**
             * Agora targetClosure contém TODOS os estados alcançáveis
             * de 'state' consumindo 'symbol' (considerando epsilon-transições).
             * 
             * Criar uma transição state --symbol--> t para cada t em targetClosure.
             * 
             * NOTA: Se targetClosure está vazio, não adicionamos nenhuma transição
             * (estado morto - não há transição com esse símbolo).
             **}
            for k := 0 to targetClosure.Count - 1 do
            begin
              newTransitions[transCount].FromState := state;
              newTransitions[transCount].Symbol := symbol;
              newTransitions[transCount].ToState := targetClosure.Item(k);
              Inc(transCount);  // Incrementar contador de transições
            end;
          finally
            targetClosure.Free;  // IMPORTANTE: Liberar targetClosure!
          end;
        end;  // Fim loop símbolos
      finally
        stateClosure.Free;  // IMPORTANTE: Liberar stateClosure!
      end;
    end;  // Fim loop estados
    
    {**
     * Ajustar tamanho do array para o número real de transições
     * 
     * Alocamos espaço máximo (n * |Σ| * n) mas geralmente usamos menos.
     * SetLength ajusta para o tamanho exato (transCount).
     * 
     * Copiar para NFATransitions (campo da classe) para uso posterior.
     **}
    SetLength(newTransitions, transCount);
    SetLength(NFATransitions, transCount);
    for i := 0 to transCount - 1 do
      NFATransitions[i] := newTransitions[i];
    
    {**
     * LOGGING: Imprimir AFN resultante no console
     * 
     * Útil para debugging e para o professor ver o processamento passo-a-passo.
     **}
    WriteLn('-------------------------------------------');
    WriteLn('AFN RESULTANTE (SEM EPSILON):');
    WriteLn('  Alfabeto: ', NFAAlphabet.CommaText);
    WriteLn('  Estados: ', NFAStates.CommaText);
    WriteLn('  Iniciais: ', NFAInitials.CommaText);
    WriteLn('  Finais: ', NFAFinals.CommaText);
    WriteLn('  Transicoes: ', Length(NFATransitions), ' transicoes');
    for i := 0 to High(NFATransitions) do
      WriteLn('    ', NFATransitions[i].FromState, ' --', NFATransitions[i].Symbol, '--> ', NFATransitions[i].ToState);
    WriteLn('-------------------------------------------');
    WriteLn('');
    
    {**
     * Exibir resultado formatado no memo de saída (interface do usuário)
     * 
     * O usuário vê este texto na aba "AFN sem epsilon" da interface.
     * Formato limpo e organizado com emojis e separadores.
     **}
    memoNFAOutput.Lines.Add('=== AFN SEM EPSILON ===');
    memoNFAOutput.Lines.Add('');
    memoNFAOutput.Lines.Add('Alfabeto: ' + NFAAlphabet.CommaText);
    memoNFAOutput.Lines.Add('Estados: ' + NFAStates.CommaText);
    memoNFAOutput.Lines.Add('Estados Iniciais: ' + NFAInitials.CommaText);
    memoNFAOutput.Lines.Add('Estados Finais: ' + NFAFinals.CommaText);
    memoNFAOutput.Lines.Add('');
    memoNFAOutput.Lines.Add('Transições (' + IntToStr(Length(NFATransitions)) + '):');
    for i := 0 to High(NFATransitions) do
      memoNFAOutput.Lines.Add('  ' + NFATransitions[i].FromState + ' --' + 
                             NFATransitions[i].Symbol + '--> ' + 
                             NFATransitions[i].ToState);
    
    {**
     * Atualizar interface gráfica
     * 
     * - PageControl2.ActivePage: Muda para aba de texto "AFN sem epsilon"
     * - PageControl1.ActivePage: Muda para aba de diagrama "AFN"
     * - btnUseAsInput.Enabled: Habilita botão para copiar AFN para entrada
     * - PaintBox.Invalidate: Força redesenho dos diagramas
     **}
    PageControl2.ActivePage := TabNFAOutput;
    PageControl1.ActivePage := TabNFA;
    
    // Habilitar botão "Usar como Input" para permitir workflow encadeado
    btnUseAsInput.Enabled := True;
    
    // Forçar redesenho dos diagramas (AFN-ε e AFN)
    PaintBoxEpsilonNFA.Invalidate;
    PaintBoxNFA.Invalidate;
    
  finally
    {**
     * LIMPEZA FINAL: Liberar TODAS as estruturas temporárias
     * 
     * Este bloco finally garante que a limpeza aconteça SEMPRE,
     * mesmo se houver exceção durante o processamento.
     * 
     * CRÍTICO: Evita memory leak de todas as estruturas auxiliares.
     * 
     * NOTA: NFAAlphabet, NFAStates, etc. NÃO são liberados aqui
     * pois são campos da classe (serão usados em outras conversões).
     **}
    Alphabet.Free;
    States.Free;
    Initials.Free;
    Finals.Free;
    parts.Free;
    newInitials.Free;
    newFinals.Free;
  end;
end;

{******************************************************************************
 * ConvertAFNtoAFD - Converte AFN em AFD usando Construção de Subconjuntos
 * 
 * Este método implementa o algoritmo clássico de SUBSET CONSTRUCTION
 * (Construção de Subconjuntos) para converter um Autômato Finito
 * Não-Determinístico (AFN) em um Autômato Finito Determinístico (AFD).
 * 
 * DIFERENÇA FUNDAMENTAL AFN vs AFD:
 * - AFN: De um estado com um símbolo, pode ir para MÚLTIPLOS estados
 *        Exemplo: q0 --a--> (q1, q2)
 * - AFD: De um estado com um símbolo, vai para EXATAMENTE UM estado
 *        Exemplo: q0 --a--> q1
 * 
 * IDEIA DO ALGORITMO:
 * Cada estado do AFD representa um CONJUNTO de estados do AFN.
 * Exemplo:
 *   AFN: q0 --a--> q1  e  q0 --a--> q2
 *   AFD: (q0) --a--> (q1,q2)
 * 
 * ALGORITMO COMPLETO (BFS - Busca em Largura):
 * 1. Estado inicial do AFD = conjunto dos estados iniciais do AFN
 * 2. Adicionar à fila de processamento (workQ)
 * 3. Enquanto fila não está vazia:
 *    a) Remove conjunto da fila (curSet)
 *    b) Para cada símbolo do alfabeto:
 *       - Calcula nextSet = todos estados alcançáveis de curSet com símbolo
 *       - Cria transição curSet --símbolo--> nextSet no AFD
 *       - Se nextSet é novo, adiciona à fila
 *    c) Se curSet contém algum estado final do AFN, marca como final no AFD
 * 
 * EXEMPLO EXECUÇÃO:
 *   AFN: q0 --a--> q1, q0 --a--> q2, q1 --b--> q3, q2 --b--> q3
 *   
 *   Passo 1: workQ = [(q0)], DFA = ()
 *   Passo 2: Processa (q0):
 *     - Com 'a': (q0) --a--> (q1,q2)  (NOVO! adiciona à fila)
 *     - Com 'b': (q0) --b--> ()  (conjunto vazio, sem transição)
 *   Passo 3: Processa (q1,q2):
 *     - Com 'a': (q1,q2) --a--> ()
 *     - Com 'b': (q1,q2) --b--> (q3)  (NOVO! adiciona à fila)
 *   Passo 4: Processa (q3):
 *     - Com 'a': (q3) --a--> ()
 *     - Com 'b': (q3) --b--> ()
 *   Passo 5: Fila vazia - FIM!
 * 
 * EXPLOSÃO DE ESTADOS:
 * No pior caso, o AFD pode ter 2^n estados (onde n = estados do AFN).
 * Exemplo: AFN com 10 estados → AFD com até 1024 estados!
 * 
 * ENTRADA:
 *   - memoInput: Texto do AFN (alfabeto, estados, iniciais, finais, transições)
 * 
 * SAÍDA:
 *   - DFAStates, DFAInitial, DFAFinals, DFATransitions: AFD completo
 *   - memoOutput: Texto formatado do AFD
 *   - PaintBoxDFA: Diagrama renderizado
 * 
 * COMPLEXIDADE:
 *   O(2^n · |Σ|) no pior caso
 *   - 2^n: Número máximo de estados do AFD
 *   - |Σ|: Tamanho do alfabeto
 * 
 * IMPORTANTE:
 * - Este método ASSUME que não há epsilon-transições!
 * - Se houver epsilon, mostra aviso e continua (mas resultado pode estar errado)
 * - Use RemoveEpsilonTransitions primeiro se o AFN tiver epsilon
 ******************************************************************************}
procedure TFormMain.ConvertAFNtoAFD;
var
  {** Variáveis para parsing do AFN de entrada **}
  Alphabet, States, Initials, Finals: TStringList;
  Transitions: array of TTransition;
  parts: TStringList;
  i, j, k, tStart: Integer;
  
  {** Variáveis para construção do AFD **}
  localDFAStates, workQ, dfaMap, isFinal: TStringList;
  curSet, nextSet, cloneSet: TStringSet;
  sym, key: string;
  localDFATransitions: TTransitionArray;
  hasFinal, hasEpsilon: Boolean;
  line: string;
begin
  memoOutput.Lines.Clear;
  
  {**
   * PASSO 0: Limpeza de dados anteriores (mesmo padrão de RemoveEpsilonTransitions)
   * 
   * Sempre verificar Assigned() antes de Free() para evitar Access Violation.
   **}
  if Assigned(NFAAlphabet) then NFAAlphabet.Free;
  if Assigned(NFAStates) then NFAStates.Free;
  if Assigned(NFAInitials) then NFAInitials.Free;
  if Assigned(NFAFinals) then NFAFinals.Free;
  if Assigned(DFAStates) then DFAStates.Free;
  if Assigned(DFAFinals) then DFAFinals.Free;
  
  {** Criar estruturas para armazenar AFN e AFD **}
  NFAAlphabet := TStringList.Create;
  NFAStates := TStringList.Create;
  NFAInitials := TStringList.Create;
  NFAFinals := TStringList.Create;
  DFAStates := TStringList.Create;
  DFAFinals := TStringList.Create;
  
  {** Validação de entrada (mesmo formato do RemoveEpsilonTransitions) **}
  if memoInput.Lines.Count < 4 then
  begin
    ShowMessage('Entrada inválida! São necessárias pelo menos 4 linhas:' + LineEnding +
                '1. Alfabeto' + LineEnding +
                '2. Estados' + LineEnding +
                '3. Estados iniciais' + LineEnding +
                '4. Estados finais');
    Exit;
  end;
  
  Alphabet := TStringList.Create;
  States := TStringList.Create;
  Initials := TStringList.Create;
  Finals := TStringList.Create;
  parts := TStringList.Create;
  
  try
    {**
     * Configurar parser de strings delimitadas
     * 
     * Delimiter = ' ' → tokens separados por espaço
     * StrictDelimiter = True → ignora espaços múltiplos, tabs
     * 
     * Isso permite parsing robusto mesmo com formatação inconsistente.
     **}
    parts.Delimiter := ' ';
    parts.StrictDelimiter := True;
    
    {**
     * PASSO 1: Ler ALFABETO (Linha 0)
     * 
     * FORMATO: símbolo1 símbolo2 ... símboloN
     * EXEMPLO: a b c
     * 
     * IMPORTANTE: AFD NÃO pode ter epsilon-transições!
     * Portanto, filtramos epsilon do alfabeto (será tratado como aviso).
     * 
     * SÍMBOLOS EPSILON RECONHECIDOS:
     *   - 'ε' (Unicode U+03B5)
     *   - 'epsilon' (palavra completa)
     *   - 'e' (abreviação comum)
     *   - '&' (notação alternativa)
     * 
     * Se epsilon for detectado, será tratado nas transições (gera aviso).
     **}
    parts.DelimitedText := Trim(memoInput.Lines[0]);
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        {** Filtrar epsilon do alfabeto - AFD não pode ter epsilon **}
        if (parts[i] <> 'ε') and (parts[i] <> 'epsilon') and 
           (parts[i] <> 'e') and (parts[i] <> '&') then
        begin
          Alphabet.Add(parts[i]);
          NFAAlphabet.Add(parts[i]);  // Guardar em campo da classe
        end;
      end;
    end;
    
    {**
     * PASSO 2: Ler ESTADOS (Linha 1)
     * 
     * FORMATO: estado1 estado2 ... estadoN
     * EXEMPLO: q0 q1 q2 q3
     * 
     * Cada estado é identificador único (string).
     * Duplicados são permitidos no arquivo (ignorados pelo Add).
     **}
    parts.DelimitedText := Trim(memoInput.Lines[1]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then 
      begin
        States.Add(parts[i]);
        NFAStates.Add(parts[i]);  // Campo da classe
      end;
    
    {**
     * PASSO 3: Ler ESTADOS INICIAIS (Linha 2)
     * 
     * FORMATO: inicial1 inicial2 ... inicialN
     * EXEMPLO: q0 (AFD tem apenas 1, AFN pode ter múltiplos)
     * 
     * Para subset construction:
     *   - Se AFN tem múltiplos iniciais: estado inicial do AFD = conjunto deles
     *   - Se AFN tem 1 inicial: estado inicial do AFD = conjunto com esse estado
     **}
    parts.DelimitedText := Trim(memoInput.Lines[2]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then 
      begin
        Initials.Add(parts[i]);
        NFAInitials.Add(parts[i]);  // Campo da classe
      end;
    
    {**
     * PASSO 4: Ler ESTADOS FINAIS (Linha 3)
     * 
     * FORMATO: final1 final2 ... finalN
     * EXEMPLO: q3 q5 q7
     * 
     * Um estado do AFD é final SE E SOMENTE SE contém
     * pelo menos um estado final do AFN.
     **}
    parts.DelimitedText := Trim(memoInput.Lines[3]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then 
      begin
        Finals.Add(parts[i]);
        NFAFinals.Add(parts[i]);  // Campo da classe
      end;
    
    {**
     * PASSO 5: Ler TRANSIÇÕES (Linhas 4+)
     * 
     * FORMATO: origem símbolo destino
     * EXEMPLO: q0 a q1
     * 
     * Cada linha representa uma transição do AFN.
     * AFN pode ter múltiplas transições (q0, a) → q1 E (q0, a) → q2.
     * 
     * DETECÇÃO DE EPSILON-TRANSIÇÕES:
     * Se houver epsilon, gerar AVISO pois AFD não pode ter epsilon.
     * O usuário deveria usar primeiro "AFN-ε → AFN" para remover.
     * 
     * VALIDAÇÃO:
     * - Linhas vazias são ignoradas (Continue)
     * - Linhas com menos de 3 tokens são ignoradas
     * - Formato esperado: origem símbolo destino
     **}
    tStart := 4;  // Primeira linha de transição
    SetLength(Transitions, Max(0, memoInput.Lines.Count - tStart));
    SetLength(NFATransitions, Max(0, memoInput.Lines.Count - tStart));
    
    {** Flag para detectar presença de epsilon-transições **}
    hasEpsilon := False;
    
    for i := tStart to memoInput.Lines.Count - 1 do
    begin
      line := Trim(memoInput.Lines[i]);
      if line = '' then Continue;  // Ignorar linhas vazias
      
      parts.DelimitedText := line;
      if parts.Count >= 3 then
      begin
        {**
         * Verificar se é epsilon-transição
         * 
         * IMPORTANTE: AFD NÃO pode ter epsilon!
         * Se detectado, marcar flag para gerar aviso ao usuário.
         * 
         * O algoritmo ainda processará (para não crashar),
         * mas o resultado estará INCORRETO se houver epsilon.
         **}
        if (parts[1] = 'ε') or (parts[1] = 'epsilon') or 
           (parts[1] = 'e') or (parts[1] = '&') then
        begin
          hasEpsilon := True;  // Marcar para aviso posterior
        end;
        
        {** Armazenar transição (origem, símbolo, destino) **}
        Transitions[i - tStart].FromState := parts[0];
        Transitions[i - tStart].Symbol := parts[1];
        Transitions[i - tStart].ToState := parts[2];
        
        {** Também armazenar em campo da classe (para uso posterior) **}
        NFATransitions[i - tStart].FromState := parts[0];
        NFATransitions[i - tStart].Symbol := parts[1];
        NFATransitions[i - tStart].ToState := parts[2];
      end;
    end;
    
    {**
     * AVISO CRÍTICO: Epsilon detectado!
     * 
     * PROBLEMA: AFD não pode ter epsilon-transições por definição.
     * Um AFD é DETERMINÍSTICO: de cada estado com cada símbolo,
     * há EXATAMENTE UMA transição (ou nenhuma).
     * 
     * Epsilon-transições permitem transições "sem consumir símbolo",
     * o que viola o determinismo.
     * 
     * SOLUÇÃO CORRETA:
     *   1. Remover epsilon primeiro (AFN-ε → AFN)
     *   2. Usar resultado como entrada
     *   3. Então converter AFN → AFD
     * 
     * COMPORTAMENTO: Se usuário continuar, algoritmo processará
     * mas resultado estará INCORRETO (transições epsilon serão
     * tratadas como símbolos normais).
     **}
    if hasEpsilon then
    begin
      WriteLn('[AVISO] AFN contém epsilon-transições!');
      WriteLn('[AVISO] Use "AFN-ε → AFN" primeiro para remover epsilon.');
      ShowMessage('ATENÇÃO: O autômato contém epsilon-transições!' + LineEnding + LineEnding +
                  'Para conversão correta:' + LineEnding +
                  '1. Clique em "🔀 AFN-ε → AFN" para remover epsilon' + LineEnding +
                  '2. Clique em "📥 Usar como Input"' + LineEnding +
                  '3. Depois clique em "🔄 AFN → AFD"' + LineEnding + LineEnding +
                  'Continuar com epsilon pode gerar um AFD incorreto!');
    end;
    
    {**
     * LOGGING: Imprimir AFN de entrada no console
     * 
     * Útil para debugging e para o professor ver o estado
     * do autômato antes da conversão.
     * 
     * Formato:
     *   - Alfabeto: lista separada por vírgulas
     *   - Estados: lista separada por vírgulas
     *   - Iniciais: lista separada por vírgulas
     *   - Finais: lista separada por vírgulas
     *   - Transições: uma por linha (origem --símbolo--> destino)
     **}
    WriteLn('-------------------------------------------');
    WriteLn('AFN DE ENTRADA:');
    WriteLn('  Alfabeto: ', Alphabet.CommaText);
    WriteLn('  Estados: ', States.CommaText);
    WriteLn('  Iniciais: ', Initials.CommaText);
    WriteLn('  Finais: ', Finals.CommaText);
    WriteLn('  Transicoes: ', Length(Transitions), ' transicoes');
    for i := 0 to High(Transitions) do
      WriteLn('    ', Transitions[i].FromState, ' --', Transitions[i].Symbol, '--> ', Transitions[i].ToState);
    WriteLn('-------------------------------------------');
    WriteLn('');
    
    {**
     * PASSO 6: INICIALIZAÇÃO DO SUBSET CONSTRUCTION
     * 
     * ESTRUTURAS DE DADOS PRINCIPAIS:
     * 
     * localDFAStates: TStringList
     *   - Lista de NOMES dos estados do AFD
     *   - Cada nome representa um conjunto de estados do AFN
     *   - Exemplo: "(q0,q1)", "(q2)", "(q3,q4,q5)"
     * 
     * workQ: TStringList (Fila de trabalho)
     *   - Estados do AFD que ainda precisam ser processados
     *   - Algoritmo BFS: Remove do início, adiciona no fim
     *   - Quando vazia: todos estados processados → FIM
     * 
     * dfaMap: TStringList com Objects
     *   - Mapeamento: Nome → Conjunto real (TStringSet)
     *   - Exemplo: "(q0,q1)" → TStringSet com (q0, q1)
     *   - OwnsObjects = True → Free automático dos TStringSet
     * 
     * isFinal: TStringList
     *   - Nomes dos estados do AFD que são finais
     *   - Um estado AFD é final SE contém algum estado final do AFN
     * 
     * localDFATransitions: TTransitionArray
     *   - Array dinâmico de transições do AFD
     *   - Cresce conforme novas transições são descobertas
     **}
    localDFAStates := TStringList.Create;
    workQ := TStringList.Create;
    dfaMap := TStringList.Create;
    dfaMap.OwnsObjects := True;  // Libera TStringSets automaticamente
    isFinal := TStringList.Create;
    
    try
      {**
       * ESTADO INICIAL DO AFD
       * 
       * REGRA: Estado inicial do AFD = CONJUNTO dos iniciais do AFN
       * 
       * EXEMPLO:
       *   Se AFN tem iniciais (q0, q1)
       *   → Estado inicial do AFD = (q0,q1)
       * 
       * PASSOS:
       *   1. Criar TStringSet com todos os iniciais do AFN
       *   2. Adicionar à lista de estados do AFD
       *   3. Adicionar à fila de trabalho (precisa ser processado)
       *   4. Registrar no mapa (nome → conjunto)
       **}
      curSet := TStringSet.Create;
      for i := 0 to Initials.Count - 1 do
        curSet.Add(Initials[i]);
      
      {** Registrar estado inicial em todas as estruturas **}
      localDFAStates.Add(GetSetName(curSet));  // Lista de estados
      workQ.Add(GetSetName(curSet));           // Fila de trabalho
      dfaMap.AddObject(GetSetName(curSet), curSet);  // Mapa nome→conjunto
      
      {** Inicializar array de transições vazio **}
      SetLength(localDFATransitions, 0);
      
      {**
       * PASSO 7: LOOP PRINCIPAL (BFS - Busca em Largura)
       * 
       * ALGORITMO:
       *   While fila não está vazia:
       *     1. Remove estado da fila (FIFO)
       *     2. Processa esse estado (gera transições)
       *     3. Novos estados descobertos vão para o fim da fila
       *   End
       * 
       * INVARIANTE:
       *   - Todos estados em localDFAStates foram ou serão processados
       *   - Todos estados processados geraram suas transições
       *   - Quando fila vazia: AFD completo está construído
       * 
       * TERMINAÇÃO:
       *   - Número de estados do AFD é finito (≤ 2^n)
       *   - Cada estado só é adicionado uma vez à fila
       *   - Logo, fila eventualmente esvazia
       **}
      while workQ.Count > 0 do
      begin
        {**
         * Remover próximo estado da fila (BFS)
         * 
         * workQ[0] = primeiro da fila (FIFO)
         * Delete(0) = remove do início
         * 
         * Recuperar conjunto real do mapa.
         **}
        key := workQ[0];
        workQ.Delete(0);
        curSet := TStringSet(dfaMap.Objects[dfaMap.IndexOf(key)]);
        
        {**
         * DETERMINAR SE É ESTADO FINAL
         * 
         * REGRA: Estado do AFD é final ⟺ contém algum estado final do AFN
         * 
         * JUSTIFICATIVA:
         *   Se curSet = (q0, q1, q2) e q2 é final no AFN,
         *   então existe caminho que leva a q2 (estado final),
         *   logo o conjunto todo deve ser final no AFD.
         * 
         * EXEMPLO:
         *   AFN: finais = (q3, q5)
         *   Estado AFD (q1, q3, q4) → FINAL (contém q3)
         *   Estado AFD (q0, q2) → NÃO-FINAL (não contém q3 nem q5)
         **}
        hasFinal := False;
        for i := 0 to Finals.Count - 1 do
          if curSet.Contains(Finals[i]) then hasFinal := True;
        if hasFinal then isFinal.Add(key);
        
        {**
         * GERAR TRANSIÇÕES PARA CADA SÍMBOLO
         * 
         * Para o estado atual curSet e cada símbolo do alfabeto,
         * calcular para onde o AFD vai:
         * 
         * ALGORITMO:
         *   Para cada símbolo a:
         *     nextSet = conjunto vazio
         *     Para cada estado q em curSet:
         *       Para cada transição (q, a, r) no AFN:
         *         Adicionar r ao nextSet
         *     Criar transição do AFD: curSet --a--> nextSet
         * 
         * EXEMPLO:
         *   curSet = (q0, q1), símbolo = 'a'
         *   AFN tem:
         *     q0 --a--> q2
         *     q0 --a--> q3
         *     q1 --a--> q4
         *   
         *   nextSet = (q2, q3, q4)
         *   Transição AFD: (q0,q1) --a--> (q2,q3,q4)
         * 
         * COMPLEXIDADE DESTE LOOP:
         *   O(|curSet| · |Transitions|) para cada símbolo
         *   Total: O(|Σ| · |curSet| · m) onde m = transições do AFN
         **}
        for i := 0 to Alphabet.Count - 1 do
        begin
          sym := Alphabet[i];
          nextSet := TStringSet.Create;
          try
            {**
             * CALCULAR UNIÃO DOS MOVES
             * 
             * Para cada estado q do conjunto atual:
             *   Para cada transição do AFN:
             *     Se origem = q e símbolo = sym:
             *       Adicionar destino ao nextSet
             * 
             * Isso implementa: δ_AFD(curSet, sym) = ⋃ δ_AFN(q, sym)
             *                                        q∈curSet
             **}
            for j := 0 to curSet.Count - 1 do
            begin
              for k := 0 to High(Transitions) do
              begin
                if Transitions[k].FromState = curSet.Item(j) then
                  if Transitions[k].Symbol = sym then
                    nextSet.Add(Transitions[k].ToState);
              end;
            end;
            
{**
             * TRATAMENTO DE CONJUNTO VAZIO
             * 
             * Se nextSet está vazio (nenhuma transição com esse símbolo),
             * o AFD vai para um "estado de erro" implícito ()
             * 
             * EXEMPLO:
             *   Estado (q0) com símbolo 'b'
             *   Se não há transição q0 --b--> ? no AFN
             *   → nextSet vazio → Estado de erro ()
             * 
             * IMPORTANTE: () NÃO é adicionado ao AFD
             * (seria um dead state - rejeita tudo)
             **}
            if nextSet.IsEmpty then
              key := '()'  {** Estado de erro implícito **}
            else
              key := GetSetName(nextSet);  {** Nome do próximo estado **}
            
            {**
             * REGISTRAR TRANSIÇÃO NO AFD
             * 
             * Adicionar nova transição ao array dinâmico.
             * Usar SetLength + High para expandir array.
             * 
             * TRANSIÇÃO: curSet --sym--> nextSet
             * FORMATO: (origem, símbolo, destino)
             * 
             * Armazenar em DOIS lugares:
             *   1. localDFATransitions (variável local)
             *   2. DFATransitions (campo da classe)
             * 
             * Isso permite uso posterior e exibição no diagrama.
             **}
            SetLength(localDFATransitions, Length(localDFATransitions) + 1);
            localDFATransitions[High(localDFATransitions)].FromState := GetSetName(curSet);
            localDFATransitions[High(localDFATransitions)].Symbol := sym;
            localDFATransitions[High(localDFATransitions)].ToState := key;
            
            {** Também armazenar no campo privado da classe **}
            SetLength(DFATransitions, Length(DFATransitions) + 1);
            DFATransitions[High(DFATransitions)].FromState := GetSetName(curSet);
            DFATransitions[High(DFATransitions)].Symbol := sym;
            DFATransitions[High(DFATransitions)].ToState := key;
            
            {**
             * DESCOBERTA DE NOVOS ESTADOS
             * 
             * Se nextSet nunca foi visto antes, é um NOVO estado do AFD.
             * 
             * VERIFICAÇÕES:
             *   1. dfaMap.IndexOf(key) = -1 → não está no mapa
             *   2. localDFAStates.IndexOf(key) = -1 → não está na lista
             *   3. key ≠ '()' → não é estado de erro vazio
             * 
             * AÇÕES:
             *   1. Clonar nextSet (criar cópia independente)
             *   2. Adicionar ao mapa (nome → conjunto)
             *   3. Adicionar à lista de estados
             *   4. Adicionar à fila de trabalho (precisa ser processado)
             * 
             * EXEMPLO:
             *   Descobrimos (q2,q3) pela primeira vez
             *   → Adicionar a todas estruturas
             *   → Adicionar à fila (precisamos processar suas transições)
             * 
             * CLONE É CRÍTICO:
             *   nextSet será liberado no finally
             *   Precisamos de uma CÓPIA para guardar no mapa
             *   Clone() cria nova instância com mesmos elementos
             **}
            if (dfaMap.IndexOf(key) = -1) and (localDFAStates.IndexOf(key) = -1) then
            begin
              if key <> '()' then  {** Não adicionar estado de erro **}
              begin
                cloneSet := nextSet.Clone;  {** Criar cópia independente **}
                dfaMap.AddObject(key, cloneSet);  {** Guardar no mapa **}
              end;
              localDFAStates.Add(key);  {** Adicionar à lista de estados **}
              if key <> '()' then  {** Não processar estado de erro **}
                workQ.Add(key);  {** Adicionar à fila de trabalho **}
            end;
            
          finally
            {**
             * LIBERAR nextSet
             * 
             * IMPORTANTE: nextSet foi criado no início do loop de símbolo.
             * Precisa ser liberado a cada iteração para evitar memory leak.
             * 
             * Se nextSet foi clonado (novo estado), a cópia está no dfaMap.
             * Liberar nextSet não afeta a cópia.
             **}
            nextSet.Free;
          end;
        end;  {** Fim loop símbolos **}
      end;  {** Fim while fila não vazia **}
      
      {**
       * PASSO 8: EXIBIR RESULTADO
       * 
       * Formatar e apresentar o AFD resultante para o usuário.
       * 
       * SAÍDAS:
       *   1. Console (WriteLn): Para debugging e logs
       *   2. memoOutput: Interface gráfica (aba "AFD")
       *   3. Campos da classe: Para uso em minimização
       * 
       * FORMATO:
       *   - Cabeçalho decorativo
       *   - Alfabeto
       *   - Lista de estados
       *   - Estado inicial
       *   - Estados finais
       *   - Lista de transições
       **}
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('    RESULTADO DA CONVERSÃO AFN → AFD');
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('');
      
      {**
       * LOGGING NO CONSOLE
       * 
       * Imprimir estatísticas do AFD resultante.
       * Útil para debugging e verificação.
       * 
       * INFORMAÇÕES:
       *   - Número de estados (pode ser exponencial!)
       *   - Lista completa de estados
       *   - Estado inicial (sempre o primeiro)
       *   - Estados finais (os que contêm finais do AFN)
       *   - Número e lista de transições
       **}
      WriteLn('-------------------------------------------');
      WriteLn('AFD RESULTANTE:');
      WriteLn('  Estados: ', localDFAStates.Count, ' estados');
      for i := 0 to localDFAStates.Count - 1 do
        WriteLn('    ', localDFAStates[i]);
      WriteLn('  Estado inicial: ', localDFAStates[0]);
      WriteLn('  Estados finais: ', isFinal.Count);
      for i := 0 to isFinal.Count - 1 do
        WriteLn('    ', isFinal[i]);
      WriteLn('  Transicoes: ', Length(localDFATransitions), ' transicoes');
      for i := 0 to High(localDFATransitions) do
        WriteLn('    ', localDFATransitions[i].FromState, ' --', localDFATransitions[i].Symbol, '--> ', localDFATransitions[i].ToState);
      WriteLn('-------------------------------------------');
      WriteLn('');
      
      {**
       * FORMATAÇÃO PARA INTERFACE GRÁFICA
       * 
       * Apresentar resultado de forma amigável com emojis e formatação.
       **}
      memoOutput.Lines.Add('📋 ALFABETO: ' + Alphabet.CommaText);
      memoOutput.Lines.Add('');
      
      {**
       * LISTA DE ESTADOS DO AFD
       * 
       * Mostrar todos os estados gerados.
       * Também copiar para DFAStates (campo da classe).
       * 
       * NOTA: Número de estados pode ser muito maior que o AFN!
       * Exemplo: AFN com 10 estados → AFD com até 1024 estados (2^10)
       **}
      memoOutput.Lines.Add('🔵 ESTADOS DO DFA:');
      for i := 0 to localDFAStates.Count - 1 do
      begin
        memoOutput.Lines.Add('   • ' + localDFAStates[i]);
        DFAStates.Add(localDFAStates[i]);  {** Copiar para campo da classe **}
      end;
      memoOutput.Lines.Add('');
      
      {**
       * DETERMINAR ESTADO INICIAL
       * 
       * O estado inicial do AFD é sempre o PRIMEIRO da lista
       * (foi o primeiro adicionado, antes do loop principal).
       * 
       * Tentar recuperar do dfaMap para obter nome correto.
       * Se não encontrar, usar o primeiro da lista diretamente.
       **}
      if dfaMap.IndexOf(localDFAStates[0]) <> -1 then
      begin
        DFAInitial := GetSetName(TStringSet(dfaMap.Objects[dfaMap.IndexOf(localDFAStates[0])]));
        memoOutput.Lines.Add('▶️  ESTADO INICIAL: ' + DFAInitial);
      end
      else
      begin
        DFAInitial := localDFAStates[0];
        memoOutput.Lines.Add('▶️  ESTADO INICIAL: ' + DFAInitial);
      end;
      memoOutput.Lines.Add('');
      
      {**
       * LISTA DE ESTADOS FINAIS
       * 
       * Mostrar todos os estados finais do AFD.
       * Também copiar para DFAFinals (campo da classe).
       * 
       * LEMBRETE: Estado do AFD é final ⇔ contém algum estado final do AFN
       **}
      memoOutput.Lines.Add('🎯 ESTADOS FINAIS:');
      if isFinal.Count > 0 then
      begin
        for i := 0 to isFinal.Count - 1 do
        begin
          memoOutput.Lines.Add('   • ' + isFinal[i]);
          DFAFinals.Add(isFinal[i]);  {** Copiar para campo da classe **}
        end;
      end
      else
        memoOutput.Lines.Add('   (nenhum)');  {** Caso raro: AFD sem finais **}
      memoOutput.Lines.Add('');
      
      {**
       * LISTA DE TRANSIÇÕES
       * 
       * Mostrar todas as transições do AFD.
       * Formato: origem --símbolo--> destino
       * 
       * NOTA: AFD tem EXATAMENTE UMA transição por (estado, símbolo)
       * (ou nenhuma, indo para estado de erro implícito)
       **}
      memoOutput.Lines.Add('➡️  TRANSIÇÕES:');
      for i := 0 to High(localDFATransitions) do
        memoOutput.Lines.Add('   ' + localDFATransitions[i].FromState + ' --' + 
                            localDFATransitions[i].Symbol + '--> ' + 
                            localDFATransitions[i].ToState);
      
      memoOutput.Lines.Add('');
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('✅ Conversão concluída com sucesso!');
      
      {**
       * ATUALIZAR INTERFACE GRÁFICA
       * 
       * 1. Habilitar botão de minimização
       *    Agora que temos AFD, podemos minimizá-lo
       * 
       * 2. Mudar para aba de resultado AFD
       *    PageControl2 = Abas de texto (Entrada, AFN, AFD, MinDFA)
       * 
       * 3. Redesenhar diagramas
       *    Invalidate() força re-pintura dos painéis gráficos
       **}
      if Assigned(btnMinimize) then
        btnMinimize.Enabled := True;
      
      if Assigned(PageControl2) then
        PageControl2.ActivePage := TabOutput;
      
      if Assigned(PaintBoxNFA) then PaintBoxNFA.Invalidate;
      if Assigned(PaintBoxDFA) then PaintBoxDFA.Invalidate;
      
    finally
      {**
       * LIMPEZA DE ESTRUTURAS LOCAIS
       * 
       * IMPORTANTE: Liberar TODAS as estruturas criadas.
       * 
       * NOTA: dfaMap.OwnsObjects = True
       *   → Free() do dfaMap também libera todos os TStringSet
       * 
       * ESTRUTURAS NÃO LIBERADAS (campos da classe):
       *   - DFAStates, DFAFinals, DFATransitions
       *   - NFAAlphabet, NFAStates, etc.
       *   Serão usados por MinimizeDFA e diagramas
       **}
      localDFAStates.Free;
      workQ.Free;
      dfaMap.Free;  {** Também libera todos os TStringSet (OwnsObjects=True) **}
      isFinal.Free;
    end;
    
  finally
    {**
     * LIMPEZA DE ESTRUTURAS DE PARSING
     * 
     * Liberar todas as estruturas temporárias usadas no parsing.
     * Este bloco executa SEMPRE, mesmo se houver exceção.
     **}
    Alphabet.Free;
    States.Free;
    Initials.Free;
    Finals.Free;
    parts.Free;
  end;
end;

{**
 * PaintBoxEpsilonNFAPaint - Event handler para desenhar AFN-ε
 * 
 * Chamado automaticamente quando o painel precisa ser redesenhado:
 *   - Primeira exibição
 *   - Após Invalidate()
 *   - Após redimensionamento da janela
 * 
 * VALIDAÇÃO: Verifica se há dados antes de desenhar.
 * Se EpsilonNFAStates não foi criado, sai sem fazer nada.
 **}
procedure TFormMain.PaintBoxEpsilonNFAPaint(Sender: TObject);
begin
  {** Validação: Só desenha se há AFN-ε carregado **}
  if not Assigned(EpsilonNFAStates) then Exit;
  
  {**
   * Chamar rotina genérica de desenho
   * 
   * DrawAutomaton é um método polimórfico que desenha
   * qualquer autômato (AFN-ε, AFN, AFD, MinDFA).
   * 
   * PARÂMETROS:
   *   Canvas: Superfície de desenho
   *   States: Lista de estados
   *   Initials: Estados iniciais (podem ser múltiplos no AFN-ε)
   *   Finals: Estados finais
   *   Transitions: Array de transições
   **}
  DrawAutomaton(PaintBoxEpsilonNFA.Canvas, EpsilonNFAStates, EpsilonNFAInitials, 
                EpsilonNFAFinals, EpsilonNFATransitions);
end;

{**
 * PaintBoxNFAPaint - Event handler para desenhar AFN (sem epsilon)
 * 
 * Mesmo padrão do PaintBoxEpsilonNFAPaint, mas para AFN sem epsilon.
 **}
procedure TFormMain.PaintBoxNFAPaint(Sender: TObject);
begin
  if not Assigned(NFAStates) then Exit;
  DrawAutomaton(PaintBoxNFA.Canvas, NFAStates, NFAInitials, NFAFinals, NFATransitions);
end;

{**
 * PaintBoxDFAPaint - Event handler para desenhar AFD
 * 
 * DIFERENÇA: AFD tem apenas 1 estado inicial (não é lista)
 * Precisamos criar uma TStringList temporária para passar ao DrawAutomaton.
 * 
 * IMPORTANTE: Free() ao final para evitar memory leak!
 **}
procedure TFormMain.PaintBoxDFAPaint(Sender: TObject);
var
  Initials: TStringList;
begin
  if not Assigned(DFAStates) or (DFAStates.Count = 0) then Exit;
  
  {**
   * Criar lista temporária com estado inicial
   * 
   * AFD tem sempre exatamente 1 estado inicial.
   * Estado inicial é sempre o primeiro da lista DFAStates.
   * 
   * Criar TStringList → Adicionar inicial → Passar ao DrawAutomaton → Free
   **}
  Initials := TStringList.Create;
  try
    if DFAStates.Count > 0 then
      Initials.Add(DFAStates[0]);  {** Primeiro estado = inicial **}
    
    DrawAutomaton(PaintBoxDFA.Canvas, DFAStates, Initials, DFAFinals, DFATransitions);
  finally
    Initials.Free;  {** IMPORTANTE: Liberar memória! **}
  end;
end;

{**
 * PaintBoxMinDFAPaint - Event handler para desenhar AFD Minimizado
 * 
 * Mesmo padrão do PaintBoxDFAPaint.
 * 
 * DIFERENÇA: Usa MinDFAInitial (string) ao invés de pegar o primeiro.
 **}
procedure TFormMain.PaintBoxMinDFAPaint(Sender: TObject);
var
  Initials: TStringList;
begin
  if not Assigned(MinDFAStates) or (MinDFAStates.Count = 0) then Exit;
  
  Initials := TStringList.Create;
  try
    {**
     * Adicionar estado inicial do MinDFA
     * 
     * MinDFAInitial é string com nome do estado inicial.
     * É definido durante a minimização (partição que contém o inicial do AFD).
     **}
    if MinDFAInitial <> '' then
      Initials.Add(MinDFAInitial);
    
    DrawAutomaton(PaintBoxMinDFA.Canvas, MinDFAStates, Initials, MinDFAFinals, MinDFATransitions);
  finally
    Initials.Free;
  end;
end;

procedure TFormMain.btnMinimizeClick(Sender: TObject);
begin
  WriteLn('[GUI] Iniciando minimizacao do AFD...');
  try
    MinimizeDFA;
    WriteLn('[GUI] Minimizacao concluida com sucesso!');
  except
    on E: Exception do
    begin
      WriteLn('[ERRO] Falha na minimizacao: ', E.Message);
      ShowMessage('Erro na minimização: ' + E.Message);
    end;
  end;
end;

{******************************************************************************
 * MinimizeDFA - Minimização de AFD usando Algoritmo de Myhill-Nerode
 * 
 * Este método implementa o algoritmo clássico de minimização de autômatos
 * finitos determinísticos baseado no Teorema de Myhill-Nerode. O objetivo é
 * encontrar o MENOR autômato equivalente (mesmo comportamento, menos estados).
 * 
 * TEORIA: Teorema de Myhill-Nerode
 * 
 * Dois estados p e q são EQUIVALENTES se:
 *   ∀w ∈ Σ*: δ(p,w) ∈ F ⟺ δ(q,w) ∈ F
 * 
 * Ou seja: Para QUALQUER string w, ou ambos aceitam ou ambos rejeitam.
 * 
 * ALGORITMO: Partition-Refinement
 * 
 * FASE 1: Partição Inicial
 *   P₀ = (F, Q \ F)
 *   Dividir em dois grupos: finais e não-finais
 *   (Estados finais e não-finais NUNCA são equivalentes)
 * 
 * FASE 2: Refinamento Iterativo
 *   Repeat:
 *     Para cada partição π:
 *       Para cada par de estados (p,q) em π:
 *         Para cada símbolo a:
 *           Se δ(p,a) e δ(q,a) estão em partições diferentes:
 *             → p e q NÃO são equivalentes
 *             → Dividir π
 *     Until: Nenhuma partição foi dividida (ponto fixo)
 * 
 * FASE 3: Construção do AFD Minimizado
 *   - Cada partição final → 1 estado no AFD minimizado
 *   - Estado inicial: Partição que contém o estado inicial do AFD original
 *   - Estados finais: Partições que contêm estados finais do AFD original
 *   - Transições: δ_min([p], a) = [δ(p,a)]
 * 
 * EXEMPLO:
 * 
 * AFD Original (6 estados):
 *   Estados: (q0, q1, q2, q3, q4, q5)
 *   Inicial: q0
 *   Finais: (q2, q5)
 *   Transições:
 *     q0 --a--> q1    q0 --b--> q2
 *     q1 --a--> q0    q1 --b--> q3
 *     q2 --a--> q2    q2 --b--> q2
 *     q3 --a--> q1    q3 --b--> q4
 *     q4 --a--> q0    q4 --b--> q5
 *     q5 --a--> q5    q5 --b--> q5
 * 
 * Partição Inicial:
 *   P₀ = ((q0,q1,q3,q4), (q2,q5))
 *        ^^^^^^^^^^^^^^^^  ^^^^^^^
 *        Não-finais        Finais
 * 
 * Refinamento (Iteração 1):
 *   Analisar (q0,q1,q3,q4):
 *     q0 --a--> q1 (não-final)   q0 --b--> q2 (final)
 *     q1 --a--> q0 (não-final)   q1 --b--> q3 (não-final)
 *     δ(q0,b) e δ(q1,b) em partições diferentes!
 *     → q0 ≠ q1 → Dividir
 *   
 *   P₁ = ((q0,q4), (q1,q3), (q2,q5))
 * 
 * Refinamento (Iteração 2):
 *   Todas as verificações confirmam equivalência
 *   → Nenhuma divisão → CONVERGIU
 * 
 * AFD Minimizado (3 estados):
 *   Estados: (p0, p1, p2)
 *   Inicial: p0 = [q0,q4]
 *   Finais: (p2) = [q2,q5]
 *   Transições:
 *     p0 --a--> p1    p0 --b--> p2
 *     p1 --a--> p0    p1 --b--> p1
 *     p2 --a--> p2    p2 --b--> p2
 * 
 * Redução: 6 estados → 3 estados (50% menor!)
 * 
 * ESTRUTURAS DE DADOS:
 * 
 * partitions: TList of TStringList
 *   - Cada elemento é uma partição (conjunto de estados)
 *   - Exemplo: [ ["q0","q4"], ["q1","q3"], ["q2","q5"] ]
 * 
 * statePartition: array of Integer
 *   - Mapeamento: statePartition[i] = índice da partição que contém DFAStates[i]
 *   - Exemplo: Se q0 está em P₀, então statePartition[0] = 0
 * 
 * partitionNames: TStringList
 *   - Nomes dos novos estados minimizados
 *   - Exemplo: ["P0", "P1", "P2"]
 * 
 * COMPLEXIDADE:
 *   O(n · m · log n) - Algoritmo de Hopcroft/Minimização Eficiente
 *   Onde:
 *     n = número de estados
 *     m = número de transições
 *   
 *   Complexidade desta implementação:
 *     O(n² · m) - Implementação ingênua com comparação par a par
 *   
 *   No pior caso: O(n² · n · |Σ|) = O(n³ · |Σ|)
 *   Na prática: Geralmente O(n · m) porque poucas iterações
 * 
 * INVARIANTES:
 *   1. Estados em partições diferentes NÃO são equivalentes
 *   2. Estados na mesma partição SÃO equivalentes (após convergência)
 *   3. Número de partições só AUMENTA (nunca diminui)
 *   4. Convergência garantida (ponto fixo existe)
 * 
 * OBSERVAÇÕES DE IMPLEMENTAÇÃO:
 *   - Use NFAAlphabet (alfabeto do autômato original)
 *   - Estados vazios "()" devem ser tratados
 *   - Transições ausentes (dest = '') são tratadas especialmente
 ******************************************************************************}
procedure TFormMain.MinimizeDFA;
var
  // Partições de estados equivalentes
  partitions, newPartitions: TList;
  i, j, k, m, partIdx1, partIdx2: Integer;
  statePartition: array of Integer;
  partitionsChanged: Boolean;
  partition: TStringList;
  state1, state2, sym, dest1, dest2: string;
  equiv: Boolean;
  partitionNames: TStringList;
  partName: string;
  trans: TTransition;
  mappedState, mappedDest: string;
  oldInitialPartition: Integer;
begin
  {** Validação: Verificar se há AFD para minimizar **}
  if not Assigned(DFAStates) or (DFAStates.Count = 0) then
  begin
    ShowMessage('Nenhum AFD para minimizar! Execute a conversão AFN → AFD primeiro.');
    Exit;
  end;
  
  {** LOG: Estatísticas do AFD de entrada **}
  WriteLn('[GUI] AFD antes da minimizacao: ', DFAStates.Count, ' estados');
  WriteLn('-------------------------------------------');
  WriteLn('INICIANDO MINIMIZACAO DO AFD:');
  WriteLn('  Estados do AFD: ', DFAStates.Count);
  WriteLn('  Estados finais: ', DFAFinals.Count);
  WriteLn('  Transicoes: ', Length(DFATransitions));
  WriteLn('-------------------------------------------');
  WriteLn('');
  
  {**
   * Limpar dados anteriores do AFD minimizado
   * 
   * IMPORTANTE: Sempre limpar antes de minimizar novamente
   * (permite re-minimização com dados atualizados)
   **}
  MinDFAStates.Clear;
  MinDFAFinals.Clear;
  SetLength(MinDFATransitions, 0);
  MinDFAInitial := '';
  
  partitions := TList.Create;
  partitionNames := TStringList.Create;
  try
    {**
     * FASE 1: Partição Inicial - Dividir em finais e não-finais
     * 
     * Esta é a divisão mais grossa possível. Estados finais e não-finais
     * NUNCA podem ser equivalentes (comportamento diferente por definição).
     * 
     * statePartition[i] = índice da partição que contém DFAStates[i]
     **}
    SetLength(statePartition, DFAStates.Count);
    
    {**
     * Criar Partição 0: Estados NÃO-FINAIS
     * 
     * Coletar todos os estados que NÃO são finais.
     * IndexOf retorna -1 se estado não está em DFAFinals.
     * 
     * INVARIANTE: Estados não-finais NUNCA são equivalentes a estados finais
     * (comportamento fundamental diferente: aceitar vs rejeitar)
     **}
    partition := TStringList.Create;
    partitions.Add(partition);
    for i := 0 to DFAStates.Count - 1 do
      if DFAFinals.IndexOf(DFAStates[i]) = -1 then
      begin
        partition.Add(DFAStates[i]);
        statePartition[i] := 0;  // Mapear estado i para partição 0
      end;
    
    {**
     * Criar Partição 1: Estados FINAIS
     * 
     * Coletar todos os estados que SÃO finais.
     * IndexOf retorna >= 0 se estado está em DFAFinals.
     **}
    partition := TStringList.Create;
    partitions.Add(partition);
    for i := 0 to DFAStates.Count - 1 do
      if DFAFinals.IndexOf(DFAStates[i]) >= 0 then
      begin
        partition.Add(DFAStates[i]);
        statePartition[i] := 1;  // Mapear estado i para partição 1
      end;
    
    WriteLn('[GUI] Particao inicial: ', partitions.Count, ' grupos');
    
    {**
     * FASE 2: REFINAMENTO ITERATIVO (Partition-Refinement)
     * 
     * OBJETIVO: Subdividir partições até encontrar estados realmente equivalentes
     * 
     * ALGORITMO:
     *   Repeat:
     *     Para cada partição π:
     *       Para cada par de estados (p, q) em π:
     *         Se p e q têm comportamentos diferentes com algum símbolo:
     *           → Separar p e q em partições diferentes
     *   Until: Nenhuma partição foi dividida (ponto fixo alcançado)
     * 
     * CRITÉRIO DE EQUIVALÊNCIA:
     *   Dois estados p e q são equivalentes SE E SOMENTE SE:
     *   Para TODO símbolo a:
     *     δ(p,a) e δ(q,a) estão na MESMA partição
     * 
     * EXEMPLO:
     *   Partição inicial: (q0, q1, q2) (todos não-finais)
     *   Verificar q0 e q1:
     *     Com 'a': q0→q3, q1→q4
     *     Se q3 está em P₁ e q4 está em P₂ (partições diferentes):
     *       → q0 ≠ q1 → Dividir partição!
     * 
     * COMPLEXIDADE: O(n² · |Σ| · iterações)
     *   - No pior caso: O(n) iterações
     *   - Cada iteração: O(n² · |Σ|) comparações
     *   - Total: O(n³ · |Σ|) no pior caso
     * 
     * CONVERGÊNCIA GARANTIDA:
     *   - Número de partições só AUMENTA (nunca diminui)
     *   - Máximo n partições (cada estado sozinho)
     *   - Logo, algoritmo SEMPRE termina
     **}
    repeat
      partitionsChanged := False;  // Flag: alguma partição foi dividida?
      newPartitions := TList.Create;  // Nova lista de partições refinadas
      
      {**
       * Iterar por todas as partições atuais
       * 
       * Para cada partição, tentar dividir em subpartições menores
       * baseado no comportamento dos estados.
       **}
      for partIdx1 := 0 to partitions.Count - 1 do
      begin
        partition := TStringList(partitions[partIdx1]);
        
        {**
         * OTIMIZAÇÃO: Partição com ≤ 1 estado não pode ser dividida
         * 
         * Se partição tem apenas 1 estado, não há o que comparar.
         * Adiciona à nova lista sem modificação e pula para próxima.
         **}
        if partition.Count <= 1 then
        begin
          newPartitions.Add(partition);
          Continue;
        end;
        
        {**
         * TENTATIVA DE DIVISÃO: Verificar se estados são realmente equivalentes
         * 
         * ESTRATÉGIA:
         *   - Pegar cada estado state1
         *   - Comparar com todos os outros estados state2 na mesma partição
         *   - Se state1 ≠ state2: remover state2 (será processado depois)
         *   - Se state1 = state2: manter juntos
         * 
         * ALGORITMO INGÊNUO: Comparação par a par
         * (Algoritmo de Hopcroft seria mais eficiente, mas mais complexo)
         **}
        i := 0;
        while i < partition.Count do
        begin
          state1 := partition[i];
          
          {**
           * Remover temporariamente state1 para evitar auto-comparação
           * 
           * CUIDADO: Este delete não libera memória, apenas remove da lista.
           * state1 ainda existe na variável local.
           **}
          TStringList(partitions[partIdx1]).Delete(i);
          
          equiv := True;
          j := i;  // Começar de i (não de 0) pois já processamos estados anteriores
          
          {**
           * LOOP INTERNO: Comparar state1 com todos os estados restantes
           * 
           * Para cada state2 após state1:
           *   - Verificar se têm comportamento idêntico
           *   - Se diferente: deixar state2 na partição (será processado depois)
           *   - Se igual: remover state2 (é equivalente a state1)
           **}
          while j < partition.Count do
          begin
            state2 := partition[j];
            equiv := True;  // Assumir equivalente até provar o contrário
            
            {**
             * TESTE DE EQUIVALÊNCIA: Verificar comportamento para CADA símbolo
             * 
             * DOIS ESTADOS SÃO EQUIVALENTES SE:
             *   ∀a ∈ Σ: [δ(p,a)] = [δ(q,a)]
             * 
             * Ou seja: Para todo símbolo, os destinos devem estar na MESMA partição.
             * 
             * EXEMPLO:
             *   state1 --a--> q3  (q3 está em P₁)
             *   state2 --a--> q4  (q4 está em P₂)
             *   Como P₁ ≠ P₂ → state1 ≠ state2 → NÃO equivalentes!
             **}
            for k := 0 to NFAAlphabet.Count - 1 do
            begin
              sym := NFAAlphabet[k];
              
              {**
               * Encontrar destinos de state1 e state2 com símbolo atual
               * 
               * BUSCA: Percorrer todas as transições do AFD
               * Procurar: (state1, sym, ?) e (state2, sym, ?)
               * 
               * dest1 = destino de state1 com símbolo sym
               * dest2 = destino de state2 com símbolo sym
               * 
               * OBSERVAÇÃO: dest pode ser '' se não houver transição
               * (estado de rejeição implícito)
               **}
              dest1 := '';
              dest2 := '';
              
              for trans in DFATransitions do
              begin
                if (trans.FromState = state1) and (trans.Symbol = sym) then
                  dest1 := trans.ToState;
                if (trans.FromState = state2) and (trans.Symbol = sym) then
                  dest2 := trans.ToState;
              end;
              
              {**
               * VERIFICAÇÃO DE EQUIVALÊNCIA: Destinos na mesma partição?
               * 
               * CASO 1: Ambos têm transição (dest1 ≠ '' e dest2 ≠ '')
               *   - Obter partição de cada destino
               *   - Se partições diferentes → estados NÃO equivalentes
               * 
               * CASO 2: Apenas um tem transição (dest1 ≠ '' XOR dest2 ≠ '')
               *   - Um vai para estado válido, outro para rejeição
               *   - Comportamento diferente → NÃO equivalentes
               * 
               * CASO 3: Nenhum tem transição (dest1 = '' e dest2 = '')
               *   - Ambos vão para rejeição
               *   - Comportamento igual (continua verificando outros símbolos)
               * 
               * EXEMPLO:
               *   q0 --a--> q2 (q2 está em P₁)
               *   q1 --a--> q3 (q3 está em P₂)
               *   Como P₁ ≠ P₂ → q0 e q1 NÃO são equivalentes!
               **}
              if (dest1 <> '') and (dest2 <> '') then
              begin
                {** CASO 1: Ambos têm destino válido **}
                partIdx2 := statePartition[DFAStates.IndexOf(dest1)];
                m := statePartition[DFAStates.IndexOf(dest2)];
                
                if partIdx2 <> m then
                begin
                  equiv := False;  // Destinos em partições diferentes!
                  Break;           // Não precisa verificar mais símbolos
                end;
              end
              else if dest1 <> dest2 then
              begin
                {**
                 * CASO 2: Apenas um tem destino
                 * 
                 * Se dest1 = '' e dest2 = 'q3' (ou vice-versa):
                 *   - Um vai para rejeição, outro continua
                 *   - Comportamentos diferentes!
                 **}
                equiv := False;
                Break;
              end;
              {** CASO 3: Ambos vazios (dest1 = dest2 = '') - continua loop **}
            end;  // Fim loop símbolos
            
            {**
             * DECISÃO: Estados são equivalentes ou não?
             * 
             * Se NOT equiv (comportamento diferente encontrado):
             *   - Deixar state2 na partição (será comparado com próximo state1)
             *   - Incrementar j para próximo estado
             * 
             * Se equiv (comportamento idêntico para TODOS os símbolos):
             *   - Remover state2 da partição (é equivalente a state1)
             *   - Marcar partitionsChanged = True (houve divisão!)
             *   - NÃO incrementar j (próximo estado agora está na posição j)
             **}
            if not equiv then
              Inc(j)  // state2 não é equivalente, próximo
            else
            begin
              partition.Delete(j);      // state2 é equivalente, remover
              partitionsChanged := True; // Partição foi modificada
            end;
          end;  // Fim loop comparação com outros estados
          
          {**
           * Reinsere state1 de volta na partição
           * 
           * IMPORTANTE: state1 foi removido temporariamente no início.
           * Agora que comparamos com todos os outros, adicionar de volta.
           * 
           * Todos os estados equivalentes a state1 foram removidos,
           * então agora state1 representa sua própria classe de equivalência.
           **}
          partition.Insert(i, state1);
          Inc(i);  // Próximo estado a ser processado
        end;  // Fim loop estados da partição
        
        newPartitions.Add(partition);
      end;  // Fim loop partições
      
      {**
       * Substituir lista de partições antiga pela nova
       * 
       * A lista antiga é liberada e substituída pela refinada.
       **}
      partitions.Free;
      partitions := newPartitions;
      
      {**
       * ATUALIZAR MAPEAMENTO: statePartition[i] → nova partição
       * 
       * IMPORTANTE: Após o refinamento, os índices das partições mudaram.
       * Precisamos atualizar o array statePartition para refletir
       * em qual partição cada estado está agora.
       * 
       * ALGORITMO:
       *   Para cada partição i:
       *     Para cada estado s nessa partição:
       *       statePartition[índice de s] = i
       * 
       * EXEMPLO:
       *   Se estado "q0" está na partição 2:
       *     statePartition[DFAStates.IndexOf("q0")] = 2
       **}
      for i := 0 to partitions.Count - 1 do
      begin
        partition := TStringList(partitions[i]);
        for j := 0 to partition.Count - 1 do
        begin
          k := DFAStates.IndexOf(partition[j]);
          if k >= 0 then
            statePartition[k] := i;
        end;
      end;
      
    until not partitionsChanged;  // CONVERGÊNCIA: Nenhuma partição foi dividida
    
    WriteLn('[GUI] Particoes finais: ', partitions.Count, ' grupos');
    WriteLn('[GUI] Criando estados minimizados...');
    
    {**
     * FASE 3: CONSTRUIR AFD MINIMIZADO
     * 
     * Agora que temos as partições finais (classes de equivalência),
     * cada partição vira UM ÚNICO ESTADO no AFD minimizado.
     * 
     * MAPEAMENTO:
     *   Partição com 1 estado: usar nome do estado
     *   Partição com N estados: criar nome "[q0,q1,...,qN]"
     * 
     * EXEMPLO:
     *   Partição (q0, q4) → Estado "[q0,q4]"
     *   Partição (q2)     → Estado "q2"
     * 
     * PROPRIEDADES DO NOVO AFD:
     *   - Estados: Um por partição
     *   - Estado inicial: Partição que contém o estado inicial original
     *   - Estados finais: Partições que contêm algum estado final original
     *   - Transições: δ_min([p], a) = [δ(p,a)]
     **}
    for i := 0 to partitions.Count - 1 do
    begin
      partition := TStringList(partitions[i]);
      if partition.Count > 0 then
      begin
        {**
         * Gerar nome do estado minimizado
         * 
         * CONVENÇÃO:
         *   - Se partição tem 1 estado: use o nome original
         *   - Se partição tem N estados: use "[estado1,estado2,...,estadoN]"
         * 
         * EXEMPLO:
         *   (q0, q1, q2) → "[q0,q1,q2]"
         *   (q5)         → "q5"
         **}
        if partition.Count = 1 then
          partName := partition[0]
        else
        begin
          {** Concatenar nomes com vírgulas entre colchetes **}
          partName := '[';
          for j := 0 to partition.Count - 1 do
          begin
            if j > 0 then partName += ',';
            partName += partition[j];
          end;
          partName += ']';
        end;
        
        MinDFAStates.Add(partName);
        partitionNames.Add(partName);  // Guardar para mapear transições
        
        {**
         * Determinar se é ESTADO FINAL
         * 
         * REGRA: Uma partição é final SE E SOMENTE SE
         * contém pelo menos um estado final do AFD original.
         * 
         * JUSTIFICATIVA:
         *   Se contém estado final, aceita alguma string.
         *   Estados equivalentes aceitam as mesmas strings.
         *   Logo, toda a partição deve ser final.
         * 
         * EXEMPLO:
         *   Se (q2, q5) e q5 é final → partição é final
         **}
        for j := 0 to partition.Count - 1 do
          if DFAFinals.IndexOf(partition[j]) >= 0 then
          begin
            MinDFAFinals.Add(partName);
            Break;
          end;
        
        {**
         * Determinar se é ESTADO INICIAL
         * 
         * REGRA: A partição que contém o estado inicial
         * do AFD original é o estado inicial do AFD minimizado.
         * 
         * EXEMPLO:
         *   Se DFAInitial = "q0" e partição contém q0:
         *     → MinDFAInitial = nome dessa partição
         **}
        for j := 0 to partition.Count - 1 do
          if partition[j] = DFAInitial then
          begin
            MinDFAInitial := partName;
            Break;
          end;
      end;
    end;
    
    {**
     * FASE 4: CRIAR TRANSIÇÕES MINIMIZADAS
     * 
     * ALGORITMO:
     *   Para cada partição P:
     *     Escolher representante r ∈ P (qualquer estado da partição)
     *     Para cada símbolo a:
     *       Encontrar δ(r, a) = destino
     *       Encontrar partição Q que contém destino
     *       Criar transição: P --a--> Q
     * 
     * TEOREMA: A escolha do representante não importa!
     *   Se r₁ e r₂ estão na mesma partição (são equivalentes):
     *     δ(r₁, a) e δ(r₂, a) estão na MESMA partição
     *   Logo, qualquer representante produz a mesma transição.
     * 
     * EXEMPLO:
     *   Partição P₀ = (q0, q4)
     *   Representante: q0
     *   
     *   q0 --a--> q1
     *   q1 está na partição P₁
     *   
     *   Transição minimizada: P₀ --a--> P₁
     * 
     * COMPLEXIDADE: O(partições · |Σ| · |transições|)
     **}
    for i := 0 to partitions.Count - 1 do
    begin
      partition := TStringList(partitions[i]);
      if partition.Count = 0 then Continue;
      
      mappedState := partitionNames[i];  // Nome do estado minimizado
      state1 := partition[0];            // REPRESENTANTE da partição
      
      {** Para cada símbolo do alfabeto **}
      for k := 0 to NFAAlphabet.Count - 1 do
      begin
        sym := NFAAlphabet[k];
        dest1 := '';
        
        {**
         * Encontrar transição do representante
         * 
         * BUSCA: Procurar em DFATransitions por (state1, sym, ?)
         * 
         * OBSERVAÇÃO: Break assim que encontrar (AFD tem no máximo
         * uma transição por (estado, símbolo))
         **}
        for trans in DFATransitions do
        begin
          if (trans.FromState = state1) and (trans.Symbol = sym) then
          begin
            dest1 := trans.ToState;
            Break;
          end;
        end;
        
        if dest1 <> '' then
        begin
          {**
           * Mapear destino para sua partição
           * 
           * PASSOS:
           *   1. Encontrar índice da partição que contém dest1
           *   2. Usar partitionNames[índice] para obter nome da partição destino
           *   3. Criar transição: mappedState --sym--> mappedDest
           **}
          oldInitialPartition := statePartition[DFAStates.IndexOf(dest1)];
          mappedDest := partitionNames[oldInitialPartition];
          
          {** Adicionar transição ao array **}
          SetLength(MinDFATransitions, Length(MinDFATransitions) + 1);
          MinDFATransitions[High(MinDFATransitions)].FromState := mappedState;
          MinDFATransitions[High(MinDFATransitions)].Symbol := sym;
          MinDFATransitions[High(MinDFATransitions)].ToState := mappedDest;
        end;
      end;
    end;
    
    // LOG: AFD Minimizado resultante
    WriteLn('');
    WriteLn('-------------------------------------------');
    WriteLn('AFD MINIMIZADO:');
    WriteLn('  Estados: ', MinDFAStates.Count, ' estados');
    for i := 0 to MinDFAStates.Count - 1 do
      WriteLn('    ', MinDFAStates[i]);
    WriteLn('  Estado inicial: ', MinDFAInitial);
    WriteLn('  Estados finais: ', MinDFAFinals.Count);
    for i := 0 to MinDFAFinals.Count - 1 do
      WriteLn('    ', MinDFAFinals[i]);
    WriteLn('  Transicoes: ', Length(MinDFATransitions), ' transicoes');
    for i := 0 to High(MinDFATransitions) do
      WriteLn('    ', MinDFATransitions[i].FromState, ' --', MinDFATransitions[i].Symbol, '--> ', MinDFATransitions[i].ToState);
    WriteLn('-------------------------------------------');
    WriteLn('  REDUCAO: ', DFAStates.Count, ' -> ', MinDFAStates.Count, ' estados');
    WriteLn('-------------------------------------------');
    WriteLn('');
    
    // Exibir resultado no memoMinOutput
    memoMinOutput.Lines.Clear;
    memoMinOutput.Lines.Add('═══════════════════════════════════════════════════');
    memoMinOutput.Lines.Add('    AFD MINIMIZADO');
    memoMinOutput.Lines.Add('═══════════════════════════════════════════════════');
    memoMinOutput.Lines.Add('');
    memoMinOutput.Lines.Add('📋 ALFABETO: ' + NFAAlphabet.CommaText);
    memoMinOutput.Lines.Add('');
    memoMinOutput.Lines.Add('🔵 ESTADOS MINIMIZADOS:');
    for i := 0 to MinDFAStates.Count - 1 do
      memoMinOutput.Lines.Add('   • ' + MinDFAStates[i]);
    memoMinOutput.Lines.Add('');
    memoMinOutput.Lines.Add('▶️  ESTADO INICIAL: ' + MinDFAInitial);
    memoMinOutput.Lines.Add('');
    memoMinOutput.Lines.Add('🎯 ESTADOS FINAIS:');
    if MinDFAFinals.Count > 0 then
      for i := 0 to MinDFAFinals.Count - 1 do
        memoMinOutput.Lines.Add('   • ' + MinDFAFinals[i])
    else
      memoMinOutput.Lines.Add('   (nenhum)');
    memoMinOutput.Lines.Add('');
    memoMinOutput.Lines.Add('➡️  TRANSIÇÕES:');
    for i := 0 to High(MinDFATransitions) do
      memoMinOutput.Lines.Add('   ' + MinDFATransitions[i].FromState + ' --' + 
                          MinDFATransitions[i].Symbol + '--> ' + 
                          MinDFATransitions[i].ToState);
    memoMinOutput.Lines.Add('');
    memoMinOutput.Lines.Add('📊 REDUÇÃO: ' + IntToStr(DFAStates.Count) + ' → ' + IntToStr(MinDFAStates.Count) + ' estados');
    memoMinOutput.Lines.Add('═══════════════════════════════════════════════════');
    memoMinOutput.Lines.Add('✅ Minimização concluída!');
    
    // Também adicionar resumo no memoOutput
    memoOutput.Lines.Add('');
    // Também adicionar resumo no memoOutput
    memoOutput.Lines.Add('');
    memoOutput.Lines.Add('═══════════════════════════════════════════════════');
    memoOutput.Lines.Add('    MINIMIZAÇÃO CONCLUÍDA');
    memoOutput.Lines.Add('═══════════════════════════════════════════════════');
    memoOutput.Lines.Add('');
    memoOutput.Lines.Add('📊 Redução de estados: ' + IntToStr(DFAStates.Count) + ' → ' + IntToStr(MinDFAStates.Count));
    memoOutput.Lines.Add('✅ Veja o resultado completo na aba "⚡ AFD Minimizado"');
    
    WriteLn('[GUI] AFD minimizado: ', MinDFAStates.Count, ' estados');
    WriteLn('[GUI] Reducao: ', DFAStates.Count, ' -> ', MinDFAStates.Count, ' estados');
    
    // Mudar para a aba de resultado minimizado
    if Assigned(PageControl2) then
      PageControl2.ActivePage := TabMinOutput;
    
    // Redesenhar diagrama
    if Assigned(PaintBoxMinDFA) then
    begin
      PaintBoxMinDFA.Invalidate;
      PageControl1.ActivePage := TabMinDFA;
    end;
    
  finally
    // Limpar partições
    for i := 0 to partitions.Count - 1 do
      TStringList(partitions[i]).Free;
    partitions.Free;
    partitionNames.Free;
  end;
end;

{******************************************************************************
 * DrawAutomaton - Desenha diagrama de autômato em um TCanvas
 * 
 * Este método é responsável pela renderização visual completa de um autômato
 * finito. Desenha estados, transições, setas de entrada, e destaca estados
 * finais. Tudo feito usando API nativa do Pascal (TCanvas) sem bibliotecas
 * externas.
 * 
 * PARÂMETROS:
 *   ACanvas: Superfície de desenho (vem de TPaintBox.Canvas)
 *   States: Lista de todos os estados para desenhar
 *   Initials: Estados iniciais (desenha seta de entrada)
 *   Finals: Estados finais (desenha círculo duplo)
 *   Transitions: Array com todas as transições
 * 
 * LAYOUT DO DIAGRAMA:
 * 
 * Constantes:
 *   StateRadius = 20px      - Raio dos círculos de estado
 *   StateSpacingX = 100px   - Distância horizontal entre estados
 *   StateSpacingY = 80px    - Distância vertical entre linhas
 *   MarginX = 50px          - Margem esquerda
 *   MarginY = 40px          - Margem superior
 * 
 * Grid:
 *   - Máximo 4 estados por linha (cols)
 *   - Quantas linhas forem necessárias (rows)
 *   - Posicionamento fixo: não centraliza (evita cortar setas)
 * 
 * Elementos Desenhados:
 *   1. Estados: Círculos de raio 20px
 *   2. Estados finais: Círculo duplo + fundo amarelo
 *   3. Seta de entrada: Indica estados iniciais (20px à esquerda)
 *   4. Transições: Linhas com setas
 *   5. Self-loops: Arcos acima do estado
 *   6. Rótulos: Símbolos das transições
 * 
 * ALGORITMO:
 * 
 * Fase 1: Calcular Posições
 *   - Determinar grid (cols × rows)
 *   - Calcular (x,y) para cada estado
 *   - Armazenar em array statePos
 * 
 * Fase 2: Desenhar Transições
 *   - Para cada transição:
 *     * Se self-loop: desenhar arco acima
 *     * Senão: desenhar linha com seta
 *     * Adicionar rótulo no meio
 * 
 * Fase 3: Desenhar Estados
 *   - Para cada estado:
 *     * Se final: círculo duplo + amarelo
 *     * Senão: círculo simples
 *     * Escrever nome no centro
 * 
 * Fase 4: Desenhar Setas de Entrada
 *   - Para cada inicial:
 *     * Desenhar seta apontando de fora
 * 
 * GEOMETRIA DAS SETAS:
 * 
 * Cálculo de ângulo:
 *   angle = arctan2(y2 - y1, x2 - x1)
 * 
 * Ponto de partida (borda do círculo):
 *   x = x_centro + raio * cos(angle)
 *   y = y_centro + raio * sin(angle)
 * 
 * Ponta da seta (triângulo):
 *   Duas linhas com ângulo de ±30° (Pi/6)
 *   Comprimento: 8px
 * 
 * CORES:
 *   - Estados normais: Branco (clWhite)
 *   - Estados finais: Amarelo (clYellow)
 *   - Linhas/texto: Preto (clBlack)
 * 
 * FONTE:
 *   - Tamanho: 8pt
 *   - Estilo: Negrito
 * 
 * COMPLEXIDADE:
 *   O(n + m) onde n = estados, m = transições
 *   - Calcular posições: O(n)
 *   - Desenhar transições: O(m)
 *   - Desenhar estados: O(n)
 *   - Desenhar setas: O(iniciais)
 ******************************************************************************}
procedure TFormMain.DrawAutomaton(ACanvas: TCanvas; States, Initials, Finals: TStringList;
  const Transitions: TTransitionArray);
const
  StateRadius = 20;         // Raio dos círculos de estado
  StateSpacingX = 100;      // Espaçamento horizontal entre estados
  StateSpacingY = 80;       // Espaçamento vertical entre linhas
  MarginX = 50;             // Margem esquerda
  MarginY = 40;             // Margem superior
var
  i, j, x, y, x1, y1, x2, y2: Integer;
  cols, rows, col, row: Integer;
  statePos: array of record x, y: Integer; end;
  midX, midY, dx, dy: Integer;
  angle: Double;
  arrowLen: Integer;
  selfLoopAngle: Double;
  labelX, labelY: Integer;
  transLabel: string;
begin
  {**
   * FASE 0: Limpar canvas
   * 
   * Preencher toda a área com branco para limpar desenhos anteriores.
   **}
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(0, 0, ACanvas.Width, ACanvas.Height);
  
  {** Validação: Se não há estados, mostrar mensagem e sair **}
  if States.Count = 0 then
  begin
    ACanvas.TextOut(10, 10, 'Nenhum autômato para exibir');
    Exit;
  end;
  
  {**
   * FASE 1: Calcular grid de posições
   * 
   * OBJETIVO: Distribuir estados em grid de colunas × linhas
   * 
   * RESTRIÇÃO: Máximo 4 colunas (para evitar diagramas muito largos)
   * 
   * CÁLCULO:
   *   cols = min(4, ceil(√n))     - Preferência por layout quadrado
   *   rows = ceil(n / cols)        - Quantas linhas necessárias
   * 
   * EXEMPLO:
   *   10 estados:
   *     cols = min(4, ceil(√10)) = min(4, 4) = 4
   *     rows = ceil(10/4) = 3
   *     Grid: 4×3 (12 posições, 10 usadas)
   **}
  cols := Max(1, Min(4, Ceil(Sqrt(States.Count))));
  rows := Ceil(States.Count / cols);
  
  SetLength(statePos, States.Count);
  
  {**
   * Calcular posição (x,y) de cada estado
   * 
   * Layout: Grid com origem no topo-esquerdo
   * 
   * Para estado i:
   *   row = i div cols  (linha do grid)
   *   col = i mod cols  (coluna do grid)
   *   
   *   x = MarginX + col * SpacingX
   *   y = MarginY + row * SpacingY
   * 
   * IMPORTANTE: Margem horizontal de 60px (não 50px)
   * para dar espaço à seta de estado inicial.
   **}
  for i := 0 to States.Count - 1 do
  begin
    row := i div cols;
    col := i mod cols;
    
    // Posicionar com espaçamento fixo
    // Margem de 60px horizontal para seta inicial
    statePos[i].x := 60 + col * StateSpacingX;
    statePos[i].y := 60 + row * StateSpacingY;
  end;
  
  {**
   * FASE 2: Desenhar transições
   * 
   * OBJETIVO: Desenhar todas as setas de transição ANTES dos estados
   * (para que estados fiquem por cima das linhas)
   * 
   * DOIS CASOS:
   * 
   * 1. SELF-LOOP (estado para si mesmo):
   *    - Desenhar arco/elipse acima do estado
   *    - Posição: (x-12, y-radius-24) até (x+12, y-radius)
   *    - Rótulo: 15px à direita do arco
   * 
   * 2. TRANSIÇÃO NORMAL (estado A → estado B):
   *    - Calcular ângulo: θ = arctan2(Δy, Δx)
   *    - Início: borda do círculo A na direção de B
   *    - Fim: borda do círculo B vindo de A
   *    - Desenhar linha + ponta da seta
   *    - Rótulo: ponto médio entre A e B
   * 
   * GEOMETRIA DA SETA:
   * 
   * Dada transição de (x1,y1) para (x2,y2):
   * 
   * 1. Calcular ângulo:
   *    θ = arctan2(y2-y1, x2-x1)
   * 
   * 2. Ponto inicial (borda do círculo origem):
   *    startX = x1 + radius·cos(θ)
   *    startY = y1 + radius·sin(θ)
   * 
   * 3. Ponto final (borda do círculo destino):
   *    endX = x2 - radius·cos(θ)
   *    endY = y2 - radius·sin(θ)
   * 
   * 4. Ponta da seta (triângulo com 8px de comprimento):
   *    Duas linhas a partir do ponto final:
   *    - Linha 1: ângulo θ - 30° (π/6)
   *    - Linha 2: ângulo θ + 30° (π/6)
   * 
   * EXEMPLO:
   *   Estado A em (60,60) → Estado B em (160,60)
   *   
   *   θ = arctan2(0, 100) = 0 (horizontal direita)
   *   
   *   Start: (60 + 20·cos(0), 60 + 20·sin(0)) = (80, 60)
   *   End:   (160 - 20·cos(0), 60 - 20·sin(0)) = (140, 60)
   *   
   *   Linha horizontal de (80,60) até (140,60)
   *   
   *   Ponta: Duas linhas a partir de (140,60)
   *     - (140 - 8·cos(-30°), 60 - 8·sin(-30°)) ≈ (133, 56)
   *     - (140 - 8·cos(+30°), 60 - 8·sin(+30°)) ≈ (133, 64)
   *   
   *   Rótulo: (110, 52) (meio da linha, 8px acima)
   * 
   * OBSERVAÇÃO:
   *   O método IndexOf retorna -1 se estado não existir.
   *   Continue pula transições inválidas (estados não encontrados).
   **}
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Font.Size := 8;
  
  for i := 0 to High(Transitions) do
  begin
    {** Buscar posição do estado origem **}
    j := States.IndexOf(Transitions[i].FromState);
    if j = -1 then Continue;  // Estado não encontrado, pular
    x1 := statePos[j].x;
    y1 := statePos[j].y;
    
    {** Buscar posição do estado destino **}
    j := States.IndexOf(Transitions[i].ToState);
    if j = -1 then Continue;  // Estado não encontrado, pular
    x2 := statePos[j].x;
    y2 := statePos[j].y;
    
    {**
     * CASO 1: Self-loop (transição para si mesmo)
     * 
     * Quando x1=x2 e y1=y2, desenhar arco acima do estado.
     * 
     * Geometria:
     *   - Elipse de largura 24px, altura 24px
     *   - Posicionada acima do estado (y - radius - 24)
     *   - Rótulo à direita do arco
     **}
    if (x1 = x2) and (y1 = y2) then
    begin
      ACanvas.Ellipse(x1 - 12, y1 - StateRadius - 24, x1 + 12, y1 - StateRadius);
      ACanvas.TextOut(x1 + 15, y1 - StateRadius - 24, Transitions[i].Symbol);
    end
    else
    begin
      {**
       * CASO 2: Transição normal entre dois estados diferentes
       * 
       * Passo 1: Calcular ângulo da linha
       **}
      angle := ArcTan2(y2 - y1, x2 - x1);
      
      {**
       * Passo 2: Calcular ponto inicial (borda do círculo origem)
       * 
       * Usar trigonometria:
       *   x = centro + raio·cos(θ)
       *   y = centro + raio·sin(θ)
       **}
      midX := x1 + Round(StateRadius * Cos(angle));
      midY := y1 + Round(StateRadius * Sin(angle));
      
      {**
       * Passo 3: Calcular ponto final (borda do círculo destino)
       * 
       * Subtrair raio (para parar na borda, não no centro)
       **}
      dx := x2 - Round(StateRadius * Cos(angle));
      dy := y2 - Round(StateRadius * Sin(angle));
      
      {** Passo 4: Desenhar linha da transição **}
      ACanvas.MoveTo(midX, midY);
      ACanvas.LineTo(dx, dy);
      
      {**
       * Passo 5: Desenhar ponta da seta (triângulo)
       * 
       * Duas linhas formando um "V" apontando para o estado destino:
       *   - Linha 1: ângulo - 30° (π/6 radianos)
       *   - Linha 2: ângulo + 30° (π/6 radianos)
       *   - Comprimento: 8 pixels
       * 
       * A ponta é desenhada a partir do ponto final (dx, dy)
       * voltando 8px na direção do ângulo ±30°.
       **}
      arrowLen := 8;
      ACanvas.LineTo(dx - Round(arrowLen * Cos(angle - Pi / 6)),
                    dy - Round(arrowLen * Sin(angle - Pi / 6)));
      ACanvas.MoveTo(dx, dy);
      ACanvas.LineTo(dx - Round(arrowLen * Cos(angle + Pi / 6)),
                    dy - Round(arrowLen * Sin(angle + Pi / 6)));
      
      {**
       * Passo 6: Desenhar rótulo no meio da transição
       * 
       * Posição: Ponto médio entre origem e destino
       * Offset: 8px acima (para não sobrepor a linha)
       **}
      labelX := (x1 + x2) div 2;
      labelY := (y1 + y2) div 2 - 8;
      ACanvas.TextOut(labelX, labelY, Transitions[i].Symbol);
    end;
  end;
  
  {**
   * FASE 3: Desenhar estados
   * 
   * OBJETIVO: Desenhar círculos representando os estados
   * 
   * Estados são desenhados APÓS as transições para ficarem por cima das
   * linhas, garantindo melhor visibilidade.
   * 
   * DOIS TIPOS DE ESTADOS:
   * 
   * 1. ESTADO NORMAL:
   *    - Círculo branco com borda preta
   *    - Raio = 20px
   * 
   * 2. ESTADO FINAL:
   *    - Círculo duplo (dois círculos concêntricos)
   *    - Círculo externo: amarelo, raio 20px
   *    - Círculo interno: branco, raio 18px (20-2)
   *    - Efeito visual: Anel amarelo de 2px
   * 
   * ALGORITMO:
   * 
   * Para cada estado i:
   *   1. Obter posição (x,y) de statePos[i]
   *   
   *   2. Verificar se é estado final:
   *      Se Finals.IndexOf(state) >= 0:
   *        - Desenhar círculo amarelo (raio 20)
   *        - Desenhar círculo branco (raio 18)
   *      Senão:
   *        - Desenhar círculo branco (raio 20)
   *   
   *   3. Se é estado inicial:
   *      - Desenhar seta apontando para o estado
   *      - Seta de 20px à esquerda do estado
   *   
   *   4. Desenhar nome do estado centralizado
   * 
   * RENDERIZAÇÃO DO TEXTO:
   * 
   * Para centralizar o nome do estado:
   *   textWidth = Canvas.TextWidth(nome)
   *   textHeight = Canvas.TextHeight(nome)
   *   
   *   x_texto = x_estado - textWidth/2
   *   y_texto = y_estado - textHeight/2
   * 
   * Importante: Definir Brush.Style = bsClear para que o fundo
   * do texto seja transparente (não apague o círculo).
   * 
   * COMPLEXIDADE: O(n)
   *   - Um loop percorrendo todos os n estados
   *   - Cada operação de desenho é O(1)
   **}
  ACanvas.Pen.Width := 1;
  ACanvas.Font.Size := 8;
  ACanvas.Font.Style := [fsBold];
  
  for i := 0 to States.Count - 1 do
  begin
    x := statePos[i].x;
    y := statePos[i].y;
    
    {**
     * Verificar se é estado final (círculo duplo)
     * 
     * TÉCNICA DO CÍRCULO DUPLO:
     *   1. Desenhar círculo externo amarelo (raio completo)
     *   2. Desenhar círculo interno branco (raio - 2px)
     *   3. Resultado: Anel amarelo de 2px de espessura
     * 
     * GEOMETRIA:
     *   Ellipse(left, top, right, bottom)
     *   Para círculo centrado em (x,y) com raio r:
     *     left = x - r
     *     top = y - r
     *     right = x + r
     *     bottom = y + r
     **}
    if Assigned(Finals) and (Finals.IndexOf(States[i]) >= 0) then
    begin
      {** Círculo externo amarelo **}
      ACanvas.Brush.Color := clYellow;
      ACanvas.Ellipse(x - StateRadius, y - StateRadius, x + StateRadius, y + StateRadius);
      
      {** Círculo interno branco (2px menor) **}
      ACanvas.Brush.Color := clWhite;
      ACanvas.Ellipse(x - StateRadius + 2, y - StateRadius + 2, 
                     x + StateRadius - 2, y + StateRadius - 2);
    end
    else
    begin
      {** Estado normal: círculo branco simples **}
      ACanvas.Brush.Color := clWhite;
      ACanvas.Ellipse(x - StateRadius, y - StateRadius, x + StateRadius, y + StateRadius);
    end;
    
    {**
     * FASE 4: Marcar estado inicial com seta
     * 
     * OBJETIVO: Indicar qual(is) estado(s) são iniciais
     * 
     * GEOMETRIA DA SETA INICIAL:
     *   - Linha horizontal da esquerda apontando para o estado
     *   - Começa 20px à esquerda da borda do estado
     *   - Termina 3px antes da borda (para não tocar)
     *   - Ponta da seta: duas linhas de 4px formando "V"
     * 
     * COORDENADAS:
     *   Linha horizontal:
     *     De: (x - radius - 20, y)
     *     Para: (x - radius - 3, y)
     *   
     *   Ponta superior:
     *     De: (x - radius - 3, y)
     *     Para: (x - radius - 7, y - 4)
     *   
     *   Ponta inferior:
     *     De: (x - radius - 3, y)
     *     Para: (x - radius - 7, y + 4)
     * 
     * Resultado: Seta → apontando para o estado inicial
     **}
    if Assigned(Initials) and (Initials.IndexOf(States[i]) >= 0) then
    begin
      {** Linha horizontal **}
      ACanvas.MoveTo(x - StateRadius - 20, y);
      ACanvas.LineTo(x - StateRadius - 3, y);
      
      {** Ponta superior **}
      ACanvas.LineTo(x - StateRadius - 7, y - 4);
      
      {** Voltar ao fim da linha para desenhar ponta inferior **}
      ACanvas.MoveTo(x - StateRadius - 3, y);
      ACanvas.LineTo(x - StateRadius - 7, y + 4);
    end;
    
    {**
     * Desenhar nome do estado centralizado
     * 
     * CENTRALIZAÇÃO:
     *   Canvas.TextWidth(text) retorna largura em pixels
     *   Canvas.TextHeight(text) retorna altura em pixels
     *   
     *   Para centralizar:
     *     x_texto = x_círculo - largura/2
     *     y_texto = y_círculo - altura/2
     * 
     * BACKGROUND TRANSPARENTE:
     *   bsClear = não preencher fundo do texto
     *   (evita apagar o círculo ao desenhar o texto)
     **}
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(x - ACanvas.TextWidth(States[i]) div 2, 
                   y - ACanvas.TextHeight(States[i]) div 2, 
                   States[i]);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

end.

