unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Math, ComCtrls;

type
  { Tipos auxiliares }
  TTransition = record
    FromState, Symbol, ToState: string;
  end;
  
  TTransitionArray = array of TTransition;
  
  { TFormMain }
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
    // Dados do AFN-ε (entrada)
    EpsilonNFAAlphabet, EpsilonNFAStates, EpsilonNFAInitials, EpsilonNFAFinals: TStringList;
    EpsilonNFATransitions: TTransitionArray;
    // Dados do AFN (sem epsilon)
    NFAAlphabet, NFAStates, NFAInitials, NFAFinals: TStringList;
    NFATransitions: TTransitionArray;
    // Dados do AFD
    DFAStates: TStringList;
    DFATransitions: TTransitionArray;
    DFAFinals: TStringList;
    DFAInitial: string;
    // Dados do AFD Minimizado
    MinDFAStates: TStringList;
    MinDFATransitions: TTransitionArray;
    MinDFAFinals: TStringList;
    MinDFAInitial: string;
    
    // ComboBox para testes
    cmbTestFiles: TComboBox;
    
    procedure RemoveEpsilonTransitions;
    procedure ConvertAFNtoAFD;
    procedure MinimizeDFA;
    procedure LoadSampleFile;
    procedure LoadTestFiles;
    procedure OnTestFileSelected(Sender: TObject);
    procedure DrawAutomaton(ACanvas: TCanvas; States, Initials, Finals: TStringList;
      const Transitions: TTransitionArray);
  public
  end;

  { TStringSet - classe auxiliar para conjuntos de strings }
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

{ TStringSet }

constructor TStringSet.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupIgnore;
end;

destructor TStringSet.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TStringSet.Add(const s: string);
begin
  if s = '' then Exit;
  FList.Add(s);
end;

function TStringSet.Contains(const s: string): Boolean;
begin
  Result := FList.IndexOf(s) >= 0;
end;

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

function TStringSet.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

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

function TStringSet.Count: Integer;
begin
  Result := FList.Count;
end;

function TStringSet.Item(i: Integer): string;
begin
  Result := FList[i];
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := 'Conversor AFN-ε → AFN → AFD → MinDFA';
  memoInput.Lines.Clear;
  
  // Inicializar listas
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
  
  // Desabilitar botão "Usar como Input" inicialmente
  if Assigned(btnUseAsInput) then
    btnUseAsInput.Enabled := False;
  
  // Criar ComboBox para arquivos de teste
  cmbTestFiles := TComboBox.Create(Self);
  cmbTestFiles.Parent := Panel1;
  cmbTestFiles.Left := edtFilePath.Left + edtFilePath.Width + 8;
  cmbTestFiles.Top := edtFilePath.Top;
  cmbTestFiles.Width := btnLoadFile.Left - (edtFilePath.Left + edtFilePath.Width) - 16;
  cmbTestFiles.Style := csDropDownList;
  cmbTestFiles.OnChange := @OnTestFileSelected;
  
  // Carregar lista de arquivos de teste
  LoadTestFiles;
  
  // Carregar arquivo exemplo se existir
  LoadSampleFile;
end;

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

procedure TFormMain.LoadSampleFile;
var
  samplePath: string;
begin
  samplePath := ExtractFilePath(Application.ExeName) + '..\src\sample_afn.txt';
  if not FileExists(samplePath) then
    samplePath := 'src\sample_afn.txt';
  
  if FileExists(samplePath) then
  begin
    memoInput.Lines.LoadFromFile(samplePath);
    edtFilePath.Text := samplePath;
  end
  else
  begin
    // Exemplo padrão se não encontrar o arquivo
    memoInput.Lines.Add('a b');
    memoInput.Lines.Add('q0 q1 q2');
    memoInput.Lines.Add('q0');
    memoInput.Lines.Add('q2');
    memoInput.Lines.Add('q0 a q0');
    memoInput.Lines.Add('q0 b q0');
    memoInput.Lines.Add('q0 a q1');
    memoInput.Lines.Add('q1 b q2');
    edtFilePath.Text := '(exemplo padrão)';
  end;
end;

procedure TFormMain.btnLoadFileClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Arquivos de texto (*.txt)|*.txt|Todos os arquivos (*.*)|*.*';
  OpenDialog.DefaultExt := 'txt';
  
  if OpenDialog.Execute then
  begin
    WriteLn('[GUI] Carregando arquivo: ', OpenDialog.FileName);
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

procedure TFormMain.btnClearClick(Sender: TObject);
begin
  WriteLn('[GUI] Limpando interface e dados...');
  memoInput.Lines.Clear;
  memoOutput.Lines.Clear;
  memoMinOutput.Lines.Clear;
  memoNFAOutput.Lines.Clear;
  edtFilePath.Text := '';
  
  // Limpar dados dos diagramas
  if Assigned(EpsilonNFAAlphabet) then EpsilonNFAAlphabet.Clear;
  if Assigned(EpsilonNFAStates) then EpsilonNFAStates.Clear;
  if Assigned(EpsilonNFAInitials) then EpsilonNFAInitials.Clear;
  if Assigned(EpsilonNFAFinals) then EpsilonNFAFinals.Clear;
  if Assigned(NFAAlphabet) then NFAAlphabet.Clear;
  if Assigned(NFAStates) then NFAStates.Clear;
  if Assigned(NFAInitials) then NFAInitials.Clear;
  if Assigned(NFAFinals) then NFAFinals.Clear;
  if Assigned(DFAStates) then DFAStates.Clear;
  if Assigned(DFAFinals) then DFAFinals.Clear;
  if Assigned(MinDFAStates) then MinDFAStates.Clear;
  if Assigned(MinDFAFinals) then MinDFAFinals.Clear;
  SetLength(EpsilonNFATransitions, 0);
  SetLength(NFATransitions, 0);
  SetLength(DFATransitions, 0);
  SetLength(MinDFATransitions, 0);
  DFAInitial := '';
  MinDFAInitial := '';
  
  // Desabilitar botões
  if Assigned(btnMinimize) then btnMinimize.Enabled := False;
  if Assigned(btnUseAsInput) then btnUseAsInput.Enabled := False;
  
  // Redesenhar os painéis vazios
  if Assigned(PaintBoxEpsilonNFA) then PaintBoxEpsilonNFA.Invalidate;
  if Assigned(PaintBoxNFA) then PaintBoxNFA.Invalidate;
  if Assigned(PaintBoxDFA) then PaintBoxDFA.Invalidate;
  if Assigned(PaintBoxMinDFA) then PaintBoxMinDFA.Invalidate;
  WriteLn('[GUI] Limpeza concluida.');
end;

procedure TFormMain.btnRemoveEpsilonClick(Sender: TObject);
begin
  WriteLn('[GUI] Iniciando remocao de epsilon-transicoes...');
  try
    RemoveEpsilonTransitions;
    WriteLn('[GUI] Remocao de epsilon concluida com sucesso!');
  except
    on E: Exception do
    begin
      WriteLn('[ERRO] Falha na remocao de epsilon: ', E.Message);
      memoNFAOutput.Lines.Clear;
      memoNFAOutput.Lines.Add('ERRO: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.btnUseAsInputClick(Sender: TObject);
var
  i: Integer;
begin
  WriteLn('[GUI] Usando AFN resultante como entrada...');
  
  // Limpar entrada atual
  memoInput.Lines.Clear;
  
  // Copiar AFN sem epsilon para a entrada
  // Linha 0: alfabeto
  memoInput.Lines.Add(NFAAlphabet.CommaText.Replace(',', ' '));
  
  // Linha 1: estados
  memoInput.Lines.Add(NFAStates.CommaText.Replace(',', ' '));
  
  // Linha 2: iniciais
  memoInput.Lines.Add(NFAInitials.CommaText.Replace(',', ' '));
  
  // Linha 3: finais
  memoInput.Lines.Add(NFAFinals.CommaText.Replace(',', ' '));
  
  // Linhas 4+: transições
  for i := 0 to High(NFATransitions) do
    memoInput.Lines.Add(NFATransitions[i].FromState + ' ' + 
                       NFATransitions[i].Symbol + ' ' + 
                       NFATransitions[i].ToState);
  
  WriteLn('[GUI] AFN copiado para entrada. Pronto para converter AFN -> AFD.');
  ShowMessage('AFN sem epsilon copiado para a entrada!' + LineEnding +
              'Agora você pode clicar em "AFN → AFD" para converter.');
end;

procedure TFormMain.btnConvertClick(Sender: TObject);
begin
  WriteLn('[GUI] Iniciando conversao AFN -> AFD...');
  try
    ConvertAFNtoAFD;
    WriteLn('[GUI] Conversao concluida com sucesso!');
  except
    on E: Exception do
    begin
      WriteLn('[ERRO] Falha na conversao: ', E.Message);
      memoOutput.Lines.Clear;
      memoOutput.Lines.Add('ERRO: ' + E.Message);
    end;
  end;
end;

function GetSetName(setObj: TStringSet): string;
begin
  Result := setObj.ToString;
end;

{ Calcula o epsilon-fecho (epsilon-closure) de um conjunto de estados }
function ComputeEpsilonClosure(const states: TStringSet; 
                               const transitions: TTransitionArray): TStringSet;
var
  stack: array of string;
  stackTop: Integer;
  current, target: string;
  i: Integer;
begin
  Result := TStringSet.Create;
  
  // Inicializar com os estados fornecidos
  for i := 0 to states.Count - 1 do
  begin
    Result.Add(states.Item(i));
  end;
  
  // Usar pilha para processar epsilon-transições
  SetLength(stack, 1000);
  stackTop := 0;
  
  // Adicionar todos os estados iniciais à pilha
  for i := 0 to states.Count - 1 do
  begin
    stack[stackTop] := states.Item(i);
    Inc(stackTop);
  end;
  
  // Processar pilha
  while stackTop > 0 do
  begin
    Dec(stackTop);
    current := stack[stackTop];
    
    // Procurar todas as epsilon-transições do estado atual
    for i := 0 to High(transitions) do
    begin
      if (transitions[i].FromState = current) and 
         ((transitions[i].Symbol = 'ε') or (transitions[i].Symbol = 'epsilon') or 
          (transitions[i].Symbol = 'e') or (transitions[i].Symbol = '&')) then
      begin
        target := transitions[i].ToState;
        
        // Se ainda não está no fecho, adicionar
        if not Result.Contains(target) then
        begin
          Result.Add(target);
          stack[stackTop] := target;
          Inc(stackTop);
        end;
      end;
    end;
  end;
end;

{ Remove epsilon-transições de um AFN-ε, convertendo-o em AFN }
procedure TFormMain.RemoveEpsilonTransitions;
var
  Alphabet, States, Initials, Finals: TStringList;
  Transitions: array of TTransition;
  parts: TStringList;
  i, j, k, m, tStart: Integer;
  line: string;
  newInitials, newFinals: TStringList;
  initialClosure, stateClosure, tempClosure, destClosure: TStringSet;
  newTransitions: array of TTransition;
  transCount: Integer;
  state, symbol, targetState: string;
  targetClosure: TStringSet;
  hasEpsilon: Boolean;
begin
  memoNFAOutput.Lines.Clear;
  
  // Limpar dados anteriores do AFN-ε
  if Assigned(EpsilonNFAAlphabet) then EpsilonNFAAlphabet.Free;
  if Assigned(EpsilonNFAStates) then EpsilonNFAStates.Free;
  if Assigned(EpsilonNFAInitials) then EpsilonNFAInitials.Free;
  if Assigned(EpsilonNFAFinals) then EpsilonNFAFinals.Free;
  
  // Limpar dados anteriores do AFN (sem epsilon)
  if Assigned(NFAAlphabet) then NFAAlphabet.Free;
  if Assigned(NFAStates) then NFAStates.Free;
  if Assigned(NFAInitials) then NFAInitials.Free;
  if Assigned(NFAFinals) then NFAFinals.Free;
  
  EpsilonNFAAlphabet := TStringList.Create;
  EpsilonNFAStates := TStringList.Create;
  EpsilonNFAInitials := TStringList.Create;
  EpsilonNFAFinals := TStringList.Create;
  
  NFAAlphabet := TStringList.Create;
  NFAStates := TStringList.Create;
  NFAInitials := TStringList.Create;
  NFAFinals := TStringList.Create;
  
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
    
    // Linha 0: alfabeto (pode incluir epsilon)
    parts.DelimitedText := Trim(memoInput.Lines[0]);
    hasEpsilon := False;
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        Alphabet.Add(parts[i]);
        EpsilonNFAAlphabet.Add(parts[i]);
        
        // Verificar se tem epsilon
        if (parts[i] = 'ε') or (parts[i] = 'epsilon') or 
           (parts[i] = 'e') or (parts[i] = '&') then
          hasEpsilon := True
        else
          NFAAlphabet.Add(parts[i]); // Não adicionar epsilon ao novo alfabeto
      end;
    end;
    
    // Linha 1: estados
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
    
    // Linha 2: iniciais
    parts.DelimitedText := Trim(memoInput.Lines[2]);
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        Initials.Add(parts[i]);
        EpsilonNFAInitials.Add(parts[i]);
      end;
    end;
    
    // Linha 3: finais
    parts.DelimitedText := Trim(memoInput.Lines[3]);
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        Finals.Add(parts[i]);
        EpsilonNFAFinals.Add(parts[i]);
      end;
    end;
    
    // Linhas restantes: transições
    tStart := 4;
    SetLength(Transitions, Max(0, memoInput.Lines.Count - tStart));
    SetLength(EpsilonNFATransitions, Max(0, memoInput.Lines.Count - tStart));
    for i := tStart to memoInput.Lines.Count - 1 do
    begin
      line := Trim(memoInput.Lines[i]);
      if line = '' then Continue;
      
      parts.DelimitedText := line;
      if parts.Count >= 3 then
      begin
        Transitions[i - tStart].FromState := parts[0];
        Transitions[i - tStart].Symbol := parts[1];
        Transitions[i - tStart].ToState := parts[2];
        
        EpsilonNFATransitions[i - tStart].FromState := parts[0];
        EpsilonNFATransitions[i - tStart].Symbol := parts[1];
        EpsilonNFATransitions[i - tStart].ToState := parts[2];
      end;
    end;
    
    // LOG: AFN-ε de entrada
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
    
    if not hasEpsilon then
    begin
      ShowMessage('Aviso: O autômato não possui epsilon-transições!' + LineEnding +
                  'Use símbolos: ε, epsilon, e, ou &');
      // Mesmo assim, continuar com a conversão
    end;
    
    // Calcular novos estados iniciais (epsilon-fecho dos iniciais)
    newInitials := TStringList.Create;
    tempClosure := TStringSet.Create;
    try
      for i := 0 to Initials.Count - 1 do
        tempClosure.Add(Initials[i]);
      
      initialClosure := ComputeEpsilonClosure(tempClosure, Transitions);
      tempClosure.Free;
      
      try
        for i := 0 to initialClosure.Count - 1 do
        begin
          newInitials.Add(initialClosure.Item(i));
          NFAInitials.Add(initialClosure.Item(i));
        end;
        
        WriteLn('NOVOS ESTADOS INICIAIS (com epsilon-fecho): ', newInitials.CommaText);
      finally
        initialClosure.Free;
      end;
    except
      tempClosure.Free;
      raise;
    end;
    
    // Calcular novos estados finais
    // Um estado é final se seu epsilon-fecho contém algum estado final original
    newFinals := TStringList.Create;
    try
      for i := 0 to States.Count - 1 do
      begin
        state := States[i];
        tempClosure := TStringSet.Create;
        tempClosure.Add(state);
        stateClosure := ComputeEpsilonClosure(tempClosure, Transitions);
        tempClosure.Free;
        
        try
          // Verificar se o fecho contém algum estado final
          for j := 0 to Finals.Count - 1 do
          begin
            if stateClosure.Contains(Finals[j]) then
            begin
              if newFinals.IndexOf(state) < 0 then
              begin
                newFinals.Add(state);
                NFAFinals.Add(state);
              end;
              Break;
            end;
          end;
        finally
          stateClosure.Free;
        end;
      end;
      
      WriteLn('NOVOS ESTADOS FINAIS: ', newFinals.CommaText);
    finally
    end;
    
    // Construir novas transições (sem epsilon)
    SetLength(newTransitions, States.Count * NFAAlphabet.Count * States.Count);
    transCount := 0;
    
    for i := 0 to States.Count - 1 do
    begin
      state := States[i];
      
      // Calcular epsilon-fecho do estado
      stateClosure := TStringSet.Create;
      stateClosure.Add(state);
      tempClosure := ComputeEpsilonClosure(stateClosure, Transitions);
      stateClosure.Free;
      stateClosure := tempClosure;
      
      try
        // Para cada símbolo do alfabeto (exceto epsilon)
        for j := 0 to NFAAlphabet.Count - 1 do
        begin
          symbol := NFAAlphabet[j];
          
          // Conjunto de estados alcançáveis
          targetClosure := TStringSet.Create;
          try
            // Para cada estado no epsilon-fecho
            for k := 0 to stateClosure.Count - 1 do
            begin
              targetState := stateClosure.Item(k);
              
              // Procurar transições com o símbolo
              for tStart := 0 to High(Transitions) do
              begin
                if (Transitions[tStart].FromState = targetState) and 
                   (Transitions[tStart].Symbol = symbol) then
                begin
                  // Adicionar epsilon-fecho do estado destino
                  tempClosure := TStringSet.Create;
                  tempClosure.Add(Transitions[tStart].ToState);
                  try
                    destClosure := ComputeEpsilonClosure(tempClosure, Transitions);
                    try
                      for m := 0 to destClosure.Count - 1 do
                        targetClosure.Add(destClosure.Item(m));
                    finally
                      destClosure.Free;
                    end;
                  finally
                    tempClosure.Free;
                  end;
                end;
              end;
            end;
            
            // Adicionar transições para cada estado alcançável
            for k := 0 to targetClosure.Count - 1 do
            begin
              newTransitions[transCount].FromState := state;
              newTransitions[transCount].Symbol := symbol;
              newTransitions[transCount].ToState := targetClosure.Item(k);
              Inc(transCount);
            end;
          finally
            targetClosure.Free;
          end;
        end;
      finally
        stateClosure.Free;
      end;
    end;
    
    SetLength(newTransitions, transCount);
    SetLength(NFATransitions, transCount);
    for i := 0 to transCount - 1 do
      NFATransitions[i] := newTransitions[i];
    
    // LOG: AFN resultante
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
    
    // Exibir resultado no memo
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
    
    // Mudar para a aba do AFN resultante
    PageControl2.ActivePage := TabNFAOutput;
    PageControl1.ActivePage := TabNFA;
    
    // Habilitar botão "Usar como Input"
    btnUseAsInput.Enabled := True;
    
    // Redesenhar diagramas
    PaintBoxEpsilonNFA.Invalidate;
    PaintBoxNFA.Invalidate;
    
  finally
    Alphabet.Free;
    States.Free;
    Initials.Free;
    Finals.Free;
    parts.Free;
    newInitials.Free;
    newFinals.Free;
  end;
end;

procedure TFormMain.ConvertAFNtoAFD;
var
  Alphabet, States, Initials, Finals: TStringList;
  Transitions: array of TTransition;
  parts: TStringList;
  i, j, k, tStart: Integer;
  localDFAStates, workQ, dfaMap, isFinal: TStringList;
  curSet, nextSet, cloneSet: TStringSet;
  sym, key: string;
  localDFATransitions: TTransitionArray;
  hasFinal, hasEpsilon: Boolean;
  line: string;
begin
  memoOutput.Lines.Clear;
  
  // Limpar dados anteriores
  if Assigned(NFAAlphabet) then NFAAlphabet.Free;
  if Assigned(NFAStates) then NFAStates.Free;
  if Assigned(NFAInitials) then NFAInitials.Free;
  if Assigned(NFAFinals) then NFAFinals.Free;
  if Assigned(DFAStates) then DFAStates.Free;
  if Assigned(DFAFinals) then DFAFinals.Free;
  
  NFAAlphabet := TStringList.Create;
  NFAStates := TStringList.Create;
  NFAInitials := TStringList.Create;
  NFAFinals := TStringList.Create;
  DFAStates := TStringList.Create;
  DFAFinals := TStringList.Create;
  
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
    
    // Linha 0: alfabeto
    parts.DelimitedText := Trim(memoInput.Lines[0]);
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> '' then 
      begin
        // Filtrar epsilon do alfabeto - AFD não pode ter epsilon
        if (parts[i] <> 'ε') and (parts[i] <> 'epsilon') and 
           (parts[i] <> 'e') and (parts[i] <> '&') then
        begin
          Alphabet.Add(parts[i]);
          NFAAlphabet.Add(parts[i]);
        end;
      end;
    end;
    
    // Linha 1: estados
    parts.DelimitedText := Trim(memoInput.Lines[1]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then 
      begin
        States.Add(parts[i]);
        NFAStates.Add(parts[i]);
      end;
    
    // Linha 2: iniciais
    parts.DelimitedText := Trim(memoInput.Lines[2]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then 
      begin
        Initials.Add(parts[i]);
        NFAInitials.Add(parts[i]);
      end;
    
    // Linha 3: finais
    parts.DelimitedText := Trim(memoInput.Lines[3]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then 
      begin
        Finals.Add(parts[i]);
        NFAFinals.Add(parts[i]);
      end;
    
    // Linhas restantes: transições
    tStart := 4;
    SetLength(Transitions, Max(0, memoInput.Lines.Count - tStart));
    SetLength(NFATransitions, Max(0, memoInput.Lines.Count - tStart));
    
    // Verificar se há epsilon-transições
    hasEpsilon := False;
    for i := tStart to memoInput.Lines.Count - 1 do
    begin
      line := Trim(memoInput.Lines[i]);
      if line = '' then Continue;
      
      parts.DelimitedText := line;
      if parts.Count >= 3 then
      begin
        // Verificar se é epsilon-transição
        if (parts[1] = 'ε') or (parts[1] = 'epsilon') or 
           (parts[1] = 'e') or (parts[1] = '&') then
        begin
          hasEpsilon := True;
        end;
        
        Transitions[i - tStart].FromState := parts[0];
        Transitions[i - tStart].Symbol := parts[1];
        Transitions[i - tStart].ToState := parts[2];
        
        NFATransitions[i - tStart].FromState := parts[0];
        NFATransitions[i - tStart].Symbol := parts[1];
        NFATransitions[i - tStart].ToState := parts[2];
      end;
    end;
    
    // Avisar se houver epsilon-transições
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
    
    // LOG: AFN de entrada
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
    
    // Construção do DFA via subset construction
    localDFAStates := TStringList.Create;
    workQ := TStringList.Create;
    dfaMap := TStringList.Create;
    dfaMap.OwnsObjects := True;
    isFinal := TStringList.Create;
    
    try
      // Estado inicial do DFA = conjunto de estados iniciais do AFN
      curSet := TStringSet.Create;
      for i := 0 to Initials.Count - 1 do
        curSet.Add(Initials[i]);
      
      localDFAStates.Add(GetSetName(curSet));
      workQ.Add(GetSetName(curSet));
      dfaMap.AddObject(GetSetName(curSet), curSet);
      
      SetLength(localDFATransitions, 0);
      
      while workQ.Count > 0 do
      begin
        key := workQ[0];
        workQ.Delete(0);
        curSet := TStringSet(dfaMap.Objects[dfaMap.IndexOf(key)]);
        
        // Marcar como final se contém algum estado final do AFN
        hasFinal := False;
        for i := 0 to Finals.Count - 1 do
          if curSet.Contains(Finals[i]) then hasFinal := True;
        if hasFinal then isFinal.Add(key);
        
        // Transições para cada símbolo do alfabeto
        for i := 0 to Alphabet.Count - 1 do
        begin
          sym := Alphabet[i];
          nextSet := TStringSet.Create;
          try
            // União dos moves
            for j := 0 to curSet.Count - 1 do
            begin
              for k := 0 to High(Transitions) do
              begin
                if Transitions[k].FromState = curSet.Item(j) then
                  if Transitions[k].Symbol = sym then
                    nextSet.Add(Transitions[k].ToState);
              end;
            end;
            
            if nextSet.IsEmpty then
              key := '{}'
            else
              key := GetSetName(nextSet);
            
            // Registrar transição
            SetLength(localDFATransitions, Length(localDFATransitions) + 1);
            localDFATransitions[High(localDFATransitions)].FromState := GetSetName(curSet);
            localDFATransitions[High(localDFATransitions)].Symbol := sym;
            localDFATransitions[High(localDFATransitions)].ToState := key;
            
            // Também armazenar no campo privado
            SetLength(DFATransitions, Length(DFATransitions) + 1);
            DFATransitions[High(DFATransitions)].FromState := GetSetName(curSet);
            DFATransitions[High(DFATransitions)].Symbol := sym;
            DFATransitions[High(DFATransitions)].ToState := key;
            
            // Se é novo estado do DFA, adicionar
            if (dfaMap.IndexOf(key) = -1) and (localDFAStates.IndexOf(key) = -1) then
            begin
              if key <> '{}' then
              begin
                cloneSet := nextSet.Clone;
                dfaMap.AddObject(key, cloneSet);
              end;
              localDFAStates.Add(key);
              if key <> '{}' then
                workQ.Add(key);
            end;
            
          finally
            nextSet.Free;
          end;
        end;
      end;
      
      // Exibir resultado
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('    RESULTADO DA CONVERSÃO AFN → AFD');
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('');
      
      // LOG: AFD resultante
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
      
      memoOutput.Lines.Add('📋 ALFABETO: ' + Alphabet.CommaText);
      memoOutput.Lines.Add('');
      
      memoOutput.Lines.Add('🔵 ESTADOS DO DFA:');
      for i := 0 to localDFAStates.Count - 1 do
      begin
        memoOutput.Lines.Add('   • ' + localDFAStates[i]);
        DFAStates.Add(localDFAStates[i]);
      end;
      memoOutput.Lines.Add('');
      
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
      
      memoOutput.Lines.Add('🎯 ESTADOS FINAIS:');
      if isFinal.Count > 0 then
      begin
        for i := 0 to isFinal.Count - 1 do
        begin
          memoOutput.Lines.Add('   • ' + isFinal[i]);
          DFAFinals.Add(isFinal[i]);
        end;
      end
      else
        memoOutput.Lines.Add('   (nenhum)');
      memoOutput.Lines.Add('');
      
      memoOutput.Lines.Add('➡️  TRANSIÇÕES:');
      for i := 0 to High(localDFATransitions) do
        memoOutput.Lines.Add('   ' + localDFATransitions[i].FromState + ' --' + 
                            localDFATransitions[i].Symbol + '--> ' + 
                            localDFATransitions[i].ToState);
      
      memoOutput.Lines.Add('');
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('✅ Conversão concluída com sucesso!');
      
      // Habilitar botão de minimização
      if Assigned(btnMinimize) then
        btnMinimize.Enabled := True;
      
      // Mudar para a aba de resultado AFD
      if Assigned(PageControl2) then
        PageControl2.ActivePage := TabOutput;
      
      // Redesenhar os diagramas
      if Assigned(PaintBoxNFA) then PaintBoxNFA.Invalidate;
      if Assigned(PaintBoxDFA) then PaintBoxDFA.Invalidate;
      
    finally
      localDFAStates.Free;
      workQ.Free;
      dfaMap.Free;
      isFinal.Free;
    end;
    
  finally
    Alphabet.Free;
    States.Free;
    Initials.Free;
    Finals.Free;
    parts.Free;
  end;
end;

procedure TFormMain.PaintBoxEpsilonNFAPaint(Sender: TObject);
begin
  if not Assigned(EpsilonNFAStates) then Exit;
  DrawAutomaton(PaintBoxEpsilonNFA.Canvas, EpsilonNFAStates, EpsilonNFAInitials, 
                EpsilonNFAFinals, EpsilonNFATransitions);
end;

procedure TFormMain.PaintBoxNFAPaint(Sender: TObject);
begin
  if not Assigned(NFAStates) then Exit;
  DrawAutomaton(PaintBoxNFA.Canvas, NFAStates, NFAInitials, NFAFinals, NFATransitions);
end;

procedure TFormMain.PaintBoxDFAPaint(Sender: TObject);
var
  Initials: TStringList;
begin
  if not Assigned(DFAStates) or (DFAStates.Count = 0) then Exit;
  
  Initials := TStringList.Create;
  try
    // Estado inicial é sempre o primeiro
    if DFAStates.Count > 0 then
      Initials.Add(DFAStates[0]);
    
    DrawAutomaton(PaintBoxDFA.Canvas, DFAStates, Initials, DFAFinals, DFATransitions);
  finally
    Initials.Free;
  end;
end;

procedure TFormMain.PaintBoxMinDFAPaint(Sender: TObject);
var
  Initials: TStringList;
begin
  if not Assigned(MinDFAStates) or (MinDFAStates.Count = 0) then Exit;
  
  Initials := TStringList.Create;
  try
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
  if not Assigned(DFAStates) or (DFAStates.Count = 0) then
  begin
    ShowMessage('Nenhum AFD para minimizar! Execute a conversão AFN → AFD primeiro.');
    Exit;
  end;
  
  WriteLn('[GUI] AFD antes da minimizacao: ', DFAStates.Count, ' estados');
  WriteLn('-------------------------------------------');
  WriteLn('INICIANDO MINIMIZACAO DO AFD:');
  WriteLn('  Estados do AFD: ', DFAStates.Count);
  WriteLn('  Estados finais: ', DFAFinals.Count);
  WriteLn('  Transicoes: ', Length(DFATransitions));
  WriteLn('-------------------------------------------');
  WriteLn('');
  
  // Limpar dados anteriores
  MinDFAStates.Clear;
  MinDFAFinals.Clear;
  SetLength(MinDFATransitions, 0);
  MinDFAInitial := '';
  
  partitions := TList.Create;
  partitionNames := TStringList.Create;
  try
    // Inicialização: dividir em estados finais e não-finais
    SetLength(statePartition, DFAStates.Count);
    
    // Partição 0: estados não-finais
    partition := TStringList.Create;
    partitions.Add(partition);
    for i := 0 to DFAStates.Count - 1 do
      if DFAFinals.IndexOf(DFAStates[i]) = -1 then
      begin
        partition.Add(DFAStates[i]);
        statePartition[i] := 0;
      end;
    
    // Partição 1: estados finais
    partition := TStringList.Create;
    partitions.Add(partition);
    for i := 0 to DFAStates.Count - 1 do
      if DFAFinals.IndexOf(DFAStates[i]) >= 0 then
      begin
        partition.Add(DFAStates[i]);
        statePartition[i] := 1;
      end;
    
    WriteLn('[GUI] Particao inicial: ', partitions.Count, ' grupos');
    
    // Refinamento iterativo das partições
    repeat
      partitionsChanged := False;
      newPartitions := TList.Create;
      
      for partIdx1 := 0 to partitions.Count - 1 do
      begin
        partition := TStringList(partitions[partIdx1]);
        if partition.Count <= 1 then
        begin
          // Partição com 1 estado não pode ser dividida
          newPartitions.Add(partition);
          Continue;
        end;
        
        // Tentar dividir esta partição
        i := 0;
        while i < partition.Count do
        begin
          state1 := partition[i];
          
          // Criar nova partição com estados equivalentes a state1
          TStringList(partitions[partIdx1]).Delete(i);
          
          equiv := True;
          j := i;
          while j < partition.Count do
          begin
            state2 := partition[j];
            equiv := True;
            
            // Verificar equivalência para cada símbolo
            for k := 0 to NFAAlphabet.Count - 1 do
            begin
              sym := NFAAlphabet[k];
              
              // Encontrar destinos de state1 e state2 com símbolo sym
              dest1 := '';
              dest2 := '';
              
              for trans in DFATransitions do
              begin
                if (trans.FromState = state1) and (trans.Symbol = sym) then
                  dest1 := trans.ToState;
                if (trans.FromState = state2) and (trans.Symbol = sym) then
                  dest2 := trans.ToState;
              end;
              
              // Verificar se destinos estão na mesma partição
              if (dest1 <> '') and (dest2 <> '') then
              begin
                partIdx2 := statePartition[DFAStates.IndexOf(dest1)];
                m := statePartition[DFAStates.IndexOf(dest2)];
                if partIdx2 <> m then
                begin
                  equiv := False;
                  Break;
                end;
              end
              else if dest1 <> dest2 then
              begin
                equiv := False;
                Break;
              end;
            end;
            
            if not equiv then
              Inc(j)
            else
            begin
              partition.Delete(j);
              partitionsChanged := True;
            end;
          end;
          
          // Adicionar state1 de volta
          partition.Insert(i, state1);
          Inc(i);
        end;
        
        newPartitions.Add(partition);
      end;
      
      partitions.Free;
      partitions := newPartitions;
      
      // Atualizar mapeamento de estados para partições
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
      
    until not partitionsChanged;
    
    WriteLn('[GUI] Particoes finais: ', partitions.Count, ' grupos');
    WriteLn('[GUI] Criando estados minimizados...');
    
    // Criar estados minimizados (cada partição vira um estado)
    for i := 0 to partitions.Count - 1 do
    begin
      partition := TStringList(partitions[i]);
      if partition.Count > 0 then
      begin
        // Nome da partição: concatenar estados
        if partition.Count = 1 then
          partName := partition[0]
        else
        begin
          partName := '[';
          for j := 0 to partition.Count - 1 do
          begin
            if j > 0 then partName += ',';
            partName += partition[j];
          end;
          partName += ']';
        end;
        
        MinDFAStates.Add(partName);
        partitionNames.Add(partName);
        
        // Verificar se é estado final
        for j := 0 to partition.Count - 1 do
          if DFAFinals.IndexOf(partition[j]) >= 0 then
          begin
            MinDFAFinals.Add(partName);
            Break;
          end;
        
        // Verificar se contém estado inicial
        for j := 0 to partition.Count - 1 do
          if partition[j] = DFAInitial then
          begin
            MinDFAInitial := partName;
            Break;
          end;
      end;
    end;
    
    // Criar transições minimizadas
    for i := 0 to partitions.Count - 1 do
    begin
      partition := TStringList(partitions[i]);
      if partition.Count = 0 then Continue;
      
      mappedState := partitionNames[i];
      state1 := partition[0]; // Representante da partição
      
      // Para cada símbolo, encontrar transição
      for k := 0 to NFAAlphabet.Count - 1 do
      begin
        sym := NFAAlphabet[k];
        dest1 := '';
        
        // Encontrar destino do representante
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
          // Mapear destino para sua partição
          oldInitialPartition := statePartition[DFAStates.IndexOf(dest1)];
          mappedDest := partitionNames[oldInitialPartition];
          
          // Adicionar transição
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

procedure TFormMain.DrawAutomaton(ACanvas: TCanvas; States, Initials, Finals: TStringList;
  const Transitions: TTransitionArray);
const
  StateRadius = 20;
  StateSpacingX = 100;  // Espaçamento horizontal entre estados
  StateSpacingY = 80;   // Espaçamento vertical entre estados
  MarginX = 50;
  MarginY = 40;
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
  // Limpar canvas
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(0, 0, ACanvas.Width, ACanvas.Height);
  
  if States.Count = 0 then
  begin
    ACanvas.TextOut(10, 10, 'Nenhum autômato para exibir');
    Exit;
  end;
  
  // Calcular grid de posições
  cols := Max(1, Min(4, Ceil(Sqrt(States.Count)))); // Máximo 4 colunas
  rows := Ceil(States.Count / cols);
  
  SetLength(statePos, States.Count);
  
  // Posicionar a partir do topo-esquerdo com margem maior para a seta inicial
  for i := 0 to States.Count - 1 do
  begin
    row := i div cols;
    col := i mod cols;
    
    // Posicionar usando espaçamento fixo a partir da margem superior esquerda
    // Margem de 60px horizontal para dar espaço à seta de estado inicial
    statePos[i].x := 60 + col * StateSpacingX;
    statePos[i].y := 60 + row * StateSpacingY;
  end;
  
  // Desenhar transições
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Font.Size := 8;
  
  for i := 0 to High(Transitions) do
  begin
    j := States.IndexOf(Transitions[i].FromState);
    if j = -1 then Continue;
    x1 := statePos[j].x;
    y1 := statePos[j].y;
    
    j := States.IndexOf(Transitions[i].ToState);
    if j = -1 then Continue;
    x2 := statePos[j].x;
    y2 := statePos[j].y;
    
    // Self-loop
    if (x1 = x2) and (y1 = y2) then
    begin
      ACanvas.Ellipse(x1 - 12, y1 - StateRadius - 24, x1 + 12, y1 - StateRadius);
      ACanvas.TextOut(x1 + 15, y1 - StateRadius - 24, Transitions[i].Symbol);
    end
    else
    begin
      // Calcular ângulo e desenhar seta
      angle := ArcTan2(y2 - y1, x2 - x1);
      
      // Ponto inicial (borda do círculo origem)
      midX := x1 + Round(StateRadius * Cos(angle));
      midY := y1 + Round(StateRadius * Sin(angle));
      
      // Ponto final (borda do círculo destino)
      dx := x2 - Round(StateRadius * Cos(angle));
      dy := y2 - Round(StateRadius * Sin(angle));
      
      ACanvas.MoveTo(midX, midY);
      ACanvas.LineTo(dx, dy);
      
      // Desenhar ponta da seta
      arrowLen := 8;
      ACanvas.LineTo(dx - Round(arrowLen * Cos(angle - Pi / 6)),
                    dy - Round(arrowLen * Sin(angle - Pi / 6)));
      ACanvas.MoveTo(dx, dy);
      ACanvas.LineTo(dx - Round(arrowLen * Cos(angle + Pi / 6)),
                    dy - Round(arrowLen * Sin(angle + Pi / 6)));
      
      // Label no meio da transição
      labelX := (x1 + x2) div 2;
      labelY := (y1 + y2) div 2 - 8;
      ACanvas.TextOut(labelX, labelY, Transitions[i].Symbol);
    end;
  end;
  
  // Desenhar estados
  ACanvas.Pen.Width := 1;
  ACanvas.Font.Size := 8;
  ACanvas.Font.Style := [fsBold];
  
  for i := 0 to States.Count - 1 do
  begin
    x := statePos[i].x;
    y := statePos[i].y;
    
    // Verificar se é estado final (círculo duplo)
    if Assigned(Finals) and (Finals.IndexOf(States[i]) >= 0) then
    begin
      ACanvas.Brush.Color := clYellow;
      ACanvas.Ellipse(x - StateRadius, y - StateRadius, x + StateRadius, y + StateRadius);
      ACanvas.Brush.Color := clWhite;
      ACanvas.Ellipse(x - StateRadius + 2, y - StateRadius + 2, 
                     x + StateRadius - 2, y + StateRadius - 2);
    end
    else
    begin
      ACanvas.Brush.Color := clWhite;
      ACanvas.Ellipse(x - StateRadius, y - StateRadius, x + StateRadius, y + StateRadius);
    end;
    
    // Marcar estado inicial com seta
    if Assigned(Initials) and (Initials.IndexOf(States[i]) >= 0) then
    begin
      ACanvas.MoveTo(x - StateRadius - 20, y);
      ACanvas.LineTo(x - StateRadius - 3, y);
      ACanvas.LineTo(x - StateRadius - 7, y - 4);
      ACanvas.MoveTo(x - StateRadius - 3, y);
      ACanvas.LineTo(x - StateRadius - 7, y + 4);
    end;
    
    // Nome do estado
    ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(x - ACanvas.TextWidth(States[i]) div 2, 
                   y - ACanvas.TextHeight(States[i]) div 2, 
                   States[i]);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

end.

