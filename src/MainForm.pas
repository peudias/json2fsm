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
    edtFilePath: TEdit;
    lblInput: TLabel;
    lblOutput: TLabel;
    lblFile: TLabel;
    memoInput: TMemo;
    memoOutput: TMemo;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    TabNFA: TTabSheet;
    TabDFA: TTabSheet;
    PaintBoxNFA: TPaintBox;
    PaintBoxDFA: TPaintBox;
    procedure btnClearClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxNFAPaint(Sender: TObject);
    procedure PaintBoxDFAPaint(Sender: TObject);
  private
    // Dados do AFN
    NFAAlphabet, NFAStates, NFAInitials, NFAFinals: TStringList;
    NFATransitions: TTransitionArray;
    // Dados do AFD
    DFAStates: TStringList;
    DFATransitions: TTransitionArray;
    DFAFinals: TStringList;
    
    procedure ConvertAFNtoAFD;
    procedure LoadSampleFile;
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
  Caption := 'Conversor AFN → AFD';
  memoInput.Lines.Clear;
  memoOutput.Lines.Clear;
  
  // Carregar arquivo exemplo se existir
  LoadSampleFile;
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
    try
      memoInput.Lines.LoadFromFile(OpenDialog.FileName);
      edtFilePath.Text := OpenDialog.FileName;
    except
      on E: Exception do
        ShowMessage('Erro ao carregar arquivo: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.btnClearClick(Sender: TObject);
begin
  memoInput.Lines.Clear;
  memoOutput.Lines.Clear;
  edtFilePath.Text := '';
  
  // Limpar dados dos diagramas
  if Assigned(NFAAlphabet) then NFAAlphabet.Clear;
  if Assigned(NFAStates) then NFAStates.Clear;
  if Assigned(NFAInitials) then NFAInitials.Clear;
  if Assigned(NFAFinals) then NFAFinals.Clear;
  if Assigned(DFAStates) then DFAStates.Clear;
  if Assigned(DFAFinals) then DFAFinals.Clear;
  SetLength(NFATransitions, 0);
  SetLength(DFATransitions, 0);
  
  // Redesenhar os painéis vazios
  if Assigned(PaintBoxNFA) then PaintBoxNFA.Invalidate;
  if Assigned(PaintBoxDFA) then PaintBoxDFA.Invalidate;
end;

procedure TFormMain.btnConvertClick(Sender: TObject);
begin
  try
    ConvertAFNtoAFD;
  except
    on E: Exception do
    begin
      memoOutput.Lines.Clear;
      memoOutput.Lines.Add('ERRO: ' + E.Message);
    end;
  end;
end;

function GetSetName(setObj: TStringSet): string;
begin
  Result := setObj.ToString;
end;

function IsEpsilonSymbol(const s: string): Boolean;
begin
  Result :=
    (s = 'ε') or
    (LowerCase(s) = 'eps') or
    (LowerCase(s) = 'epsilon');
end;

procedure EpsilonClosure(const Transitions: array of TTransition; Source, Dest: TStringSet);
var
  stack: TStringList;
  i, k: Integer;
  st: string;
begin
  for i := 0 to Source.Count - 1 do
    Dest.Add(Source.Item(i));

  stack := TStringList.Create;
  try
    for i := 0 to Source.Count - 1 do
      stack.Add(Source.Item(i));

    while stack.Count > 0 do
    begin
      st := stack[stack.Count - 1];
      stack.Delete(stack.Count - 1);

      for k := 0 to High(Transitions) do
      begin
        if (Transitions[k].FromState = st) and IsEpsilonSymbol(Transitions[k].Symbol) then
        begin
          if not Dest.Contains(Transitions[k].ToState) then
          begin
            Dest.Add(Transitions[k].ToState);
            stack.Add(Transitions[k].ToState);
          end;
        end;
      end;
    end;
  finally
    stack.Free;
  end;
end;

procedure MoveSet(const Transitions: array of TTransition; Source: TStringSet;
  const Sym: string; Dest: TStringSet);
var
  i, k: Integer;
begin
  for i := 0 to Source.Count - 1 do
    for k := 0 to High(Transitions) do
      if (Transitions[k].FromState = Source.Item(i)) and
         (Transitions[k].Symbol = Sym) and
         (not IsEpsilonSymbol(Transitions[k].Symbol)) then
        Dest.Add(Transitions[k].ToState);
end;


procedure TFormMain.ConvertAFNtoAFD;
var
  Alphabet, States, Initials, Finals: TStringList;
  Transitions: array of TTransition;
  parts: TStringList;
  i, j, k, tStart: Integer;
  localDFAStates, workQ, dfaMap, isFinal: TStringList;
  curSet, nextSet, cloneSet: TStringSet;
  moveResult: TStringSet;
  sym, key: string;
  localDFATransitions: TTransitionArray;
  hasFinal: Boolean;
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
    
    // Linha 0: alfabeto (ignorando epsilon)
    parts.DelimitedText := Trim(memoInput.Lines[0]);
    for i := 0 to parts.Count - 1 do
      if (parts[i] <> '') and (not IsEpsilonSymbol(parts[i])) then
      begin
        Alphabet.Add(parts[i]);
        NFAAlphabet.Add(parts[i]);
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
        
        NFATransitions[i - tStart].FromState := parts[0];
        NFATransitions[i - tStart].Symbol := parts[1];
        NFATransitions[i - tStart].ToState := parts[2];
      end;
    end;
    
    // Construção do DFA via subset construction
    localDFAStates := TStringList.Create;
    workQ := TStringList.Create;
    dfaMap := TStringList.Create;
    dfaMap.OwnsObjects := True;
    isFinal := TStringList.Create;
    
    try
      // Estado inicial do DFA = ε-closure(conjunto de estados iniciais do AFN)
      curSet := TStringSet.Create;
      for i := 0 to Initials.Count - 1 do
        curSet.Add(Initials[i]);
      
      nextSet := TStringSet.Create;
      EpsilonClosure(Transitions, curSet, nextSet);
      curSet.Free;
      curSet := nextSet;

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
          moveResult := TStringSet.Create;
          nextSet := TStringSet.Create;
          try
            // move(curSet, sym) sem ε
            MoveSet(Transitions, curSet, sym, moveResult);

            if moveResult.IsEmpty then
              key := '{}'
            else
            begin
              EpsilonClosure(Transitions, moveResult, nextSet);
              if nextSet.IsEmpty then
                key := '{}'
              else
                key := GetSetName(nextSet);
            end;
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
              if (key <> '{}') and (not nextSet.IsEmpty) then
              begin
                cloneSet := nextSet.Clone;
                dfaMap.AddObject(key, cloneSet);
              end;
              localDFAStates.Add(key);
              if key <> '{}' then
                workQ.Add(key);
            end;
            
          finally
            moveResult.Free;
            nextSet.Free;
          end;
        end;
      end;
      
      // Exibir resultado
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('    RESULTADO DA CONVERSÃO AFN → AFD');
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('');
      
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
        memoOutput.Lines.Add('▶️  ESTADO INICIAL: ' + GetSetName(TStringSet(dfaMap.Objects[dfaMap.IndexOf(localDFAStates[0])])))
      else
        memoOutput.Lines.Add('▶️  ESTADO INICIAL: ' + localDFAStates[0]);
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

