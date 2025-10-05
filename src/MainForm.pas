unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Math;

type
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
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure btnClearClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ConvertAFNtoAFD;
    procedure LoadSampleFile;
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

  { TTransition - registro para transições }
  TTransition = record
    FromState: string;
    Symbol: string;
    ToState: string;
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

procedure TFormMain.ConvertAFNtoAFD;
var
  Alphabet, States, Initials, Finals: TStringList;
  Transitions: array of TTransition;
  parts: TStringList;
  i, j, k, tStart: Integer;
  dfaStates, workQ, dfaMap, isFinal: TStringList;
  curSet, nextSet, cloneSet: TStringSet;
  sym, key: string;
  dfaTransitions: array of record fromName, symbol, toName: string; end;
  hasFinal: Boolean;
  line: string;
begin
  memoOutput.Lines.Clear;
  
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
      if parts[i] <> '' then Alphabet.Add(parts[i]);
    
    // Linha 1: estados
    parts.DelimitedText := Trim(memoInput.Lines[1]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then States.Add(parts[i]);
    
    // Linha 2: iniciais
    parts.DelimitedText := Trim(memoInput.Lines[2]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then Initials.Add(parts[i]);
    
    // Linha 3: finais
    parts.DelimitedText := Trim(memoInput.Lines[3]);
    for i := 0 to parts.Count - 1 do
      if parts[i] <> '' then Finals.Add(parts[i]);
    
    // Linhas restantes: transições
    tStart := 4;
    SetLength(Transitions, Max(0, memoInput.Lines.Count - tStart));
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
      end;
    end;
    
    // Construção do DFA via subset construction
    dfaStates := TStringList.Create;
    workQ := TStringList.Create;
    dfaMap := TStringList.Create;
    dfaMap.OwnsObjects := True;
    isFinal := TStringList.Create;
    
    try
      // Estado inicial do DFA = conjunto de estados iniciais do AFN
      curSet := TStringSet.Create;
      for i := 0 to Initials.Count - 1 do
        curSet.Add(Initials[i]);
      
      dfaStates.Add(GetSetName(curSet));
      workQ.Add(GetSetName(curSet));
      dfaMap.AddObject(GetSetName(curSet), curSet);
      
      SetLength(dfaTransitions, 0);
      
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
            SetLength(dfaTransitions, Length(dfaTransitions) + 1);
            dfaTransitions[High(dfaTransitions)].fromName := GetSetName(curSet);
            dfaTransitions[High(dfaTransitions)].symbol := sym;
            dfaTransitions[High(dfaTransitions)].toName := key;
            
            // Se é novo estado do DFA, adicionar
            if (dfaMap.IndexOf(key) = -1) and (dfaStates.IndexOf(key) = -1) then
            begin
              if key <> '{}' then
              begin
                cloneSet := nextSet.Clone;
                dfaMap.AddObject(key, cloneSet);
              end;
              dfaStates.Add(key);
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
      
      memoOutput.Lines.Add('📋 ALFABETO: ' + Alphabet.CommaText);
      memoOutput.Lines.Add('');
      
      memoOutput.Lines.Add('🔵 ESTADOS DO DFA:');
      for i := 0 to dfaStates.Count - 1 do
        memoOutput.Lines.Add('   • ' + dfaStates[i]);
      memoOutput.Lines.Add('');
      
      if dfaMap.IndexOf(dfaStates[0]) <> -1 then
        memoOutput.Lines.Add('▶️  ESTADO INICIAL: ' + GetSetName(TStringSet(dfaMap.Objects[dfaMap.IndexOf(dfaStates[0])])))
      else
        memoOutput.Lines.Add('▶️  ESTADO INICIAL: ' + dfaStates[0]);
      memoOutput.Lines.Add('');
      
      memoOutput.Lines.Add('🎯 ESTADOS FINAIS:');
      if isFinal.Count > 0 then
      begin
        for i := 0 to isFinal.Count - 1 do
          memoOutput.Lines.Add('   • ' + isFinal[i]);
      end
      else
        memoOutput.Lines.Add('   (nenhum)');
      memoOutput.Lines.Add('');
      
      memoOutput.Lines.Add('➡️  TRANSIÇÕES:');
      for i := 0 to High(dfaTransitions) do
        memoOutput.Lines.Add('   ' + dfaTransitions[i].fromName + ' --' + 
                            dfaTransitions[i].symbol + '--> ' + 
                            dfaTransitions[i].toName);
      
      memoOutput.Lines.Add('');
      memoOutput.Lines.Add('═══════════════════════════════════════════════════');
      memoOutput.Lines.Add('✅ Conversão concluída com sucesso!');
      
    finally
      dfaStates.Free;
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

end.
