program afn2afdgui;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm
  { you can add units after this };

{$R *.res}
{$R appicon.res}

begin
  WriteLn('===========================================');
  WriteLn('  AFN -> AFD Converter - Console Log');
  WriteLn('===========================================');
  WriteLn('');
  
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  
  WriteLn('[INIT] Aplicacao iniciada');
  WriteLn('');
  
  Application.Run;
  
  WriteLn('');
  WriteLn('[EXIT] Aplicacao encerrada');
end.
