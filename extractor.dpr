program Extractor;

uses
  system.sysUtils,
  vcl.dialogs,
  Vcl.Forms,
  Vcl.Styles,
  Vcl.Themes,
  mormot.lib.win7zip in '..\..\3P\mORMot2\src\lib\mormot.lib.win7zip.pas',
  _debugWindow in '..\DebugWindow\_debugWindow.pas',
  main in 'main.pas' {Form1},
  RAR in '..\TRAR\RAR.pas',
  RAR_DLL in '..\TRAR\RAR_DLL.pas';

{$R *.res}

begin
  {$if BazDebugWindow}debugClear;{$endif}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Charcoal Dark Slate');
  Application.CreateForm(TForm1, Form1);
  try
    form1.config    := readConfigFile;
    form1.passwords := TPasswords.create;
    form1.finishSetup;
  except on e:exception do begin
    showMessage(e.message);
    halt;
  end;end;

  Application.Run;
end.

