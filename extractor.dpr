program extractor;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles,
  _debugWindow in '..\DebugWindow\_debugWindow.pas',
  mormot.lib.win7zip in '..\..\3P\mORMot2\src\lib\mormot.lib.win7zip.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Charcoal Dark Slate');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
