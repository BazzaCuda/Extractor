unit main;
{
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/Extractor

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls,
  mormot.lib.win7zip;

type
  TProcessType = (ptFind, ptExtract);
  rawUTF8 = system.UTF8String;

  TForm1 = class(TForm)
    sg: TStringGrid;
    pnlButtons: TPanel;
    btnExtract: TButton;
    lblFeedback: TLabel;
    btnCancel: TButton;
    btnFind: TButton;
    chbReloadPWs: TCheckBox;
    edtNewPassword: TEdit;
    btnAddNewPassword: TButton;
    btnFindFiles: TButton;
    procedure btnExtractClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure sgSelectCell(Sender: TObject; ACol, ARow: LongInt; var CanSelect: Boolean);
    procedure btnFindClick(Sender: TObject);
    procedure btnAddNewPasswordClick(Sender: TObject);
    procedure btnFindFilesClick(Sender: TObject);
    procedure sgKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict private
    FPasswords: TStringList;
    FDLLPath:   string;
    FDLPath:    string;
    FDLExts:    TArray<string>;
  private
    procedure WMDropFiles(var msg: TWMDropFiles);  message WM_DROPFILES;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  winApi.shellApi,
  system.ioUtils,
  vcl.clipbrd,
  _debugWindow;

type
  TCustomGridHelper = class helper for TCustomGrid
  public
    procedure delRow(aRow: integer);
  end;

var
  GCancel: boolean;

procedure clearStringGrid(const grid: TStringGrid);
var
  c, r: integer;
begin
  for c := 0 to pred(grid.colCount) do
    for r := 0 to pred(grid.rowCount) do
      grid.cells[c, r] := '';
end;

function shiftKeyDown: boolean;
var vShiftState: TShiftState;
begin
  vShiftState   := KeyboardStateToShiftState;
  result := ssSHIFT in vShiftState;
end;

function extract7zArchive(aFilePath: string; aPassword: rawUTF8; aDLLPath: string): boolean;
var
  vArchive: I7zReader;
  vFormatHandler: T7zFormatHandler;
begin
  result    := FALSE;
  vFormatHandler := T7zLib.formatDetect(aFilePath);

  try
    vArchive  := new7zReader(aFilePath, vFormatHandler, aDLLPath, aPassword);
  except
    EXIT;
  end;

  var vOutputPath := extractFilePath(aFilePath) + TPath.getFileNameWithoutExtension(aFilePath) + '\';

  try
    vArchive.extractAll(vOutputPath);
    result := TRUE;
  except
  end;
end;

procedure initGrid(aSG: TStringGrid);
const
  cNoSelection: TGridRect = (Left: 0; Top: -1; Right: 0; Bottom: -1);
begin
  clearStringGrid(aSG);
  aSG.defaultRowHeight := 16;
  aSG.cells[0, 0]  := 'file';
  aSG.cells[1, 0]  := 'password';
  aSG.cells[2, 0]  := 'result';
  aSG.colWidths[0] := 400;
  aSG.colWidths[1] := 200;
  aSG.rowCount     := 1;
  aSG.selection    := cNoSelection;
end;

procedure findFiles(aDLPath: string; aExts: TArray<string>; aSG: TStringGrid);
var
  vRowIx: integer;

  procedure findFilesByExt(aExt: string);
  var
    RC: integer;
    SR: TSearchRec;
  begin
    RC := findFirst(aDLPath + aExt, faAnyFile - faDirectory - faHidden - faSysFile, SR);
    while RC = 0 do begin
                      inc(vRowIx);
                      aSG.rowCount := aSG.rowCount + 1;
                      aSG.cells[0, vRowIx] := aDLPath + SR.name;
                      RC := findNext(SR); end;
    findClose(SR);
  end;

begin
  initGrid(aSG);

  vRowIx := 0;

  for var i := 0 to length(aExts) - 1 do findFilesByExt(aExts[i]);
end;

procedure loadPasswords(aPasswords: TStringList; const aNewPassword: rawUTF8);
begin
  case aPasswords.count = 0 of FALSE: EXIT; end; // user must force reload if reqd
  aPasswords.duplicates := dupIgnore;
  aPasswords.sorted     := TRUE;
  aPasswords.loadFromFile(extractFilePath(paramStr(0)) + 'passwords.txt');
  aPasswords.sorted     := FALSE;

  case trim(aNewPassword) = '' of FALSE:  begin
                                            aPasswords.insert(0, aNewPassword);
                                            aPasswords.saveToFile(extractFilePath(paramStr(0)) + 'passwords.txt'); end;end;

  aPasswords.saveToFile(extractFilePath(paramStr(0)) + 'passwords_deduped.txt');
  aPasswords.insert(0, '');
end;

function smallestItemIx(aArchive: I7zReader): integer; inline;
begin
  result    := -1;
  var vSize := -1;
  for var i := 0 to aArchive.count - 1 do begin
    case aArchive.isFolder[i] of TRUE: CONTINUE; end;   // ignore folders
    case aArchive.size[i] = 0 of TRUE: CONTINUE; end;   // ignore zero length files
    case vSize = -1 of   TRUE:  begin
                                  result := i;
                                  vSize := aArchive.size[i]; end;
                        FALSE: case aArchive.size[i] < vSize of  TRUE:  begin
                                                                          result := i;
                                                                          vSize   := aArchive.size[i]; end;end;end;end;
end;

function test7zArchive(aFilePath: string; aPassword: rawUTF8; aDLLPath: string): boolean;
var
  vArchive: I7zReader;
  vFormatHandler: T7zFormatHandler;
  vStream: TMemoryStream;
  vStreamSize: int64;
begin
  result    := FALSE;
  vFormatHandler := T7zLib.formatDetect(aFilePath);
//  debugString('aFilePath', aFilePath);
//  TDebug.debugEnum<T7zFormatHandler>('vFormatHandler', vFormatHandler);

  try
    vArchive  := new7zReader(aFilePath, vFormatHandler, aDLLPath, aPassword);
  except
    EXIT;
  end;

  var vIx   := smallestItemIx(vArchive);
  case vIx = -1 of TRUE: EXIT; end;        // automatic fail

  var vZipName  := vArchive.zipName[vIx];
  var vItemSize := vArchive.size[vIx];

  try
    vStream := TMemoryStream.create;  // try to extract the smallest item in the archive to verify the password
    try
      vArchive.Extract(vIx, vStream);
      vStreamSize := vStream.size;
    finally
      vStream.free;
    end;
  except begin
    // failed to extract file; result := FALSE
    EXIT;
  end;end;

  result := vStreamSize = vItemSize;
end;

procedure refreshUI(aSG: TStringGrid; aFeedback: TLabel);
begin
  aSG.repaint;
  aFeedback.repaint;
  application.processMessages;
end;

function processFile(aSG: TStringGrid; const aRowIx: integer; aPasswords: TStringList; const aProcessType: TProcessType; aFeedback: TLabel; const aDLLPath: string): boolean;
var
  vFilePath:   string;
  vPW:         string;
  vTestResult: boolean;
begin
  result              := FALSE;
  vPW                 := '';
  aFeedback.caption   := '';
  vFilePath           := aSG.cells[0, aRowIx];

  for var i := 0 to aPasswords.count - 1 do begin
    case GCancel of  TRUE: BREAK; end;
    aSG.cells[1, aRowIX] := '';
    aSG.cells[2, aRowIX] := 'testing';
    case i = 0 of FALSE: aFeedback.caption := format('pw: %d of %d', [i, aPasswords.count - 1]); end;
    refreshUI(aSG, aFeedback);
    try
      vTestResult := test7zArchive(vFilePath, aPasswords[i], aDLLPath);
    except end;
    case vTestResult of  TRUE:  begin
                                  aSG.cells[2, aRowIx] := '';
                                  vPW := aPasswords[i];
                                  case vPW = '' of  TRUE: vPW := '<no pw>';
                                                   FALSE: aSG.cells[2, aRowIx] := 'Found'; end;
                                  aSG.cells[1, aRowIx] := vPW;
                                  refreshUI(aSG, aFeedback);

                                  case aProcessType of ptFind: begin result := TRUE; BREAK; end;end;

                                  aSG.cells[2, aRowIx] := 'extracting';
                                  refreshUI(aSG, aFeedback);
                                  result := extract7zArchive(vFilePath, aPasswords[i], aDLLPath);
                                  BREAK; end;
                        FALSE:  begin
                                  aSG.cells[1, aRowIx] := '';
                                  aSG.cells[2, aRowIx] := 'failed';
                                  refreshUI(aSG, aFeedback); end;
    end;
  end;
  aFeedback.caption := '';

  case result and (vPW <> '<no pw>') of TRUE: aPasswords.insert(1, vPW); end;

  case aProcessType of ptFind: EXIT; end;

  case result of   TRUE: aSG.cells[2, aRowIx] := 'success';
                  FALSE: aSG.cells[2, aRowIx] := 'failed'; end;
  refreshUI(aSG, aFeedback);
end;

procedure processFiles(aProcessType: TProcessType; aSG: TStringGrid; aPasswords: TStringList; aFeedback: TLabel; const aDLLPath: string);
begin
  GCancel := FALSE;
  loadPasswords(aPasswords, '');
  for var rowIx := 1 to aSG.rowCount - 1 do aSG.cells[2, rowIx] := '';                                                // clear the results column
  for var rowIx := 1 to aSG.rowCount - 1 do processFile(aSG, rowIx, aPasswords, aProcessType, aFeedback, aDLLPath);   // process the files column
end;

function readConfigFile(var vDLLPath: string; var vDLPath: string; var vDLExts: TArray<string>): boolean;
var vExtsList: string;
begin
  var vSL := TStringList.create;
  try
    vSL.loadFromFile(extractFilePath(paramStr(0)) + 'extractor.ini');
    vDLPath   := vSL.values['DLPath'];
    vExtsList := vSL.values['DLExts'];
  finally
    vSL.free;
  end;

  vDLExts   := vExtsList.split([',', ';']);
  vDLLPath  := extractFilePath(paramStr(0)) + '7z.dll';

  result := fileExists(vDLLPath) and directoryExists(vDLPath) and (length(vDLExts) <> 0);

  case result of FALSE: raise exception.create('invalid extractor.ini settings'); end;
end;


//========== FORM EVENT HANDLERS ==========

procedure TForm1.btnAddNewPasswordClick(Sender: TObject);
begin
  FPasswords.clear;
  loadPasswords(FPasswords, edtNewPassword.text);
  edtNewPassword.clear;
end;

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  GCancel := TRUE;
end;

procedure TForm1.btnExtractClick(Sender: TObject);
begin
  case chbReloadPWs.checked of TRUE: FPasswords.clear; end;
  chbReloadPWs.checked := FALSE;
  processFiles(ptExtract, sg, FPasswords, lblFeedback, FDLLPath);
end;

procedure TForm1.btnFindClick(Sender: TObject);
begin
  case chbReloadPWs.checked of TRUE: FPasswords.clear; end;
  chbReloadPWs.checked := FALSE;
  processFiles(ptFind, sg, FPasswords, lblFeedback, FDLLPath);
end;

procedure TForm1.btnFindFilesClick(Sender: TObject);
begin
  findFiles(FDLPath, FDLExts, sg);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  case FPasswords = NIL of FALSE: FPasswords.free; end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  debugClear;

  try
    readConfigFile(FDLLPath, FDLPath, FDLExts);
  except on e:exception do begin
    showMessage(e.message);
    halt;
  end;end;

  caption := format('Extractor - %s', [FDLPath]);

  lblFeedback.caption         := '';
  dragAcceptFiles(handle, True);
  FPasswords                  := TStringList.create;
  FPasswords.defaultEncoding  := TEncoding.UTF8;
  initGrid(sg);
  width := sg.colWidths[0] + sg.colWidths[1] + sg.colWidths[2] + pnlButtons.width + 30;
end;

procedure TForm1.FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  accept := TRUE;
end;

procedure TForm1.sgKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_DELETE of TRUE: sg.delRow(sg.row); end;
end;

procedure TForm1.sgSelectCell(Sender: TObject; ACol, ARow: LongInt; var CanSelect: Boolean);
begin
  clipboard.asText := sg.cells[aCol, aRow];
end;

procedure TForm1.WMDropFiles(var msg: TWMDropFiles);
var vFilePath: string;
begin
  inherited;

  case shiftKeyDown of TRUE: initGrid(sg); end;

  var hDrop := msg.Drop;
  try
    var droppedFileCount := dragQueryFile(hDrop, $FFFFFFFF, nil, 0);
    for var i := 0 to pred(droppedFileCount) do begin
      var fileNameLength := dragQueryFile(hDrop, i, nil, 0);
      setLength(vFilePath, fileNameLength);
      dragQueryFile(hDrop, i, PChar(vFilePath), fileNameLength + 1);

      sg.rowCount := sg.rowCount + 1;
      sg.cells[0, sg.rowCount - 1] := vFilePath;
    end;
  finally
    dragFinish(hDrop);
  end;
  msg.result := 0;
end;

{ TCustomGridHelper }

procedure TCustomGridHelper.delRow(aRow: integer);
begin
  self.deleteRow(aRow);
end;

end.
