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


  IPasswords = interface
  ['{032A1242-D1F7-47D5-8045-39446084DBE3}']
    function getCount:  integer;
    function getPassword(aIx: integer): rawUTF8;
    function getPWs:    TStringlist;

    function addNewPassword(const aNewPassword: rawUTF8): integer;
    function insertPassword(aIx: integer; aPassword: rawUTF8): boolean;
    function loadPasswords: integer;
    property count: integer     read getCount;
    property password[aIx: integer]: rawUTF8 read getPassword;
    property PWs:   TStringlist read getPWs;
  end;

  TPasswords = class(TInterfacedObject, IPasswords)
  strict private
    FPasswords: TStringlist;
  private
    function getPasswordsFile: string;
    function loadPasswordsFromFile(const aPasswordFile: string): boolean;
    function savePasswordsToFile(const aPasswordFile: string): boolean;
    function getDedupedFile: string;
  protected
    destructor  Destroy; override;
  public
    constructor create;
    function getPassword(aIx: integer): rawUTF8;
    function getCount:  integer;
    function getPWs:    TStringlist;

    function addNewPassword(const aNewPassword: rawUTF8): integer;
    function insertPassword(aIx: integer; aPassword: rawUTF8): boolean;
    function loadPasswords: integer;
  end;

  TConfigRec = record
    crDLPath:         string;
    crDLExts:         TArray<string>;
    crDLLPath:        string;
    crPasswords:      IPasswords;
    crPasswordCount:  integer;
    crSG:             TStringGrid;
    crFeedback:       TLabel;
    // operation specific
    crProcessType:    TProcessType;
    crArchivePath:    string;
    crPassword:       rawUTF8;
    crMoveExtracted:  boolean;
    crCancel:         boolean;
  end;

  TFindFunc = function(const aConfig: TConfigRec; const aFilePath: string): boolean;

  IFileFinder = interface
  ['{33864E0C-372C-47B3-BCBF-045356E9C391}']
    procedure setConfigRec  (const aValue: TConfigRec);
    procedure setFindFunc   (const aValue: TFindFunc);
    procedure setFileMask   (const aValue: string);
    procedure setRootFolder (const aValue: string);
    function  findFiles: boolean;
    property  configRec:   TConfigRec write setConfigRec;
    property  fileMask:    string     write setFileMask;
    property  findFunc:    TFindFunc  write setFindFunc;
    property  rootFolder:  string     write setRootFolder;
  end;

  TFileFinder = class(TInterfacedObject, IFileFinder)
  strict private
    FConfigRec:   TConfigRec;
    FFindFunc:    TFindFunc;
    FFileMask:    string;
    FRootFolder:  string;
  public
    function  findFiles: boolean;
    procedure setConfigRec  (const aValue: TConfigRec);
    procedure setFindFunc   (const aValue: TFindFunc);
    procedure setFileMask   (const aValue: string);
    procedure setRootFolder (const aValue: string);
  end;

  TForm1 = class(TForm)
    sg:                 TStringGrid;
    btnAddNewPassword:  TButton;
    btnCancel:          TButton;
    btnExtract:         TButton;
    btnFindPWs:         TButton;
    btnFindFiles:       TButton;
    chbReloadPWs:       TCheckBox;
    edtNewPassword:     TEdit;
    lblFeedback:        TLabel;
    pnlButtons:         TPanel;
    chbMoveExtracted: TCheckBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure sgKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure btnAddNewPasswordClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnFindFilesClick(Sender: TObject);
    procedure btnFindPWsClick(Sender: TObject);
    procedure sgSelectCell(Sender: TObject; ACol, ARow: LongInt; var CanSelect: Boolean);
  strict private
    FConfig: TConfigRec;
  private
    function  checkForReload: boolean;
    procedure WMDropFiles(var msg: TWMDropFiles); message WM_DROPFILES;
    procedure setPasswords(const Value: IPasswords);
  public
    function finishSetup: boolean;
    property config:      TConfigRec read FConfig write FConfig;
    property passwords:   IPasswords              write setPasswords;
  end;

var
  Form1: TForm1;

function readConfigFile: TConfigRec;

implementation

{$R *.dfm}

uses
  winApi.shellApi,
  system.ioUtils,
  vcl.clipbrd,
  _debugWindow;

type
  TStringGridHelper = class helper for TStringGrid
  public
    procedure addRow(const aFilePath: string);
    procedure clearCol(const aColIx: integer);
    procedure initGrid;
  end;

  TCustomGridHelper = class helper for TCustomGrid
  public
    procedure delRow(aRow: integer);
  end;

function newFile(const aConfig: TConfigRec; const aFilePath: string): boolean;
 // Embarcadero don't allow this to be local to the findFiles function, because "reasons" :(
 // Consequently, I've had to add aConfig as a parameter which makes this a much less neater and generic a solution than it was going to be :(
 // Having said that, in another project you could define TConfigRec completely differently, so TFileFinder is still a plausibly generic solution
begin
  result := FALSE;
  aConfig.crSG.addRow(aFilePath);
  result := TRUE;
end;

function findFiles(const aConfig: TConfigRec): boolean;

  // function newFile was supposed to go here

  function findFilesByExt(aExt: string): boolean;
  begin
    result := FALSE;

    var vFileFinder:IFileFinder := TFileFinder.create;

    vFileFinder.configRec   := aConfig;
    vFileFinder.rootFolder  := aConfig.crDLPath;
    vFileFinder.fileMask    := aExt;
    vFileFinder.findFunc    := newFile;
    vFileFinder.findFiles;

    result := TRUE;
  end;

begin
  result := FALSE;

  for var i := 0 to length(aConfig.crDLExts) - 1 do findFilesByExt(aConfig.crDLExts[i]);

  result := TRUE;
end;

function readConfigFile: TConfigRec;
var
  vExtsList:  string;
begin
  result := default(TConfigRec);
  var vSL := TStringList.create;
  try
    vSL.loadFromFile(extractFilePath(paramStr(0)) + 'extractor.ini');
    result.crDLPath := vSL.values['DLPath'];
    vExtsList       := vSL.values['DLExts'];
  finally
    vSL.free;
  end;

  result.crDLExts   := vExtsList.split([',', ';']);
  result.crDLLPath  := extractFilePath(paramStr(0)) + '7z.dll';

  case fileExists(result.crDLLPath) and directoryExists(result.crDLPath) and (length(result.crDLExts) <> 0) of FALSE: raise exception.create('invalid extractor.ini settings'); end;

  result.crDLPath := includeTrailingBackslash(result.crDLPath);
end;

function refreshUI(const aConfig: TConfigRec): boolean;
begin
  result := FALSE;

  aConfig.crSG.repaint;
  aConfig.crFeedback.repaint;
  application.processMessages;

  result := TRUE;
end;

function extractArchive(const aConfig: TConfigRec; const aRowIx: integer): boolean;
var
  vArchive: I7zReader;
  vFormatHandler: T7zFormatHandler;

  function fileSize(const aFilePath: string): int64;
  var
    vHandle:  THandle;
    vRec:     TWin32FindData;
  begin
    result := -1;

    vHandle := findFirstFile(PChar(aFilePath), vRec);
    case vHandle <> INVALID_HANDLE_VALUE of TRUE: begin
                                                    winAPI.windows.findClose(vHandle);
                                                    case (vRec.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 of TRUE:
                                                        result := (int64(vRec.nFileSizeHigh) shl 32) + vRec.nFileSizeLow; end;end;end;
  end;

  function largestItemIx: integer;
  begin
    result    := -1;
    var vSize := -1;

    for var i := 0 to vArchive.count - 1 do begin
      case vArchive.isFolder[i] of TRUE: CONTINUE; end;   // ignore folders
      case vArchive.size[i] = 0 of TRUE: CONTINUE; end;   // ignore zero length files
      case vSize = -1 of   TRUE:  begin
                                    result := i;
                                    vSize := vArchive.size[i]; end;
                          FALSE: case vArchive.size[i] > vSize of  TRUE:  begin
                                                                            result := i;
                                                                            vSize  := vArchive.size[i]; end;end;end;end;
  end;

begin
  result := FALSE;

  aConfig.crSG.cells[2, aRowIx] := 'extracting';
  refreshUI(aConfig);

  vFormatHandler := T7zLib.formatDetect(aConfig.crArchivePath);

  try
    vArchive  := new7zReader(aConfig.crArchivePath, vFormatHandler, aConfig.crDLLPath, aconfig.crPassword);
  except
    EXIT;
  end;

  var vOutputPath := extractFilePath(aConfig.crArchivePath) + TPath.getFileNameWithoutExtension(aConfig.crArchivePath);
  case lowerCase(vOutputPath) = lowerCase(aConfig.crArchivePath) of TRUE: vOutputPath := vOutputPath + '_extracted'; end; // archive file doesn't have an extension so the output folder will clash
  vOutputPath := vOutputPath + '\';

  try
    vArchive.extractAll(vOutputPath);

    var   vIx   := largestItemIx;
    case  vIx = -1 of TRUE: EXIT; end;        // automatic fail

    var vZipName  := vArchive.zipName[vIx];
    var vItemSize := vArchive.size[vIx];

    result := vItemSize = fileSize(vOutputPath + vZipName); // verify the largest file extracted ok
  except
    // extraction failed; result := FALSE
  end;
end;

function testArchive(const aConfig: TConfigRec; aPWIx: integer): boolean;
var
  vArchive: I7zReader;
  vFormatHandler: T7zFormatHandler;
  vStream: TMemoryStream;
  vStreamSize: int64;

  function smallestItemIx: integer;
  begin
    result    := -1;
    var vSize := -1;

    for var i := 0 to vArchive.count - 1 do begin
      case vArchive.isFolder[i] of TRUE: CONTINUE; end;   // ignore folders
      case vArchive.size[i] = 0 of TRUE: CONTINUE; end;   // ignore zero length files
      case vSize = -1 of   TRUE:  begin
                                    result := i;
                                    vSize := vArchive.size[i]; end;
                          FALSE: case vArchive.size[i] < vSize of  TRUE:  begin
                                                                            result := i;
                                                                            vSize   := vArchive.size[i]; end;end;end;end;
  end;

begin
  result    := FALSE;
  vFormatHandler := T7zLib.formatDetect(aConfig.crArchivePath);
//  TDebug.debugEnum<T7zFormatHandler>('vFormatHandler', vFormatHandler);

  try
    vArchive  := new7zReader(aConfig.crArchivePath, vFormatHandler, aConfig.crDLLPath, aConfig.crPasswords.password[aPWIx]);
  except
    EXIT;
  end;

  var   vIx := smallestItemIx;
  case  vIx = -1 of TRUE: EXIT; end;        // automatic fail

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

function findPW(var aConfig: TConfigRec; const aRowIx: integer): boolean; // result is also true if the archive isn't password protected, i.e. the password is ''

  procedure setFeedback(aIx: integer);
  begin
    case aIx = 0 of TRUE: EXIT; end;
    aConfig.crFeedback.caption := format('pw: %d of %d', [aIx, aConfig.crPasswords.count - 1]);
    refreshUI(aConfig);
  end;

  procedure initRow;
  begin
    aConfig.crFeedback.caption    := '';
    aConfig.crPassword            := '';
    aConfig.crSG.cells[1, aRowIX] := '';
    aConfig.crSG.cells[2, aRowIX] := 'testing';
    refreshUI(aConfig);
  end;

begin
  result := FALSE;
  var vPW: string;

  initRow;

//  aConfig.crArchivePath := aConfig.crSG.cells[0, aRowIx];

  for var i := 0 to aConfig.crPasswordcount - 1 do begin
    setFeedback(i);
    case aConfig.crCancel of TRUE: BREAK; end;
    result := testArchive(aConfig, i);
    case result of TRUE:  begin
                            aConfig.crPassword := aConfig.crPasswords.password[i];
                            BREAK; end;end;end;

  case result of   TRUE:  begin
                            aConfig.crSG.cells[2, aRowIx] := '';
                            case aConfig.crPassword = '' of  TRUE:  aConfig.crSG.cells[1, aRowIx] := '<no pw>';
                                                            FALSE:  begin
                                                                      aConfig.crSG.cells[1, aRowIx] := aConfig.crPassword;
                                                                      aConfig.crSG.cells[2, aRowIx] := 'Found';
                                                                    end;end;
                          end;
                  FALSE:  begin
                            aConfig.crSG.cells[1, aRowIx] := '';
                            aConfig.crSG.cells[2, aRowIx] := 'failed';
                          end;end;

  aConfig.crFeedback.caption  := '';
  refreshUI(aConfig);
  case result and (aConfig.crPassword <> '') of TRUE: aConfig.crPasswords.insertPassword(1, aConfig.crPassword); end; // after the '' entry at ix 0
end;

function checkSplitArchive(aArchivePath: string): string;
// read e.g. archive.7z.001, archive.7z.002 etc. and write to archive.7z
var
  vFiles: TStringList;
  vOutputFilePath: string;

  function findFiles: integer;
  var vFile: string;
  begin
    var vFileID := 1;
    while vFileID > 0 do begin
      vFile := format('%s.%.3d', [vOutputFilePath, vFileID]);
      case fileExists(vFile) of  TRUE: vFiles.add(vFile);
                                FALSE: BREAK; end;
      inc(vFileID);
   end;
   result := vFiles.count;
  end;

  function concatenateFiles: boolean;
  var
    vSrcStream, vDstStream: TFileStream;
    vBufferSize: integer;
  begin
    result := FALSE;

    vBufferSize := 8 * 1024 * 1024; // 8MB
    vDstStream := TFileStream.create(vOutputFilePath, fmCreate);
    try
      for var i := 0 to vFiles.count - 1 do begin
                                              vSrcStream := TFileStream.create(vFiles[i], fmOpenRead);
                                              try
                                                vDstStream.copyFrom(vSrcStream, vSrcStream.size, vBufferSize);
                                              finally
                                                vSrcStream.free;
                                              end;end;
    finally
      vDstStream.free;
    end;

    result := TRUE;
  end;

begin
  result := aArchivePath;
  var vExt := extractFileExt(aArchivePath);
  case vExt = '.001' of FALSE: EXIT; end;

  vOutputFilePath := extractFilePath(aArchivePath) + TPath.getFileNameWithoutExtension(aArchivePath); // strip the .001 to leave <path>/archive.7z

  vFiles := TStringList.create;
  try
    vFiles.duplicates := dupIgnore;
    vFiles.sorted     := TRUE;
    findFiles;
    case concatenateFiles of TRUE: result := vOutputFilePath; end; // replace the .001 file in the grid with the concatenated file
  finally
    vFiles.free;
  end;
end;

function processFile(var aConfig: TConfigRec; const aRowIx: integer): boolean;
  procedure moveArchive;
  begin
    var vDest := extractFilePath(aConfig.crArchivePath) + '_extracted\';
    forceDirectories(vDest);
    TFile.move(aConfig.crArchivePath, vDest + extractFilename(aConfig.crArchivePath));
  end;
begin
  case aConfig.crCancel of TRUE: EXIT; end;

  aConfig.crSG.cells[0, aRowIx] := checkSplitArchive(aConfig.crSG.cells[0, aRowIx]);
  aConfig.crArchivePath         := aConfig.crSG.cells[0, aRowIx];

  result := findPW(aConfig, aRowIx);

  case aConfig.crCancel of TRUE: EXIT; end;
  case result and (aConfig.crProcessType = ptExtract) of TRUE: result := extractArchive(aConfig, aRowIx); end;

  case aConfig.crProcessType of ptFind: EXIT; end;

  case result and aConfig.crMoveExtracted of TRUE: moveArchive; end;

  case result of   TRUE: aConfig.crSG.cells[2, aRowIx] := 'success';
                  FALSE: aConfig.crSG.cells[2, aRowIx] := 'failed'; end;
  refreshUI(aConfig);
end;

function processFiles(var aConfig: TConfigRec): boolean;
begin
  result := FALSE;

  aConfig.crSG.clearCol(2);                                                         // clear the result column
  for var vRowIx := 1 to aConfig.crSG.rowCount - 1 do processFile(aConfig, vRowIx); // process the files column

  result := TRUE;
end;

//========== FORM EVENT HANDLERS ==========

procedure TForm1.btnAddNewPasswordClick(Sender: TObject);
begin
  FConfig.crPasswordCount := FConfig.crPasswords.addNewPassword(edtNewPassword.text);
  edtNewPassword.clear;
end;

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  FConfig.crCancel := TRUE;
end;

procedure TForm1.btnExtractClick(Sender: TObject);
begin
  checkForReload;
  FConfig.crProcessType := ptExtract;
  FConfig.crMoveExtracted := chbMoveExtracted.checked;
  FConfig.crCancel      := FALSE;
  processFiles(FConfig);
end;

procedure TForm1.btnFindFilesClick(Sender: TObject);
begin
  sg.initGrid;
  findFiles(FConfig);
end;

procedure TForm1.btnFindPWsClick(Sender: TObject);
begin
  checkForReload;
  FConfig.crProcessType := ptFind;
  FConfig.crCancel      := FALSE;
  processFiles(FConfig);
end;

function TForm1.checkForReload: boolean;
begin
  result := FALSE;
  case chbReloadPWs.checked of TRUE: FConfig.crPasswordCount := FConfig.crPasswords.loadPasswords; end;
  chbReloadPWs.checked := FALSE;
  result := TRUE;
end;

function TForm1.finishSetup: boolean;
begin
  result := FALSE;

  FConfig.crSG            := sg;
  FConfig.crFeedback      := lblFeedback;
  FConfig.crPasswordCount := FConfig.crPasswords.count;

  result := TRUE;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  dragAcceptFiles(handle, True);

  sg.initGrid;
  width := sg.colWidths[0] + sg.colWidths[1] + sg.colWidths[2] + pnlButtons.width + 30;

  lblFeedback.caption := '';
end;

procedure TForm1.FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  accept := TRUE;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  caption := format('Extractor - %s', [FConfig.crDLPath]);
end;

procedure TForm1.setPasswords(const Value: IPasswords);
begin
  FConfig.crPasswords := value;
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
  function shiftKeyDown: boolean;
  var vShiftState: TShiftState;
  begin
    vShiftState   := KeyboardStateToShiftState;
    result := ssSHIFT in vShiftState;
  end;
begin
  inherited;

  case shiftKeyDown of TRUE: sg.initGrid; end;

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
  deleteRow(aRow);
  case rowCount = 1 of TRUE: TStringGrid(self).initGrid; end; // prevent the "file" column header from being auto-selected
end;

{ TPasswords }

function TPasswords.getCount: integer;
begin
  result := FPasswords.count;
end;

function TPasswords.getPassword(aIx: integer): rawUTF8;
begin
  result := FPasswords[aIx];
end;

function TPasswords.getDedupedFile: string;
begin
  result := extractFilePath(paramStr(0)) + 'passwords_deduped.txt';
end;

function TPasswords.getPasswordsFile: string;
begin
  result := extractFilePath(paramStr(0)) + 'passwords.txt';
end;

constructor TPasswords.create;
begin
  inherited;
  case fileExists(getPasswordsFile) of FALSE: raise exception.create('file not found: passwords.txt'); end;
  FPasswords                  := TStringlist.create;
  FPasswords.defaultEncoding  := TEncoding.UTF8;
  loadPasswords;
end;

destructor TPasswords.Destroy;
begin
  case FPasswords = NIL of FALSE: FPasswords.free; end;
  inherited;
end;

function TPasswords.addNewPassword(const aNewPassword: rawUTF8): integer;
begin
  result := -1;
  case trim(aNewPassword) = '' of TRUE: EXIT; end;

  loadPasswordsFromFile(getPasswordsFile); // reload the passwords file
  FPasswords.insert(0, aNewPassword);      // insert the new password
  savePasswordsToFile(getPasswordsFile);   // resave the passwords file
  savePasswordsToFile(getDedupedFile);     // immediately save a sorted, de-duplicated copy
  result := FPasswords.count;
end;

function TPasswords.getPWs: TStringlist;
begin
  result := FPasswords;
end;

function TPasswords.insertPassword(aIx: integer; aPassword: rawUTF8): boolean;
begin
  FPasswords.insert(aIx, aPassword);
end;

function TPasswords.loadPasswords: integer;
begin
  result := -1;
  loadPasswordsFromFile(getPasswordsFile);
  savePasswordsToFile(getDedupedFile);  // immediately save a sorted, de-duplicated copy
  FPasswords.insert(0, '');             // first test will be always be to check if an archive has no password
  result := FPasswords.count;
end;

function TPasswords.loadPasswordsFromFile(const aPasswordFile: string): boolean;
begin
  result := FALSE;
  FPasswords.duplicates := dupIgnore;
  FPasswords.sorted     := TRUE;
  FPasswords.loadFromFile(aPasswordFile);
  FPasswords.sorted     := FALSE;
  result := TRUE;
end;

function TPasswords.savePasswordsToFile(const aPasswordFile: string): boolean;
begin
  result := FALSE;
  FPasswords.saveToFile(aPasswordFile);
  result := TRUE;
end;

{ TStringGridHelper }

procedure TStringGridHelper.addRow(const aFilePath: string);
begin
  rowCount := rowCount + 1;
  cells[0, rowCount - 1] := aFilePath;
end;

procedure TStringGridHelper.clearCol(const aColIx: integer);
begin
  for var rowIx := 1 to rowCount - 1 do cells[aColIx, rowIx] := '';
end;

procedure TStringGridHelper.initGrid;
const
  cNoSelection: TGridRect = (Left: 0; Top: -1; Right: 0; Bottom: -1);

  procedure clearStringGrid;
  var
    c, r: integer;
  begin
    for c := 0 to pred(colCount) do
      for r := 0 to pred(rowCount) do
        cells[c, r] := '';
  end;

begin
  clearStringGrid;
  defaultRowHeight  := 16;
  cells[0, 0]       := 'file';
  cells[1, 0]       := 'password';
  cells[2, 0]       := 'result';
  colWidths[0]      := 400;
  colWidths[1]      := 200;
  rowCount          := 1;
  selection         := cNoSelection; // prevent the "file" column header from being auto-selected
end;

{ TFileFinder }

function TFileFinder.findFiles: boolean;
var
    RC: integer;
    SR: TSearchRec;
begin
  result := FALSE;
  RC := findFirst(FRootFolder + FFileMask, faAnyFile - faDirectory - faHidden - faSysFile, SR);

  while RC = 0 do begin
                    case assigned(FFindFunc) of TRUE: FFindFunc(FConfigRec, FRootFolder + SR.name); end;
                    RC := findNext(SR); end;

  findClose(SR);
  result := TRUE;
end;

procedure TFileFinder.setConfigRec(const aValue: TConfigRec);
begin
  FConfigRec := aValue;
end;

procedure TFileFinder.setFileMask(const aValue: string);
begin
  FFileMask := aValue;
end;

procedure TFileFinder.setFindFunc(const aValue: TFindFunc);
begin
  FFindFunc := aValue;
end;

procedure TFileFinder.setRootFolder(const aValue: string);
begin
  FRootFolder := aValue;
end;

end.
