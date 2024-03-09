program HeaderGen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  OpenSSL,
  OpenSSLSockets,
  FPHttpClient,
  CommonUtils;

type
  TMyHttpClient = class (TFPHTTPClient)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { THeaderGenXML }

  THeaderGenXML = class (TUXML)
  public
    class constructor CreateClass;
  end;
  THeaderGenXMLRef = specialize TUSharedRef<THeaderGenXML>;

  THeaderGen = class
  public
    type TGLAPI = (glapi_gl, glapi_gles);
    type TTypeDecl = object
      Platform: String;
      Name: String;
      Desc: String;
      ArrCount: Integer;
      PtrCount: Integer;
      IsConst: Boolean;
      IsRef: Boolean;
      IsVoid: Boolean;
      IsFunc: Boolean;
      IsStruct: Boolean;
      Params: array of TTypeDecl;
      Members: array of TTypeDecl;
      procedure Reset;
      procedure AddParam(const Param: TTypeDecl);
      procedure AddMember(const Member: TTypeDecl);
      function ToString(
        const Separator: String = ' = ';
        const NameTypeCheck: Boolean = False
      ): String;
    end;
    type TEnumDecl = object
      Platform: String;
      Name: String;
      AliasName: String;
      Value: LongWord;
      procedure Reset;
      function HasAlias: Boolean;
      function ToString: String;
    end;
    type TFuncDecl = object
      Platform: String;
      FuncDesc: TTypeDecl;
      Params: array of TTypeDecl;
      AliasName: String;
      procedure Reset;
      function HasAlias: Boolean;
      procedure AddParam(const Param: TTypeDecl);
      function ToString(const Separator: String = ' = '): String;
      function ToDebugImpl: String;
    end;
  private
    class var SynType: TUParserSyntax;
    var TypeRenames: array of String;
    procedure AddTypeRename(const TypeName: String);
    function FindTypeRename(const TypeName: String): Boolean;
    function VerifyRename(const TypeName: String): String;
    procedure DeclPtr(const Decl: TTypeDecl);
    function ParseDecl(const Desc: String; out Decl: TTypeDecl): Boolean;
    function ProcessType(const Node: TUXML): Boolean;
    function ProcessEnum(const Node: TUXML): Boolean;
    function ProcessCommand(const Node: TUXML): Boolean;
    function TranslateType(const TypeDesc: String; const Ptr: Integer = 0): String;
    function ValidateName(const NameStr: String; const TypeCheck: Boolean = False): String;
    function CheckAPI(const APIStr: String): Boolean;
  public
    var TypeDefs: array of TTypeDecl;
    var EnumDefs: array of TEnumDecl;
    var FuncDefs: array of TFuncDecl;
    var API: TGLAPI;
    var Platform: String;
    class constructor CreateClass;
    constructor Create;
    function Process(const xml: TUXML): Boolean;
    procedure FwdType(const TypeName: String);
  end;
//*)

procedure StrAppendSp(var Str: String; const SubStr: String; const Sep: String = ' ');
begin
  if Length(Str) > 0 then Str += Sep;
  Str += SubStr;
end;

var RootDir: String;
var Gen: THeaderGen;

class constructor THeaderGenXML.CreateClass;
begin
  _SyntaxTags.AddComment('<![', ']>');
  _SyntaxContent.AddComment('<![', ']>');
end;

//(*
procedure THeaderGen.TTypeDecl.Reset;
begin
  Platform := '';
  Name := '';
  Desc := '';
  ArrCount := 1;
  PtrCount := 0;
  IsConst := False;
  IsRef := False;
  IsVoid := False;
  IsFunc := False;
  IsStruct := False;
  SetLength(Params, 0);
  SetLength(Members, 0);
end;

procedure THeaderGen.TTypeDecl.AddParam(const Param: TTypeDecl);
begin
  specialize UArrAppend<TTypeDecl>(Params, Param);
end;

procedure THeaderGen.TTypeDecl.AddMember(const Member: TTypeDecl);
begin
  specialize UArrAppend<TTypeDecl>(Members, Member);
end;

function THeaderGen.TTypeDecl.ToString(const Separator: String; const NameTypeCheck: Boolean): String;
  var i: Integer;
begin
  Result := Gen.ValidateName(Name);
  if not IsFunc then Result := Gen.VerifyRename(Result);
  Result += Separator;
  if IsFunc then
  begin
    if IsVoid then Result += 'procedure (' else Result += 'function (';
    for i := 0 to High(Params) do
    begin
      if i > 0 then Result += '; ';
      Result += Params[i].ToString(': ', True);
    end;
    Result += ')';
    if not IsVoid then Result += ': ' + Desc;
    Result += ' libdecl';
  end
  else if IsStruct then
  begin
    Result += 'record'#$D#$A;
    for i := 0 to High(Members) do
    begin
      Result += '    ' + Gen.ValidateName(Members[i].Name) + ': ';
      if Members[i].ArrCount > 1 then
      begin
        Result += 'array [0..' + IntToStr(Members[i].ArrCount - 1) + '] of ';
      end;
      Result += Members[i].Desc + ';'#$D#$A;
    end;
    Result += '  end';
  end
  else
  begin
    if IsConst then Result := 'const ' + Result
    else if IsRef then Result := 'var ' + Result;
    if Desc = 'record' then
    begin
      Result += Desc + ' end';
    end
    else
    begin
      Result += Gen.VerifyRename(Desc);
    end;
  end;
end;

procedure THeaderGen.TEnumDecl.Reset;
begin
  Platform := '';
  Name := '';
  AliasName := '';
  Value := 0;
end;

function THeaderGen.TEnumDecl.HasAlias: Boolean;
begin
  Result := Length(AliasName) > 0;
end;

function THeaderGen.TEnumDecl.ToString: String;
begin
  Result := Gen.ValidateName(Name) + ' = $' + IntToHex(Value);
end;

procedure THeaderGen.TFuncDecl.Reset;
begin
  Platform := '';
  FuncDesc.Reset;
  SetLength(Params, 0);
  AliasName := '';
end;

function THeaderGen.TFuncDecl.HasAlias: Boolean;
begin
  Result := Length(AliasName) > 0;
end;

procedure THeaderGen.TFuncDecl.AddParam(const Param: TTypeDecl);
begin
  specialize UArrAppend<TTypeDecl>(Params, Param);
end;

function THeaderGen.TFuncDecl.ToString(const Separator: String): String;
  var i: Integer;
begin
  Result := FuncDesc.Name + Separator;
  if FuncDesc.IsVoid then Result += 'procedure' else Result += 'function';
  if Length(Params) > 0 then
  begin
    Result += ' (';
    for i := 0 to High(Params) do
    begin
      if i > 0 then Result += '; ';
      Result += Params[i].ToString(': ');
    end;
    Result += ')';
  end;
  if not FuncDesc.IsVoid then
  begin
    Result += ': ' + FuncDesc.Desc;
  end;
end;

function THeaderGen.TFuncDecl.ToDebugImpl: String;
  var i: Int32;
begin
  if FuncDesc.IsVoid then Result := 'procedure ' else Result := 'function ';
  Result += FuncDesc.Name + '_Debug(';
  for i := 0 to High(Params) do
  begin
    if i > 0 then Result += '; ';
    Result += Params[i].ToString(': ', True);
  end;
  Result += ')';
  if not FuncDesc.IsVoid then
  begin
    Result += ': ' + FuncDesc.Desc;
  end;
  Result += ' libdecl;'#$D#$A;
  Result += 'begin'#$D#$A;
  Result += '  glDebugFrame := get_frame;'#$D#$A;
  Result += '  ';
  if not FuncDesc.IsVoid then
  begin
    Result += 'Result := ';
  end;
  Result += FuncDesc.Name + '_Direct(';
  for i := 0 to High(Params) do
  begin
    if i > 0 then Result += ', ';
    Result += Gen.ValidateName(Params[i].Name);
  end;
  Result += ');'#$D#$A;
  Result += 'end;'#$D#$A;
end;

procedure THeaderGen.AddTypeRename(const TypeName: String);
begin
  if FindTypeRename(TypeName) then Exit;
  specialize UArrAppend<String>(TypeRenames, TypeName);
end;

function THeaderGen.FindTypeRename(const TypeName: String): Boolean;
  var i: Int32;
begin
  for i := 0 to High(TypeRenames) do
  if LowerCase(TypeRenames[i]) = LowerCase(TypeName) then Exit(True);
  Result := False;
end;

function THeaderGen.VerifyRename(const TypeName: String): String;
  var tn: String;
begin
  tn := StringReplace(TypeName, '^', '', [rfReplaceAll]);
  if FindTypeRename(tn) then Exit(TypeName + '_');
  Result := TypeName;
end;

procedure THeaderGen.DeclPtr(const Decl: TTypeDecl);
  var i, j, p: Integer;
  var add: Boolean;
  var d: TTypeDecl;
begin
  for i := 0 to Decl.PtrCount - 1 do
  begin
    d := Decl;
    d.IsConst := False;
    d.IsRef := False;
    d.IsFunc := False;
    d.IsVoid := False;
    for p := 0 to i do
    begin
      d.Name := 'P' + d.Desc;
      if p = i then d.Desc := '^' + d.Desc
      else d.Desc := 'P' + d.Desc;
    end;
    d.PtrCount := i + 1;
    add := True;
    for j := 0 to High(TypeDefs) do
    if LowerCase(TypeDefs[j].Name) = LowerCase(d.Name) then
    begin
      add := False;
      Break;
    end;
    if not add then Continue;
    specialize UArrAppend<TTypeDecl>(TypeDefs, d);
  end;
end;

function THeaderGen.ParseDecl(const Desc: String; out Decl: TTypeDecl): Boolean;
  var t: TUParserToken;
  var p: TUParser;
  var Param: String;
  var Member: String;
  var pd: TTypeDecl;
  var i: Integer;
begin
  Result := False;
  p := TUParser.Create;
  try
    p.Parse(Desc);
    p.Syntax := @SynType;
    Decl.Reset;
    Decl.Platform := Platform;
    repeat
      t := p.NextToken;
      if t = 'typedef' then begin end
      else if t = 'const' then Decl.IsConst := True
      else if t = '#include' then Exit
      else if t = '*' then Inc(Decl.PtrCount)
      else if t = '&' then Decl.IsRef := True
      else if t = ';' then Exit(True)
      else if t = '(' then
      begin
        Decl.Desc := Decl.Name;
        Decl.IsFunc := True;
        repeat
          t := p.NextToken;
          if t = tt_word then Decl.Name := t;
          if t = ')' then Break;
        until t = tt_eof;
        if t = tt_eof then Exit;
        t := p.NextToken;
        Param := '';
        if t <> '(' then Exit;
        repeat
          t := p.NextToken;
          if t = [',', ')'] then
          begin
            if (Length(Param) > 0) and ParseDecl(Param, pd) then
            begin
              Decl.AddParam(pd);
            end;
            Param := '';
            if t = ')' then Break;
          end
          else
          begin
            StrAppendSp(Param, t.Value);
          end;
        until t = tt_eof;
      end
      else if t = '{' then
      begin
        if not ((Decl.Desc = 'struct') or (Decl.Desc = 'class')) then Exit;
        Decl.IsStruct := True;
        Member := '';
        repeat
          t := p.NextToken;
          if t = [',', ';', '}'] then
          begin
            if (Length(Member) > 0) then
            begin
              if ParseDecl(Member, pd) then
              begin
                Decl.AddMember(pd);
              end;
              if t = ',' then Member := UStrExplode(Member, ' ')[0]
              else Member := '';
            end;
            if t = '}' then Break;
          end
          else
          begin
            StrAppendSp(Member, t.Value);
          end;
        until t = tt_eof;
      end
      else if t = tt_keyword then
      begin
        StrAppendSp(Decl.Desc, t.Value);
      end
      else if t = tt_word then
      begin
        if Length(Decl.Name) > 0 then
        begin
          StrAppendSp(Decl.Desc, Decl.Name);
        end;
        Decl.Name := t.Value;
      end
      else if t = '[' then
      begin
        t := p.NextToken;
        if t <> tt_number then Exit;
        Decl.ArrCount := StrToIntDef(t.Value, 1);
        t := p.NextToken;
        if t <> ']' then Exit;
      end;
    until t = tt_eof;
    Result := Length(Decl.Desc) > 0;
  finally
    if Result then
    begin
      if (LowerCase(Decl.Desc) = 'void') and (Decl.PtrCount = 0) then
      begin
        Decl.Desc := 'void';
        Decl.IsVoid := True;
      end;
      Decl.Desc := TranslateType(Decl.Desc, Decl.PtrCount);
      if LowerCase(Decl.Desc) = 'pointer' then Dec(Decl.PtrCount);
      if Decl.PtrCount > 0 then
      begin
        DeclPtr(Decl);
        for i := 0 to Decl.PtrCount - 1 do
        begin
          Decl.Desc := 'P' + Decl.Desc;
        end;
      end;
    end;
    p.Free;
  end;
end;

function THeaderGen.ProcessType(const Node: TUXML): Boolean;
  const TypeOverrides: array[0..4] of array[0..1] of String = (
    ('GLhandleARB', 'unsigned int'),
    ('GLhandle', 'unsigned int'),
    ('GLsync', 'void *'),
    ('FLOAT', 'float'),
    ('INT', 'int')
  );
  var TypeName: String;
  var n: TUXML;
  var c: String;
  var d: TTypeDecl;
  var i: Integer;
begin
  Result := False;
  TypeName := Node.AttributeValue['name'];
  if Length(TypeName) = 0 then
  begin
    n := Node.FindChild('name');
    if Assigned(n) then TypeName := n.Content;
  end;
  c := Node.Content;
  if c.StartsWith('DECLARE_HANDLE', True) then
  begin
    c := 'typedef HANDLE ' + TypeName + ';';
  end;
  for i := 0 to High(TypeOverrides) do
  if TypeName = TypeOverrides[i][0] then
  begin
    c := 'typedef ' + TypeOverrides[i][1] + ' ' + TypeName + ';';
    Break;
  end;
  if Length(c) = 0 then Exit;
  WriteLn(c, ' -> ');
  if Gen.ParseDecl(c, d) then
  begin
    if Length(d.Name) = 0 then d.Name := TypeName;
    specialize UArrAppend<TTypeDecl>(TypeDefs, d);
    WriteLn(d.ToString);
    Result := True;
  end;
end;

function THeaderGen.ProcessEnum(const Node: TUXML): Boolean;
  var d: TEnumDecl;
  var v, api_str: String;
begin
  Result := False;
  d.Reset;
  d.Platform := Platform;
  if Node.Name <> 'enum' then Exit;
  d.Name := Node.AttributeValue['name'];
  if Length(d.Name) = 0 then Exit;
  api_str := Node.AttributeValue['api'];
  if (Length(api_str) > 0) and not CheckAPI(api_str) then Exit;
  v := Node.AttributeValue['value'];
  v := StringReplace(v, '0x', '$', []);
  d.Value := StrToIntDef(v, 0);
  d.AliasName := Node.AttributeValue['alias'];
  //WriteLn('enum ' + d.ToString);
  //if d.HasAlias then WriteLn(d.AliasName + ' = ' + d.Name);
  specialize UArrAppend<TEnumDecl>(EnumDefs, d);
  Result := True;
end;

function THeaderGen.ProcessCommand(const Node: TUXML): Boolean;
  var d: TFuncDecl;
  var pd: TTypeDecl;
  var Proto: TUXML;
  var s: String;
  var i: Integer;
begin
  Result := False;
  d.Reset;
  d.Platform := Platform;
  if Node.Name <> 'command' then Exit;
  Proto := Node.FindChild('proto');
  if not Assigned(Proto) then Exit;
  s := Proto.Content;
  if not Gen.ParseDecl(s, d.FuncDesc) then Exit;
  for i := 0 to Node.ChildCount - 1 do
  if Node[i].Name = 'param' then
  begin
    s := Node[i].Content;
    if not Gen.ParseDecl(s, pd) then
    begin
      WriteLn('Failed to parse function. ' + d.FuncDesc.Name);
      if not Gen.ParseDecl(s, pd) then Exit;
    end;
    d.AddParam(pd);
  end;
  WriteLn(d.ToString(': '));
  specialize UArrAppend<TFuncDecl>(FuncDefs, d);
  Result := True;
end;

function THeaderGen.TranslateType(const TypeDesc: String; const Ptr: Integer = 0): String;
  var DescArr: TUStrArr;
  var BaseType: String;
  var PtrCount: Integer;
  var LongCount: Integer;
  var ShortCount: Integer;
  var UnsCount: Integer;
  var i: Integer;
begin
  Result := TypeDesc;
  DescArr := UStrExplode(TypeDesc, ' ');
  BaseType := '';
  PtrCount := Ptr;
  LongCount := 0;
  ShortCount := 0;
  UnsCount := 0;
  for i := High(DescArr) downto 0 do
  begin
    if DescArr[i] = '*' then Inc(PtrCount)
    else if Length(BaseType) = 0 then BaseType := DescArr[i]
    else if DescArr[i] = 'unsigned' then Inc(UnsCount)
    else if DescArr[i] = 'long' then Inc(LongCount)
    else if DescArr[i] = 'short' then Inc(ShortCount);
  end;
  if Length(BaseType) = 0 then Exit;
  if BaseType = 'short' then
  begin
    BaseType := 'int';
    Inc(ShortCount);
  end
  else if BaseType = 'long' then
  begin
    BaseType := 'int';
    Inc(LongCount);
  end;
  BaseType := StringReplace(BaseType, 'khronos_', '', [rfReplaceAll]);
  if Platform = 'linux' then
  begin
    if BaseType = 'XID' then
    begin
      BaseType := 'TXID';
    end
    else if BaseType = 'Window' then
    begin
      BaseType := 'TWindow';
    end
    else if BaseType = 'Display' then
    begin
      BaseType := 'TXDisplay';
    end
    else if BaseType = 'XVisualInfo' then
    begin
      BaseType := 'TXVisualInfo';
    end
    else if BaseType = 'Pixmap' then
    begin
      BaseType := 'TPixmap';
    end
    else if BaseType = 'Colormap' then
    begin
      BaseType := 'TColormap';
    end
    else if BaseType = 'Status' then
    begin
      BaseType := 'TStatus';
    end
    else if BaseType = 'Font' then
    begin
      BaseType := 'TFont';
    end
    else if BaseType = 'Bool' then
    begin
      BaseType := 'TBool';
    end;
  end;
  if BaseType = 'int8_t' then
  begin
    BaseType := 'char';
  end
  else if BaseType = 'uint8_t' then
  begin
    BaseType := 'char';
    Inc(UnsCount);
  end
  else if BaseType = 'int16_t' then
  begin
    BaseType := 'int';
    Inc(ShortCount);
  end
  else if BaseType = 'uint16_t' then
  begin
    BaseType := 'int';
    Inc(ShortCount);
    Inc(UnsCount);
  end
  else if BaseType = 'int32_t' then
  begin
    BaseType := 'int';
  end
  else if BaseType = 'uint32_t' then
  begin
    BaseType := 'int';
    Inc(UnsCount);
  end
  else if BaseType = 'int64_t' then
  begin
    BaseType := 'int';
    Inc(LongCount, 2);
  end
  else if BaseType = 'uint64_t' then
  begin
    BaseType := 'int';
    Inc(LongCount, 2);
    Inc(UnsCount);
  end
  else if BaseType = 'float_t' then
  begin
    BaseType := 'float';
  end
  else if BaseType = 'time_ns_t' then
  begin
    BaseType := 'int';
    Inc(LongCount, 2);
    Inc(UnsCount);
  end;
  if (BaseType = 'int') and (LongCount > 0) then Dec(LongCount);
  while (LongCount > 0) and (ShortCount > 0) do
  begin
    Dec(LongCount);
    Dec(ShortCount);
  end;
  if (LowerCase(BaseType) = 'void') then
  begin
    BaseType := 'Pointer';
    Dec(PtrCount);
  end
  else if (BaseType = 'struct') then
  begin
    BaseType := 'record';
  end
  else if (BaseType = 'bool') then
  begin
    BaseType := 'Boolean';
  end
  else if (BaseType = 'intptr_t') or (BaseType = 'ssize_t') then
  begin
    BaseType := 'PtrInt';
  end
  else if (BaseType = 'uintptr_t') or (BaseType = 'usize_t') then
  begin
    BaseType := 'PtrUInt';
  end
  else if (BaseType = 'char') then
  begin
    BaseType := specialize USelect<String>(UnsCount > 0, 'Byte', 'ShortInt');
  end
  else if (BaseType = 'int') then
  begin
    if ShortCount > 0 then BaseType := specialize USelect<String>(UnsCount > 0, 'Word', 'SmallInt')
    else if LongCount > 0 then BaseType := specialize USelect<String>(UnsCount > 0, 'QWord', 'Int64')
    else BaseType := specialize USelect<String>(UnsCount > 0, 'LongWord', 'LongInt');
  end
  else if (BaseType = 'float') then
  begin
    BaseType := 'Single';
  end
  else if (BaseType = 'double') then
  begin
    BaseType := 'Double';
  end;
  Result := BaseType;
end;

function THeaderGen.ValidateName(const NameStr: String; const TypeCheck: Boolean): String;
  const InvalidNames: array[0..18] of String = (
    'program', 'type', 'string', 'message',
    'procedure', 'function', 'record', 'unit',
    'array', 'packed', 'label', 'object',
    'begin', 'end', 'pointer', 'in',
    'out', 'var', 'property'
  );
  var i: Integer;
  var nls: String;
  var dt: TTypeDecl;
begin
  nls := LowerCase(NameStr);
  for i := 0 to High(InvalidNames) do
  if nls = InvalidNames[i] then Exit('_' + NameStr);
  if TypeCheck then
  begin
    for dt in TypeDefs do
    if nls = LowerCase(dt.Name) then Exit('_' + NameStr);
  end;
  Result := NameStr;
end;

function THeaderGen.CheckAPI(const APIStr: String): Boolean;
  var s: String;
begin
  s := LowerCase(APIStr);
  case API of
    glapi_gl: Exit(s = 'gl');
    glapi_gles: Exit(s = 'gles2');
  end;
  Result := False;
end;

class constructor THeaderGen.CreateClass;
begin
  with SynType do
  begin
    AddSymbols(['*', '&', '(', ')', ';', ',', '[', ']']);
    AddKeywords(['#include', 'typedef', 'const', 'class', 'struct']);
    AddCommentLine('//');
    AddComment('/*', '*/')
  end;
end;

constructor THeaderGen.Create;
begin
  API := glapi_gl;
end;

function THeaderGen.Process(const xml: TUXML): Boolean;
  var i, j: Integer;
  var dt: TTypeDecl;
begin
  Result := False;
  if xml.Name <> 'registry' then Exit;
  for i := 0 to xml.ChildCount -  1 do
  begin
    if xml[i].Name = 'types' then
    begin
      for j := 0 to xml[i].ChildCount - 1 do
      begin
        ProcessType(xml[i][j]);
      end;
    end
    else if xml[i].Name = 'enums' then
    begin
      for j := 0 to xml[i].ChildCount - 1 do
      begin
        ProcessEnum(xml[i][j]);
      end;
    end
    else if xml[i].Name = 'commands' then
    begin
      for j := 0 to xml[i].ChildCount - 1 do
      begin
        ProcessCommand(xml[i][j]);
      end;
    end;
  end;
  for i := 0 to High(TypeDefs) do
  begin
    if TypeDefs[i].Platform <> Platform then Continue;
    for j := 0 to High(FuncDefs) do
    begin
      if (FuncDefs[j].Platform <> Platform) then Continue;
      if (
         (LowerCase(TypeDefs[i].Name) = LowerCase(FuncDefs[j].AliasName))
         or (LowerCase(TypeDefs[i].Name) = LowerCase('T' + FuncDefs[j].AliasName))
         or (LowerCase(TypeDefs[i].Name) = LowerCase(FuncDefs[j].FuncDesc.Name))
         or (LowerCase(TypeDefs[i].Name) = LowerCase('T' + FuncDefs[j].FuncDesc.Name))
      ) then
      begin
        AddTypeRename(TypeDefs[i].Name);
      end;
    end;
  end;
  Result := True;
end;

procedure THeaderGen.FwdType(const TypeName: String);
  var d: TTypeDecl;
begin
  d.Reset;
  d.Name := TypeName;
  d.Desc := 'record';
  d.Platform := Platform;
  specialize UArrAppend<TTypeDecl>(TypeDefs, d);
end;

constructor TMyHttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MaxChunkSize := 0;
  AllowRedirect := True;
end;

procedure DownloadXML;
  const urls: array[0..5] of String = (
    'gl.xml',
    'https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/gl.xml',
    'glx.xml',
    'https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/glx.xml',
    'wgl.xml',
    'https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/wgl.xml'
  );
  var Path: String;
  var i: Integer;
begin
  if not DirectoryExists(RootDir + '/xml') then CreateDir(RootDir + '/xml');
  for i := 0 to (Length(urls) shr 1) - 1 do
  begin
    Path := RootDir + '/xml/' + urls[i * 2];
    if not FileExists(Path) then
    begin
      WriteLn('getting ', urls[i * 2]);
      TMyHttpClient.SimpleGet(urls[i * 2 + 1], Path);
    end;
  end;
end;

procedure Run;
  var s: String;
  var xml: THeaderGenXMLRef;
  var dt: THeaderGen.TTypeDecl;
  var de: THeaderGen.TEnumDecl;
  var df: THeaderGen.TFuncDecl;
  var Templ: String;
  var Platform: String;
  const GDIFunctions: array[0..4] of String = (
    'ChoosePixelFormat',
    'DescribePixelFormat',
    'GetPixelFormat',
    'SetPixelFormat',
    'SwapBuffers'
  );
  function PlatformBegin(const Platform: String): String;
  begin
    if Length(Platform) = 0 then Exit('');
    Result := '{$if defined(' + UpperCase(Platform) + ')}'#$D#$A;
  end;
  function PlatformEnd(const Platform: String): String;
  begin
    if Length(Platform) = 0 then Exit('');
    Result := '{$endif}'#$D#$A;
  end;
  function IsGDI(const FuncName: String): Boolean;
    var s: String;
  begin
    for s in GDIFunctions do
    if FuncName = s then
    begin
      Exit(True);
    end;
    Result := False;
  end;
  const Platforms: array of String = (
    'windows', 'linux', ''
  );
begin
  DownloadXML;
  Gen := THeaderGen.Create;
  try
    Templ := UFileToStr(RootDir + '/PasOpenGL.template');
    s := UFileToStr(RootDir + '/xml/gl.xml');
    xml := TUXML.Load(s);
    if xml.IsValid then Gen.Process(xml.Ptr);
    s := UFileToStr(RootDir + '/xml/wgl.xml');
    xml := TUXML.Load(s);
    if not xml.IsValid then Exit;
    Gen.Platform := 'windows';
    Gen.Process(xml.Ptr);
    Gen.Platform := '';
    s := UFileToStr(RootDir + '/xml/glx.xml');
    xml := TUXML.Load(s);
    if not xml.IsValid then Exit;
    Gen.Platform := 'linux';
    Gen.FwdType('__GLXFBConfigRec');
    Gen.FwdType('__GLXcontextRec');
    Gen.FwdType('DMparams');
    Gen.FwdType('DMbuffer');
    Gen.FwdType('VLNode');
    Gen.FwdType('VLPath');
    Gen.FwdType('VLServer');
    Gen.Process(xml.Ptr);
    Gen.Platform := '';
    if Length(Gen.TypeDefs) > 0 then
    begin
      s := 'type'#$D#$A;
      for Platform in Platforms do
      begin
        s += PlatformBegin(Platform);
        for dt in Gen.TypeDefs do
        if dt.Platform = Platform then
        begin
          s += '  ' + dt.ToString + ';'#$D#$A;
          if dt.Name.StartsWith('GL', True) then
          begin
            s += '  T' + Gen.VerifyRename(dt.Name) + ' = ' + Gen.VerifyRename(dt.Name) + ';'#$D#$A;
          end;
        end;
        s += PlatformEnd(Platform);
      end;
    end;
    if Length(Gen.EnumDefs) > 0 then
    begin
      s += 'const'#$D#$A;
      for Platform in Platforms do
      begin
        s += PlatformBegin(Platform);
        for de in Gen.EnumDefs do
        if de.Platform = Platform then
        begin
          if de.HasAlias then
          begin
            s += '  ' + de.Name + ' = ' + de.AliasName + ';'#$D#$A;
          end
          else
          begin
            s += '  ' + de.ToString + ';'#$D#$A;
          end;
        end;
        s += PlatformEnd(Platform);
      end;
    end;
    if Length(Gen.FuncDefs) > 0 then
    begin
      s += 'type'#$D#$A;
      for Platform in Platforms do
      begin
        s += PlatformBegin(Platform);
        for df in Gen.FuncDefs do
        if df.Platform = Platform then
        begin
          s += '  T' + df.ToString(' = ') + '; libdecl;'#$D#$A;
        end;
        s += PlatformEnd(Platform);
      end;
      s += 'var'#$D#$A;
      for Platform in Platforms do
      begin
        s += PlatformBegin(Platform);
        for df in Gen.FuncDefs do
        if df.Platform = Platform then
        begin
          s += '  ' + df.FuncDesc.Name + ': T' + df.FuncDesc.Name + ';'#$D#$A;
          if df.HasAlias then
          begin
            s += '  ' + df.AliasName + ': T' + df.FuncDesc.Name + ';'#$D#$A;
          end;
        end;
        s += PlatformEnd(Platform);
      end;
    end;
    Templ := StringReplace(Templ, '{#intf}', s, []);
    // direct function vars
    s := '';
    if Length(Gen.FuncDefs) > 0 then
    begin
      s += 'var'#$D#$A;
      for Platform in Platforms do
      begin
        s += PlatformBegin(Platform);
        for df in Gen.FuncDefs do
        if df.Platform = Platform then
        begin
          s += '  ' + df.FuncDesc.Name + '_Direct: T' + df.FuncDesc.Name + ';'#$D#$A;
          if df.HasAlias then
          begin
            s += '  ' + df.AliasName + '_Direct: T' + df.FuncDesc.Name + ';'#$D#$A;
          end;
        end;
        s += PlatformEnd(Platform);
      end;
    end;
    Templ := StringReplace(Templ, '{#impl_direct}', s, []);
    // function loading
    s := '';
    if Length(Gen.FuncDefs) > 0 then
    begin
      for Platform in Platforms do
      begin
        s += PlatformBegin(Platform);
        for df in Gen.FuncDefs do
        if df.Platform = Platform then
        begin
          if IsGDI(df.FuncDesc.Name) then
          begin
            s += '  ' + df.FuncDesc.Name + '_Direct := T' + df.FuncDesc.Name + '(@GDI' + df.FuncDesc.Name + ');'#$D#$A;
          end
          else
          begin
            s += '  ' + df.FuncDesc.Name + '_Direct := T' + df.FuncDesc.Name + '(ProcAddress(''' + df.FuncDesc.Name + '''));'#$D#$A;
          end;
          if df.HasAlias then
          begin
            s += '  ' + df.AliasName + '_Direct := @' + df.FuncDesc.Name + '_Direct'#$D#$A;
          end;
        end;
        s += PlatformEnd(Platform);
      end;
      s := s.TrimRight(#$D#$A);
    end;
    Templ := StringReplace(Templ, '{#impl_load}', s, []);
    // debug mode
    s := '';
    if Length(Gen.FuncDefs) > 0 then
    begin
      for Platform in Platforms do
      begin
        s += PLatformBegin(Platform);
        for df in Gen.FuncDefs do
        if df.Platform = Platform then
        begin
          s += df.ToDebugImpl;
        end;
        s += PlatformEnd(Platform);
      end;
      s := s.TrimRight(#$D#$A);
    end;
    Templ := StringReplace(Templ, '{#impl_debug}', s, []);
    s := '';
    if Length(Gen.FuncDefs) > 0 then
    begin
      for Platform in Platforms do
      begin
        s += PlatformBegin(Platform);
        for df in Gen.FuncDefs do
        if df.Platform = Platform then
        begin
          s += '  ' + df.FuncDesc.Name + ' := @' + df.FuncDesc.Name + '_Debug;'#$D#$A;
          if df.HasAlias then
          begin
            s += '  ' + df.AliasName + ' := @' + df.FuncDesc.Name + '_Debug'#$D#$A;
          end;
        end;
        s += PlatformEnd(Platform);
      end;
      s := s.TrimRight(#$D#$A);
    end;
    Templ := StringReplace(Templ, '{#impl_debug_assign}', s, []);
    // direct mode
    s := '';
    if Length(Gen.FuncDefs) > 0 then
    begin
      for Platform in Platforms do
      begin
        s += PlatformBegin(Platform);
        for df in Gen.FuncDefs do
        if df.Platform = Platform then
        begin
          s += '  ' + df.FuncDesc.Name + ' := ' + df.FuncDesc.Name + '_Direct;'#$D#$A;
          if df.HasAlias then
          begin
            s += '  ' + df.AliasName + ' := ' + df.FuncDesc.Name + '_Direct'#$D#$A;
          end;
        end;
        s += PlatformEnd(Platform);
      end;
      s := s.TrimRight(#$D#$A);
    end;
    Templ := StringReplace(Templ, '{#impl_direct_assign}', s, []);
    UStrToFile(ExpandFileName(RootDir + '/../PasOpenGL.pas'), Templ);
  finally
    Gen.Free;
  end;
end;
//*)
begin
  RootDir := ExpandFileName(ExtractFileDir(ParamStr(0)) + '/../');
  Run;
  WriteLn('done.');
  //ReadLn;
end.

