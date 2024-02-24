unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  CommonUtils, MediaUtils, Setup;

type TForm1 = class(TCommonForm)
private
  var VertexArray: TGLuint;
  var VertexBuffer: array [0..1] of TGLuint;
  var IndexBuffer: TGLuint;
  var VertexShader: TGLuint;
  var PixelShader: TGLuint;
  var UniformWVP: TGLint;
  var Shader: TGLuint;
  var IndexCount: UInt32;
public
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
end;

var Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Initialize;
  var Positions: array of TUVec3;
  var Colors: array of TUVec4;
  var Indices: array of UInt32;
  var CurPosition: UInt32;
  var CurIndex: UInt32;
  function AddPosition(const Value: TUVec3): UInt32;
  begin
    Result := CurPosition;
    Positions[CurPosition] := Value;
    Inc(CurPosition);
  end;
  function AddIndex(const Value: Int32): UInt32;
  begin
    Result := CurIndex;
    Indices[CurIndex] := Value;
    Inc(CurIndex);
  end;
  procedure AddQuad(const v0, v1, v2, v3: TUVec3);
    var i0, i1, i2, i3: Int32;
  begin
    i0 := AddPosition(v0);
    i1 := AddPosition(v1);
    i2 := AddPosition(v2);
    i3 := AddPosition(v3);
    AddIndex(i0); AddIndex(i1); AddIndex(i2);
    AddIndex(i2); AddIndex(i1); AddIndex(i3);
  end;
  procedure GenerateBuffers;
    const Radius: TUFloat = 1;
    const SegH = 30;
    const SegV = 30;
    var v, h, i, j: Int32;
    var p0, p1: TUVec2;
    var r: TURot2;
    var t, cr0, cr1, y0, y1, tw: TUFloat;
    var cw: array of TUFloat;
    const cp: array [0..3] of record
      Pos: TUVec3;
      Col: TUVec4;
    end = (
      (Pos: (0, -1.1, 0); Col: (1, 0, 0, 1)),
      (Pos: (0, 1.1, 0); Col: (0, 0, 1, 1)),
      (Pos: (-1.1, 0, 0); Col: (0, 1, 0, 1)),
      (Pos: (1.10, 1.1, 0); Col: (1, 1, 0, 1))
    );
  begin
    CurPosition := 0;
    CurIndex := 0;
    SetLength(Positions, 4 * SegV * SegH);
    SetLength(Indices, 6 * SegV * SegH);
    r := TURot2.Make(2 * Pi / SegH);
    cr1 := 0;
    y1 := -Radius;
    for v := 0 to SegV - 1 do
    begin
      t := (v + 1) / SegV;
      y0 := y1;
      y1 := -Cos(t * Pi) * Radius;
      cr0 := cr1;
      cr1 := Sin(Pi * t) * Radius;
      p0 := TUVec2.Make(1, 0);
      p1 := p0.Transform(r);
      for h := 0 to SegH - 1 do
      begin
        AddQuad(
          TUVec3.Make(p0.x * cr0, y0, p0.y * cr0),
          TUVec3.Make(p1.x * cr0, y0, p1.y * cr0),
          TUVec3.Make(p0.x * cr1, y1, p0.y * cr1),
          TUVec3.Make(p1.x * cr1, y1, p1.y * cr1)
        );
        p0 := p1;
        p1 := p0.Transform(r);
      end;
    end;
    IndexCount := CurIndex;
    cw := nil;
    SetLength(cw, Length(cp));
    SetLength(Colors, Length(Positions));
    for i := 0 to High(Colors) do
    begin
      tw := 0;
      for j := 0 to High(cp) do
      begin
        cw[j] := (cp[j].Pos - Positions[i]).Len;
        cw[j] *= cw[j];
        if (cw[j] > 0) then cw[j] := 1 / cw[j] else cw[j] := 1;
        tw += cw[j];
      end;
      Colors[i] := TUVec4.Zero;
      for j := 0 to High(cw) do
      begin
        cw[j] := cw[j] / tw;
        Colors[i] := Colors[i] + cp[j].Col * cw[j];
      end;
    end;
  end;
  var ShaderSource: String;
  var Ptr: Pointer;
  var i: Integer;
  var ErrorBuffer: array[0..511] of AnsiChar;
begin
  glGenVertexArrays(1, @VertexArray);
  glGenBuffers(Length(VertexBuffer), @VertexBuffer);
  glGenBuffers(1, @IndexBuffer);
  GenerateBuffers;
  glBindVertexArray(VertexArray);
  glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer[0]);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TUVec3) * Length(Positions), @Positions[0], GL_STATIC_DRAW);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(TUVec3), Pointer(0));
  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer[1]);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(TUVec4) * Length(Colors), @Colors[0], GL_STATIC_DRAW);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(Colors[0]), Pointer(0));
  glEnableVertexAttribArray(1);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IndexBuffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(UInt32) * Length(Indices), @Indices[0], GL_STATIC_DRAW);
  VertexShader := glCreateShader(GL_VERTEX_SHADER);
  ShaderSource := UFileToStr('shader_vs.txt');
  Ptr := PAnsiChar(ShaderSource);
  glShaderSource(VertexShader, 1, @Ptr, nil);
  glCompileShader(VertexShader);
  glGetShaderiv(VertexShader, GL_COMPILE_STATUS, @i);
  if i = 0 then
  begin
    glGetShaderInfoLog(VertexShader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  PixelShader := glCreateShader(GL_FRAGMENT_SHADER);
  ShaderSource := UFileToStr('shader_ps.txt');
  Ptr := PAnsiChar(ShaderSource);
  glShaderSource(PixelShader, 1, @Ptr, nil);
  glCompileShader(PixelShader);
  glGetShaderiv(PixelShader, GL_COMPILE_STATUS, @i);
  if i = 0 then
  begin
    glGetShaderInfoLog(PixelShader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  Shader := glCreateProgram();
  glAttachShader(Shader, VertexShader);
  glAttachShader(Shader, PixelShader);
  glLinkProgram(Shader);
  glGetProgramiv(Shader, GL_LINK_STATUS, @i);
  if i = 0 then
  begin
    glGetProgramInfoLog(Shader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  glDeleteShader(PixelShader);
  glDeleteShader(VertexShader);
  glBindVertexArray(0);
  UniformWVP := glGetUniformLocation(Shader, PGLchar(PAnsiChar('WVP')));
end;

procedure TForm1.Finalize;
begin
  glDeleteProgram(Shader);
  glDeleteBuffers(1, @VertexBuffer);
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
  var d: TUFloat;
begin
  d := 1.5 + Sin(((GetTickCount64 mod 4000) / 4000) * UTwoPi) * 0.5;
  W := TUMat.RotationY(((GetTickCount64 mod 4000) / 4000) * UTwoPi);
  V := TUMat.View(TUVec3.Make(0, 1.5, -2) * d, TUVec3.Zero, TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(UPi * 0.5, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.4, 1, 0.8, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  //glPolygonMode(GL_FRONT, GL_LINE);
  //glPolygonMode(GL_BACK, GL_LINE);

  glBindVertexArray(VertexArray);
  glUseProgram(Shader);
  glUniformMatrix4fv(UniformWVP, 1, GL_TRUE, @WVP);
  glDrawElements(GL_TRIANGLES, IndexCount, GL_UNSIGNED_INT, nil);
  //glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
end;

end.

