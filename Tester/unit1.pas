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
  var VertexBuffer: TGLuint;
  var IndexBuffer: TGLuint;
  var VertexShader: TGLuint;
  var PixelShader: TGLuint;
  var UniformWVP: TGLint;
  var Shader: TGLuint;
public
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
end;

var Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Initialize;
  const Vertices: array[0..3] of packed record
    p: TUVec3;
    c: TUVec4;
  end = (
    (p: (-1, 1, 0); c: (1, 0, 0, 1)),
    (p: (1, 1, 0); c: (0, 1, 0, 1)),
    (p: (-1, -1, 0); c: (0, 0, 1, 1)),
    (p: (1, -1, 0); c: (1, 1, 0, 1))
  );
  const Indices: array[0..5] of TGLushort = (
    0, 1, 3, 0, 3, 2
  );
  var ShaderSource: String;
  var Ptr: Pointer;
  var i: Integer;
  var ErrorBuffer: array[0..511] of AnsiChar;
begin
  glGenVertexArrays(1, @VertexArray);
  glGenBuffers(1, @VertexBuffer);
  glGenBuffers(1, @IndexBuffer);
  glBindVertexArray(VertexArray);
  glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(Vertices), @Vertices, GL_STATIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IndexBuffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(Indices), @Indices, GL_STATIC_DRAW);
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
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(0));
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(12));
  glEnableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
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
begin
  W := TUMat.RotationY(((GetTickCount64 mod 4000) / 4000) * UTwoPi);
  v := TUMat.View(TUVec3.Make(0, 1.5, -2), TUVec3.Zero, TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(UPi * 0.5, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glClearColor(0.4, 1, 0.8, 1);
  //glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT);// or GL_DEPTH_BUFFER_BIT);

  glBindVertexArray(VertexArray);
  glUseProgram(Shader);
  glUniformMatrix4fv(UniformWVP, 1, GL_TRUE, @WVP);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_SHORT, nil);
  //glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
end;

end.

