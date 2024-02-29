unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  CommonUtils, MediaUtils, Setup, EasyLazFreeType, LazFreeTypeFontCollection,
  IntfGraphics, GraphType, LazFreeTypeIntfDrawer, FpImage, LazFreeType;

type TForm1 = class(TCommonForm)
private
  var VertexArray: TGLuint;
  var VertexBuffer: TGLuint;
  var IndexBuffer: TGLuint;
  var VertexShader: TGLuint;
  var PixelShader: TGLuint;
  var UniformWVP: TGLint;
  var UniformTex0: TGLint;
  var Shader: TGLuint;
  var MyFont: TFreeTypeFont;
  var Image: TLazIntfImage;
  var Drawer: TIntfFreeTypeDrawer;
  var FontTexture: TGLuint;
  var TextureSize: TUVec2i;
public
  procedure InitializeFont;
  procedure FinalizeFont;
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
end;

var Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.InitializeFont;
  var FontFamilyName: String;
  var TextSize: TUVec2i;
  const TextStr = 'Hello World!';
begin
  FontFamilyName := FontCollection.AddFile('FreeSerif.ttf').Family.FamilyName;
  WriteLn(FontFamilyName);
  MyFont := TFreeTypeFont.Create;
  MyFont.Name := FontFamilyName;
  MyFont.SizeInPoints := 64;
  MyFont.Hinted := True;
  MyFont.ClearType := False;
  MyFont.Quality := grqHighQuality;
  MyFont.SmallLinePadding := false;
  MyFont.Orientation := 0;
  TextSize := TUVec2i.Make(Round(MyFont.TextWidth(TextStr)), Round(MyFont.TextHeight(TextStr)));
  TextureSize := TUVec2i.Make(UPoT(TextSize.x), UPoT(TextSize.y));
  WriteLn('Texture Size = ', TextureSize.x, ' ', TextureSize.y);
  Image := TLazIntfImage.Create(TextureSize.x, TextureSize.y, [riqfGrey]);
  Drawer := TIntfFreeTypeDrawer.Create(Image);
  Drawer.FillPixels(colBlack);
  Drawer.DrawText(
    TextStr, MyFont,
    (TextureSize.x - TextSize.x) shr 1,
    (TextureSize.y - TextSize.y) shr 1 + TextSize.y,
    colWhite
  );
  //Image.SaveToFile('Test.png');
  glGenTextures(1, @FontTexture);
  glBindTexture(GL_TEXTURE_2D, FontTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(
    GL_TEXTURE_2D, 0, GL_RED,
    TextureSize.x, TextureSize.y, 0,
    GL_RED, GL_UNSIGNED_BYTE,
    Image.PixelData
  );
  glGenerateMipmap(GL_TEXTURE_2D);
end;

procedure TForm1.FinalizeFont;
begin
  glDeleteTextures(1, @FontTexture);
  Drawer.Free;
  Image.Free;
  MyFont.Free;
end;

procedure TForm1.Initialize;
  const Vertices: array[0..3] of packed record
    p: TUVec3;
    c: TUVec4;
    t: TUVec2;
  end = (
    (p: (-1, 1, 0); c: (1, 0, 0, 1); t: (0, 0)),
    (p: (1, 1, 0); c: (0, 1, 0, 1); t: (1, 0)),
    (p: (-1, -1, 0); c: (0, 0, 1, 1); t: (0, 1)),
    (p: (1, -1, 0); c: (1, 1, 0, 1); t: (1, 1))
  );
  const Indices: array[0..5] of TGLushort = (
    0, 1, 3, 0, 3, 2
  );
  var ShaderSource: String;
  var Ptr: Pointer;
  var i: Integer;
  var ErrorBuffer: array[0..511] of AnsiChar;
begin
  InitializeFont;
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
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(28));
  glEnableVertexAttribArray(2);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  UniformWVP := glGetUniformLocation(Shader, PGLchar(PAnsiChar('WVP')));
  UniformTex0 := glGetUniformLocation(Shader, PGLchar(PAnsiChar('tex0')));
end;

procedure TForm1.Finalize;
begin
  glDeleteProgram(Shader);
  glDeleteBuffers(1, @VertexBuffer);
  FinalizeFont;
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
begin
  W := TUMat.Scaling(1, TextureSize.y / TextureSize.x, 1);
  W := W * TUMat.RotationY(((GetTickCount64 mod 4000) / 4000) * UTwoPi);
  V := TUMat.View(TUVec3.Make(0, 0.2, -1), TUVec3.Zero, TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(UPi * 0.5, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glClearColor(0.4, 1, 0.8, 1);
  //glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT);// or GL_DEPTH_BUFFER_BIT);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindVertexArray(VertexArray);
  glUseProgram(Shader);
  glUniformMatrix4fv(UniformWVP, 1, GL_TRUE, @WVP);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, FontTexture);
  glUniform1i(UniformTex0, 0);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_SHORT, nil);
end;

end.

