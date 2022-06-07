unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  CommonUtils, CommonMedia, Windows;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    var RenderContext: HGLRC;
    var DeviceContext: HDC;
    var VertexArray: TGLuint;
    var VertexBuffer: TGLuint;
    var IndexBuffer: TGLuint;
    var VertexShader: TGLuint;
    var PixelShader: TGLuint;
    var UniformWVP: TGLint;
    var Shader: TGLuint;
    var Texture: TGLuint;
    var UniformTex0: TGLint;
    var TaskLoadTexture: specialize TUTask<TUImageDataShared>;
    procedure Tick;
    procedure InitializeOpenGL;
    procedure FinalizeOpenGL;
    procedure PrintInfo;
    procedure Initialize;
    procedure Finalize;
    procedure ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
    function TFLoadTexture(const Args: array of const): TUImageDataShared;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Timer1Timer(Sender: TObject);
  var TextureFormat, TextureType: TGLenum;
begin
  if TaskLoadTexture.IsStarted and TaskLoadTexture.IsComplete then
  begin
    if TaskLoadTexture.TaskResult.IsValid then
    begin
      ImageFormatToGL(TaskLoadTexture.TaskResult.Ptr.Format, TextureFormat, TextureType);
      glGenTextures(1, @Texture);
      glBindTexture(GL_TEXTURE_2D, Texture);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexImage2D(
        GL_TEXTURE_2D, 0, GL_RGB,
        TaskLoadTexture.TaskResult.Ptr.Width, TaskLoadTexture.TaskResult.Ptr.Height, 0,
        TextureFormat, TextureType, TaskLoadTexture.TaskResult.Ptr.Data
      );
      glGenerateMipmap(GL_TEXTURE_2D);
    end;
    TaskLoadTexture.Reset;
  end;
  Tick;
  SwapBuffers(DeviceContext);
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
begin
  W := TUMat.RotationY(((GetTickCount mod 4000) / 4000) * UTwoPi);
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
  if (Texture > 0) then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, Texture);
    glUniform1i(UniformTex0, 0);
  end;
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_BYTE, nil);
  //glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
end;

procedure TForm1.InitializeOpenGL;
  var pfd: TPixelFormatDescriptor;
  var pf: Integer;
  var pfn: GLuint;
  var FormatAttribs: TwglAttribs;
  var ContextAttribs: TwglAttribs;
begin
  DeviceContext := GetDC(Handle);
  FormatAttribs[WGL_DRAW_TO_WINDOW_ARB] := GL_TRUE;
  FormatAttribs[WGL_SUPPORT_OPENGL_ARB] := GL_TRUE;
  FormatAttribs[WGL_ACCELERATION_ARB] := WGL_FULL_ACCELERATION_ARB;
  FormatAttribs[WGL_COLOR_BITS_ARB] := 24;
  FormatAttribs[WGL_ALPHA_BITS_ARB] := 8;
  FormatAttribs[WGL_DEPTH_BITS_ARB] := 24;
  FormatAttribs[WGL_STENCIL_BITS_ARB] := 8;
  FormatAttribs[WGL_DOUBLE_BUFFER_ARB] := GL_TRUE;
  FormatAttribs[WGL_SAMPLE_BUFFERS_ARB] := GL_TRUE;
  FormatAttribs[WGL_SAMPLES_ARB] := 8;
  pfn := 1; pf := 0;
  if not wglChoosePixelFormatARB(DeviceContext, FormatAttribs.Data, nil, 1, @pf, @pfn) then
  begin
    WriteLn(glGetError);
    pfn := 0;
  end;
  if (pfn = 0) then
  begin
    UClear(pfd, SizeOf(pfd));
    pfd.nSize := SizeOf(pfd);
    pfd.nVersion := 1;
    pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    pfd.iPixelType := PFD_TYPE_RGBA;
    pfd.cColorBits := 32;
    pfd.cAlphaBits := 8;
    pfd.cDepthBits := 16;
    pfd.iLayerType := PFD_MAIN_PLANE;
    pf := ChoosePixelFormat(DeviceContext, @pfd);
  end;
  SetPixelFormat(DeviceContext, pf, @pfd);
  //ContextAttribs[WGL_CONTEXT_MAJOR_VERSION_ARB] := 3;
  //ContextAttribs[WGL_CONTEXT_MINOR_VERSION_ARB] := 3;
  //ContextAttribs[WGL_CONTEXT_FLAGS_ARB] := GL_CONTEXT_FLAG_DEBUG_BIT;
  //ContextAttribs[WGL_CONTEXT_PROFILE_MASK_ARB] := WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
  //ContextAttribs[WGL_CONTEXT_PROFILE_MASK_ARB] := WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
  ContextAttribs[WGL_CONTEXT_PROFILE_MASK_ARB] := WGL_CONTEXT_CORE_PROFILE_BIT_ARB;
  RenderContext := wglCreateContextAttribsARB(DeviceContext, 0, ContextAttribs.Data);
  if RenderContext = 0 then
  begin
    WriteLn(glGetError);
  end;
  wglMakeCurrent(DeviceContext, RenderContext);
end;

procedure TForm1.FinalizeOpenGL;
begin
  wglDeleteContext(RenderContext);
  ReleaseDC(Handle, DeviceContext);
end;

procedure TForm1.PrintInfo;
  var s: String;
  var i, n: TGLint;
begin
  s := PAnsiChar(glGetString(GL_VERSION));
  WriteLn('OpenGL Version: ', s);
  glGetIntegerv(GL_MAJOR_VERSION, @i);
  s := IntToStr(i);
  glGetIntegerv(GL_MINOR_VERSION, @i);
  s += '.' + IntToStr(i);
  WriteLn(s);
  s := PAnsiChar(glGetString(GL_VENDOR));
  WriteLn('Vendor: ', s);
  s := PAnsiChar(glGetString(GL_RENDERER));
  WriteLn('Renderer: ', s);
  glGetIntegerv(GL_NUM_EXTENSIONS, @n);
  for i := 0 to n - 1 do
  begin
    s := PAnsiChar(glGetStringi(GL_EXTENSIONS, i));
    WriteLn(s);
  end;
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
  const Indices: array[0..5] of TGLubyte = (
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
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(0));
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(12));
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(28));
  glEnableVertexAttribArray(2);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
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
  UniformWVP := glGetUniformLocation(Shader, PGLchar(PAnsiChar('WVP')));
  UniformTex0 := glGetUniformLocation(Shader, PGLchar(PAnsiChar('tex0')));
  Texture := 0;
  TaskLoadTexture := TaskLoadTexture.StartTask(@TFLoadTexture, ['../Assets/crate_c.png']);
end;

procedure TForm1.Finalize;
begin
  glDeleteTextures(1, @Texture);
  glDeleteProgram(Shader);
  glDeleteBuffers(1, @VertexBuffer);
end;

procedure TForm1.ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
begin
  case ImageFormat of
    uif_g8: begin Format := GL_LUMINANCE; DataType := GL_UNSIGNED_BYTE; Exit; end;
    uif_g16: begin Format := GL_LUMINANCE; DataType := GL_UNSIGNED_SHORT; Exit; end;
    uif_g8a8: begin Format := GL_LUMINANCE_ALPHA; DataType := GL_UNSIGNED_BYTE; Exit; end;
    uif_g16a16: begin Format := GL_LUMINANCE_ALPHA; DataType := GL_UNSIGNED_SHORT; Exit; end;
    uif_r8g8b8: begin Format := GL_RGB; DataType := GL_UNSIGNED_BYTE; Exit; end;
    uif_r16g16b16: begin Format := GL_RGB; DataType := GL_UNSIGNED_SHORT; Exit; end;
    uif_r8g8b8a8: begin Format := GL_RGBA; DataType := GL_UNSIGNED_BYTE; Exit; end;
    uif_r16g16b16a16: begin Format := GL_RGBA; DataType := GL_UNSIGNED_SHORT; Exit; end;
    uif_r32g32b32_f: begin Format := GL_RGB; DataType := GL_FLOAT; Exit; end;
  end;
  Format := 0;
  DataType := 0;
end;

function TForm1.TFLoadTexture(const Args: array of const): TUImageDataShared;
  var f: String;
begin
  if Length(Args) < 1 then Exit;
  f := AnsiString(Args[0].VAnsiString);
  Result := ULoadImageData(f);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeOpenGL;
  PrintInfo;
  Initialize;
  Timer1.Enabled := True;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := False;
  Finalize;
  FinalizeOpenGL;
end;

end.

