unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  CommonUtils, MediaUtils, Windows;

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
    var IndexCount: Int32;
    var IndexFormat: TGLenum;
    var TaskLoadTexture: specialize TUTask<TGLuint>;
    procedure Tick;
    procedure InitializeOpenGL;
    procedure FinalizeOpenGL;
    procedure PrintInfo;
    procedure Initialize;
    procedure Finalize;
    procedure ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
    function TFLoadTexture(const Args: array of const): TGLuint;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if TaskLoadTexture.IsComplete then
  begin
    Texture := TaskLoadTexture.TaskResult;
    TaskLoadTexture.Reset;
  end;
  Tick;
  SwapBuffers(DeviceContext);
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
begin
  W := TUMat.RotationY(((GetTickCount mod 4000) / 4000) * UTwoPi);
  v := TUMat.View(TUVec3.Make(0, 2, -2), TUVec3.Zero, TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(UPi * 0.4, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.4, 1, 0.8, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glBindVertexArray(VertexArray);
  glUseProgram(Shader);
  glUniformMatrix4fv(UniformWVP, 1, GL_TRUE, @WVP);
  if (Texture > 0) then
  begin
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, Texture);
    glUniform1i(UniformTex0, 0);
  end;
  glDrawElements(GL_TRIANGLES, IndexCount, IndexFormat, nil);
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
  RenderContext := wglCreateContextAttribsARB(DeviceContext, glSharedContext, ContextAttribs.Data);
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
  function AttributeName(const Attribute: TUVertexAttribute): String;
  begin
    case Attribute.Semantic of
      as_position: Result := 'position';
      as_normal: Result := 'normal';
      as_tangent: Result := 'tangent';
      as_binormal: Result := 'binormal';
      as_color: Result := 'color';
      as_texcoord: Result := 'texcoord' + IntToStr(Attribute.SetNumber);
      else Result := '';
    end;
  end;
  var ShaderSource, ShaderInputs, ShaderOutputs, s: String;
  var Ptr: Pointer;
  var i: Integer;
  var Offset: Pointer;
  var ErrorBuffer: array[0..511] of AnsiChar;
  var Scene: TUSceneDataDAE;
  var Mesh: TUSceneData.TMeshSubsetInterface;
begin
  Scene := TUSceneDataDAE.Create;
  try
    Scene.Load('../Assets/box.dae');
    Mesh := Scene.MeshList[0].Subsets[0];
    glGenVertexArrays(1, @VertexArray);
    glGenBuffers(1, @VertexBuffer);
    glGenBuffers(1, @IndexBuffer);
    glBindVertexArray(VertexArray);
    glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer);
    glBufferData(GL_ARRAY_BUFFER, Mesh.VertexCount * Mesh.VertexSize, Mesh.VertexData, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IndexBuffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, Mesh.IndexCount * Mesh.IndexSize, Mesh.IndexData, GL_STATIC_DRAW);
    IndexCount := Mesh.IndexCount;
    case Mesh.IndexSize of
      4: IndexFormat := GL_UNSIGNED_INT;
      else IndexFormat := GL_UNSIGNED_SHORT;
    end;
    Offset := nil;
    for i := 0 to High(Mesh.VertexDescritor) do
    begin
      glVertexAttribPointer(
        i, Mesh.VertexDescritor[i].DataCount, GL_FLOAT, GL_FALSE,
        Mesh.VertexSize, Offset
      );
      glEnableVertexAttribArray(i);
      Offset += Mesh.VertexDescritor[i].Size;
    end;
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    VertexShader := glCreateShader(GL_VERTEX_SHADER);
    ShaderSource := '#version 430 core'#$D#$A;
    ShaderInputs := '';
    ShaderOutputs := '';
    for i := 0 to High(Mesh.VertexDescritor) do
    begin
      s := AttributeName(Mesh.VertexDescritor[i]);
      ShaderInputs += 'layout (location = ' + IntToStr(i) + ') in vec' +
      IntToStr(Mesh.VertexDescritor[i].DataCount) + ' in_' + s + ';'#$D#$A;
      if Mesh.VertexDescritor[i].Semantic <> as_position then
      begin
        ShaderOutputs += 'layout (location = ' + IntToStr(i) + ') out vec' +
        IntToStr(Mesh.VertexDescritor[i].DataCount) + ' out_' + s + ';'#$D#$A;
      end;
    end;
    ShaderSource += ShaderInputs + ShaderOutputs;
    ShaderSource += 'uniform mat4x4 WVP;'#$D#$A;
    ShaderSource += 'void main() {'#$D#$A;
    for i := 0 to High(Mesh.VertexDescritor) do
    begin
      if Mesh.VertexDescritor[i].Semantic = as_position then
      begin
        ShaderSource += '  gl_Position = vec4(in_position, 1.0) * WVP;'#$D#$A;
      end
      else
      begin
        s := AttributeName(Mesh.VertexDescritor[i]);
        ShaderSource += '  out_' + s + ' = in_' + s + ';'#$D#$A;
      end;
    end;
    ShaderSource += '}'#$D#$A;
    //UStrToFile('test_vs.txt', ShaderSource);
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
    ShaderSource := '#version 430 core'#$D#$A;
    for i := 0 to High(Mesh.VertexDescritor) do
    begin
      if Mesh.VertexDescritor[i].Semantic = as_position then Continue;
      s := AttributeName(Mesh.VertexDescritor[i]);
      ShaderSource += 'layout (location = ' + IntToStr(i) + ') in vec' +
      IntToStr(Mesh.VertexDescritor[i].DataCount) + ' in_' + s + ';'#$D#$A;
    end;
    ShaderSource += 'out vec4 out_color;'#$D#$A;
    ShaderSource += 'uniform sampler2D tex0;'#$D#$A;
    ShaderSource += 'void main() {'#$D#$A;
    ShaderSource += '  out_color = texture(tex0, in_texcoord0);'#$D#$A;
    ShaderSource += '}'#$D#$A;
    //UStrToFile('test_ps.txt', ShaderSource);
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
  finally
    FreeAndNil(Scene);
  end;
  Texture := 0;
  TaskLoadTexture := TaskLoadTexture.StartTask(@TFLoadTexture, ['../Assets/crate_c.png']);
end;

procedure TForm1.Finalize;
begin
  glDeleteTextures(1, @Texture);
  glDeleteProgram(Shader);
  glDeleteBuffers(1, @VertexBuffer);
  glDeleteBuffers(1, @IndexBuffer);
  glDeleteVertexArrays(1, @VertexArray);
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

function TForm1.TFLoadTexture(const Args: array of const): TGLuint;
  var f: String;
  var Image: TUImageDataShared;
  var TextureFormat, TextureType: TGLenum;
begin
  if Length(Args) < 1 then Exit(0);
  f := AnsiString(Args[0].VAnsiString);
  Image := ULoadImageData(f);
  if not Image.IsValid then Exit(0);
  wglMakeCurrent(glSharedDC, glSharedContext);
  ImageFormatToGL(Image.Ptr.Format, TextureFormat, TextureType);
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(
    GL_TEXTURE_2D, 0, GL_RGB,
    Image.Ptr.Width, Image.Ptr.Height, 0,
    TextureFormat, TextureType, Image.Ptr.Data
  );
  glGenerateMipmap(GL_TEXTURE_2D);
  glFinish();
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

