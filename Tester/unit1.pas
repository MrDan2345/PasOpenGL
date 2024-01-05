unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  LCLType, LCLIntf, InterfaceBase, StdCtrls,
{$if defined(WINDOWS)}
  Windows,
{$elseif defined(LINUX)}
  X, XLib, XUtil,
  gdk2x, gtk2,
{$endif}
  CommonUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
{$if defined(WINDOWS)}
    var RenderContext: HGLRC;
    var DeviceContext: HDC;
{$elseif defined(LINUX)}
    var Display: PDisplay;
    var VisualInfo: PTXVisualInfo;
    var Context: TGLXContext;
    function NativeHandle: TWindow;
{$endif}
    var VertexArray: TGLuint;
    var VertexBuffer: TGLuint;
    var IndexBuffer: TGLuint;
    var VertexShader: TGLuint;
    var PixelShader: TGLuint;
    var UniformWVP: TGLint;
    var Shader: TGLuint;
{$if defined(WINDOWS)}
    procedure WinInitializeOpenGL;
    procedure WinFinalizeOpenGL;
{$elseif defined(LINUX)}
    procedure LinuxInitializeOpenGL;
    procedure LinuxFinalizeOpenGL;
{$endif}
    procedure InitializeOpenGL;
    procedure FinalizeOpenGL;
    procedure PrintInfo;
    procedure Initialize;
    procedure Finalize;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Timer1Timer(Sender: TObject);
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

{$if defined(WINDOWS)}
  SwapBuffers(DeviceContext);
{$elseif defined(LINUX)}
  glXSwapBuffers(Display, NativeHandle);
{$endif}
end;

{$if defined(WINDOWS)}
procedure TForm1.WinInitializeOpenGL;
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
  FormatAttribs[WGL_SAMPLES_ARB] := 4;
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

procedure TForm1.WinFinalizeOpenGL;
begin
  wglDeleteContext(RenderContext);
  ReleaseDC(Handle, DeviceContext);
end;
{$elseif defined(LINUX)}
function TForm1.NativeHandle: TWindow;
  function AsPtr(const Address: PtrUInt): Pointer;
    var Ptr: Pointer absolute Address;
  begin
    Result := Ptr;
  end;
  var Widget: PGtkWidget;
begin
  Widget := PGtkWidget(AsPtr(Handle));
  if not Assigned(Widget) then Exit(0);
  if not Assigned(Widget^.window) then Exit(0);
  Result := GDK_WINDOW_XWINDOW(Widget^.window);
end;

procedure TForm1.LinuxInitializeOpenGL;
  var VisualAttribs: array of Int32;
begin
  Display := XOpenDisplay(nil);
  VisualAttribs := [
    GLX_RGBA, GLX_DEPTH_SIZE, 24,
    GLX_DOUBLEBUFFER, None
  ];
  VisualInfo := glXChooseVisual(Display, DefaultScreen(Display), @VisualAttribs[0]);
  Context := glXCreateContext(Display, VisualInfo, nil, GL_TRUE);
  glXMakeCurrent(Display, NativeHandle, Context);
end;

procedure TForm1.LinuxFinalizeOpenGL;
begin
  glXDestroyContext(Display, Context);
  XCloseDisplay(Display);
end;
{$endif}

procedure TForm1.InitializeOpenGL;
begin
  {$if defined(WINDOWS)}
  WinInitializeOpenGL;
  {$elseif defined(LINUX)}
  LinuxInitializeOpenGL;
  {$endif}
end;

procedure TForm1.FinalizeOpenGL;
begin
  {$if defined(WINDOWS)}
  WinFinalizeOpenGL;
  {$elseif defined(LINUX)}
  LinuxFinalizeOpenGL;
  {$endif}
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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := False;
  Finalize;
  FinalizeOpenGL;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  if Timer1.Enabled then Exit;;
  InitializeOpenGL;
  PrintInfo;
  Initialize;
  Timer1.Enabled := True;
end;

end.

