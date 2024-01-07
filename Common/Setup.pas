unit Setup;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
{$if defined(WINDOWS)}
  Windows,
{$elseif defined(LINUX)}
  X, XLib, XUtil,
  gdk2x, gtk2,
{$endif}
  CommonUtils;

type TCommonForm = class(TForm)
  procedure FormActivate(Sender: TObject);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure OnTimer(Sender: TObject);
private
  var Timer: TTimer;
{$if defined(WINDOWS)}
  var RenderContext: HGLRC;
  var DeviceContext: HDC;
{$elseif defined(LINUX)}
  var Display: PDisplay;
  var VisualInfo: PTXVisualInfo;
  var Context: TGLXContext;
  function NativeHandle: TWindow;
{$endif}
{$if defined(WINDOWS)}
  procedure WinInitializeOpenGL;
  procedure WinFinalizeOpenGL;
{$elseif defined(LINUX)}
  procedure LinuxInitializeOpenGL;
  procedure LinuxFinalizeOpenGL;
{$endif}
public
  procedure InitializeOpenGL;
  procedure FinalizeOpenGL;
  procedure PrintInfo;
  procedure Initialize; virtual;
  procedure Finalize; virtual;
  procedure Tick; virtual;
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
end;

implementation

procedure TCommonForm.FormActivate(Sender: TObject);
begin
  if Timer.Enabled then Exit;;
  InitializeOpenGL;
  PrintInfo;
  Initialize;
  Timer.Enabled := True;
end;

procedure TCommonForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled := False;
  Finalize;
  FinalizeOpenGL;
end;

procedure TCommonForm.OnTimer(Sender: TObject);
begin
  Tick;
{$if defined(WINDOWS)}
  SwapBuffers(DeviceContext);
{$elseif defined(LINUX)}
  glXSwapBuffers(Display, NativeHandle);
{$endif}
end;

{$if defined(WINDOWS)}
procedure TCommonForm.WinInitializeOpenGL;
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

procedure TCommonForm.WinFinalizeOpenGL;
begin
  wglDeleteContext(RenderContext);
  ReleaseDC(Handle, DeviceContext);
end;

{$elseif defined(LINUX)}
function TCommonForm.NativeHandle: TWindow;
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

procedure TCommonForm.LinuxInitializeOpenGL;
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

procedure TCommonForm.LinuxFinalizeOpenGL;
begin
  glXDestroyContext(Display, Context);
  XCloseDisplay(Display);
end;
{$endif}

procedure TCommonForm.InitializeOpenGL;
begin
  {$if defined(WINDOWS)}
  WinInitializeOpenGL;
  {$elseif defined(LINUX)}
  LinuxInitializeOpenGL;
  {$endif}
end;

procedure TCommonForm.FinalizeOpenGL;
begin
  {$if defined(WINDOWS)}
  WinFinalizeOpenGL;
  {$elseif defined(LINUX)}
  LinuxFinalizeOpenGL;
  {$endif}
end;

procedure TCommonForm.PrintInfo;
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

procedure TCommonForm.Initialize;
begin
end;

procedure TCommonForm.Finalize;
begin
end;

procedure TCommonForm.Tick;
begin
end;

constructor TCommonForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Timer.OnTimer := @OnTimer;
  Timer.Interval := 1;
end;

destructor TCommonForm.Destroy;
begin
  inherited Destroy;
end;

end.
