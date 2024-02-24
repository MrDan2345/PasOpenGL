program BasicTester;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$if defined(WINDOWS)}
  Windows,
  {$elseif defined(LINUX)}
  X,
  XLib,
  XUtil,
  {$endif}
  Classes,
  CommonUtils,
  SysUtils,
  PasOpenGL;

{$R *.res}

type TVertex = packed record
  Pos: TUVec3;
  Color: TUVec4;
end;

var VB: GLUint;
var IB: GLUint;


procedure Initialize;
  const Vertices: array[0..3] of TVertex = (
    (Pos: (-2, 2, 0); Color: (1, 0, 0, 1)),
    (Pos: (2, 2, 0); Color: (0, 1, 0, 1)),
    (Pos: (-2, -2, 0); Color: (0, 0, 1, 1)),
    (Pos: (2, -2, 0); Color: (1, 1, 0, 1))
  );
  const Indices: array[0..5] of Word = (
    0, 1, 2, 2, 1, 3
  );
begin
  glGenBuffers(1, @VB);
  glBindBuffer(GL_ARRAY_BUFFER, VB);
  glBufferData(GL_ARRAY_BUFFER, Sizeof(TVertex) * 4, @Vertices, GL_STATIC_DRAW);
  glGenBuffers(1, @IB);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IB);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(Word) * 6, @Indices, GL_STATIC_DRAW);
  //glClearColor(0.5, 0.5, 0.5, 1);
  //glClearDepth(1);
  //glEnable(GL_TEXTURE_2D);
  //glShadeModel(GL_SMOOTH);
  //glDisable(GL_CULL_FACE);
  //glEnable(GL_BLEND);
end;

procedure Finalize;
begin
  glDeleteBuffers(1, @VB);
  glDeleteBuffers(1, @IB);
end;

procedure OnUpdate;
begin

end;

procedure OnRender;
  var W, V, P, WV: TUMat;
  var t: TUFloat;
begin
  t := 10000;
  t := (GetTickCount64 mod Round(t)) * (1 / Round(t));
  W := TUMat.RotationY(t * UTwoPi);
  V := TUMat.View(TUVec3.Make(0, 5, -5), TUVec3.Make(0, 0, 0), TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(Pi / 4, 1, 1, 100);
  WV := W * V;
  glMatrixMode(GL_MODELVIEW);
  glLoadMatrixf(@WV);
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(@P);
  glClear(GL_COLOR_BUFFER_BIT);

  glBindBuffer(GL_ARRAY_BUFFER, VB);
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertex), Pointer(0));
  glEnableClientState(GL_COLOR_ARRAY);
  glColorPointer(4, GL_FLOAT, SizeOf(TVertex), Pointer(12));

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IB);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_SHORT, Pointer(0));
end;

{$if defined(WINDOWS)}
var WindowHandle: HWND;
var AppRunning: Boolean;
var Context: HGLRC;
var DC: HDC;
function MessageHandler(Wnd: HWnd; Msg: UInt; wParam: WPARAM; lParam: LPARAM): LResult; stdcall;
begin
  case Msg of
    WM_DESTROY, WM_QUIT, WM_CLOSE:
    begin
      PostQuitMessage(0);
      Result := 0;
      Exit;
    end;
    WM_KEYDOWN:
    begin
      if wParam = VK_ESCAPE then AppRunning := False;
    end;
    WM_KEYUP:
    begin

    end;
    WM_LBUTTONDOWN:
    begin

    end;
    WM_LBUTTONUP:
    begin

    end;
    WM_RBUTTONDOWN:
    begin

    end;
    WM_RBUTTONUP:
    begin

    end;
    WM_MBUTTONDOWN:
    begin

    end;
    WM_MBUTTONUP:
    begin

    end;
  end;
  Result := DefWindowProcA(Wnd, Msg, wParam, lParam);
end;

procedure CreateWindow(const W, H: Integer; const Caption: AnsiString = 'PureOGL');
  var WndClass: TWndClassExA;
  var WndClassName: AnsiString;
  var R: TRect;
  var WndStyle: DWord;
begin
  WndClassName := 'PureOGL';
  UClear(WndClass, SizeOf(TWndClassExA));
  WndClass.cbSize := SizeOf(TWndClassExA);
  WndClass.hIconSm := LoadIcon(MainInstance, 'MAINICON');
  WndClass.hIcon := LoadIcon(MainInstance, 'MAINICON');
  WndClass.hInstance := HInstance;
  WndClass.hCursor := LoadCursor(0, IDC_ARROW);
  WndClass.lpszClassName := PAnsiChar(WndClassName);
  WndClass.style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC or CS_DBLCLKS;
  WndClass.lpfnWndProc := @MessageHandler;
  if RegisterClassExA(WndClass) = 0 then
  WndClassName := 'Static';
  WndStyle := (
    WS_CAPTION or
    WS_POPUP or
    WS_VISIBLE or
    WS_EX_TOPMOST or
    WS_MINIMIZEBOX or
    WS_SYSMENU
  );
  R.Left := (GetSystemMetrics(SM_CXSCREEN) - W) div 2;
  R.Right := R.Left + W;
  R.Top := (GetSystemMetrics(SM_CYSCREEN) - H) div 2;
  R.Bottom := R.Top + H;
  AdjustWindowRect(R, WndStyle, False);
  WindowHandle := CreateWindowExA(
    0, PAnsiChar(WndClassName), PAnsiChar(Caption),
    WndStyle,
    R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
    0, 0, HInstance, nil
  );
end;

procedure FreeWindow;
begin
  DestroyWindow(WindowHandle);
end;

procedure CreateGL;
  var pfd: TPixelFormatDescriptor;
  var pf: Integer;
  var R: TRect;
begin
  DC := GetDC(WindowHandle);
  UClear(pfd, SizeOf(pfd));
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 24;
  pfd.cAlphaBits := 8;
  pfd.cDepthBits := 16;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pf := ChoosePixelFormat(DC, @pfd);
  SetPixelFormat(DC, pf, @pfd);
  Context := wglCreateContext(DC);
  wglMakeCurrent(DC, Context);
  GetClientRect(WindowHandle, @R);
  glViewport(0, 0, R.Right - R.Left, R.Bottom - R.Top);
end;

procedure FreeGL;
begin
  wglMakeCurrent(DC, Context);
  wglDeleteContext(Context);
  ReleaseDC(WindowHandle, DC);
end;

procedure Loop;
  var msg: TMsg;
begin
  AppRunning := True;
  UClear(msg, SizeOf(msg));
  while AppRunning
  and (msg.message <> WM_QUIT)
  and (msg.message <> WM_DESTROY)
  and (msg.message <> WM_CLOSE) do
  begin
    if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end
    else
    begin
      OnUpdate;
      OnRender;
      SwapBuffers(DC);
    end
  end;
  ExitCode := 0;
end;
{$elseif defined(LINUX)}
var Display: PDisplay;
var Screen: Int32;
var WindowHandle: TWindow;
var VisualInfo: PTXVisualInfo;
var FBConfig: TGLXFBConfig;
var Context: TGLXContext;
const _NET_WM_STATE_REMOVE = 0;
const _NET_WM_STATE_ADD = 1;
const _NET_WM_STATE_TOGGLE = 2;
procedure CreateWindow(const W, H: Integer; const Caption: AnsiString = 'PureOGL');
  procedure Maximize;
    var Event: TXEvent;
  begin
    Event := Default(TXEvent);
    Event._type := ClientMessage;
    Event.xclient.window := WindowHandle;
    Event.xclient.message_type := XInternAtom(Display, '_NET_WM_STATE', False);
    Event.xclient.format := 32;
    Event.xclient.data.l[0] := _NET_WM_STATE_ADD;
    Event.xclient.data.l[1] := XInternAtom(Display, '_NET_WM_STATE_MAXIMIZED_HORZ', False);
    Event.xclient.data.l[2] := XInternAtom(Display, '_NET_WM_STATE_MAXIMIZED_VERT', False);
    XSendEvent(Display, DefaultRootWindow(Display), False, SubstructureNotifyMask, @Event);
  end;
  procedure Fullscreen;
    var Event: TXEvent;
  begin
    Event := Default(TXEvent);
    Event._type := ClientMessage;
    Event.xclient.window := WindowHandle;
    Event.xclient.message_type := XInternAtom(Display, '_NET_WM_STATE', False);
    Event.xclient.format := 32;
    Event.xclient.data.l[0] := _NET_WM_STATE_ADD;
    Event.xclient.data.l[1] := XInternAtom(Display, '_NET_WM_STATE_FULLSCREEN', False);
    XSendEvent(Display, DefaultRootWindow(Display), False, SubstructureNotifyMask, @Event);
  end;
  procedure Windowed;
    var PropAtom: array of TAtom;
    var Res: Int32;
    const XA_ATOM = 4;
  begin
    PropAtom := [
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_DESKTOP', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_DOCK', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_TOOLBAR', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_MENU', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_UTILITY', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_SPLASH', False)
      XInternAtom(Display, '_NET_WM_WINDOW_TYPE_DIALOG', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_DROPDOWN_MENU', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_POPUP_MENU', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_TOOLTIP', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_NOTIFICATION', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_COMBO', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_DND', False)
      //XInternAtom(Display, '_NET_WM_WINDOW_TYPE_NORMAL', False)
    ];
    XChangeProperty(
      Display, WindowHandle, XInternAtom(Display, '_NET_WM_WINDOW_TYPE', False),
      XA_ATOM, 32, PropModeReplace, @PropAtom[0], Length(PropAtom)
    );
  end;
  var VisualAttribs: array of Int32;
  var ColorMap: TColormap;
  var WindowAttribsInit: TXSetWindowAttributes;
  var Attribs: TGLAttribs;
  var Config: PGLXFBConfig;
  var ConfigCount: Int32;
begin
  Display := XOpenDisplay(nil);
  if not Assigned(Display) then
  begin
    WriteLn('Cannot open display');
    Exit;
  end;
  Screen := DefaultScreen(Display);
  Attribs[GLX_X_RENDERABLE] := GL_TRUE;
  Attribs[GLX_DRAWABLE_TYPE] := GLX_WINDOW_BIT;
  Attribs[GLX_RENDER_TYPE] := GLX_RGBA_BIT;
  Attribs[GLX_X_VISUAL_TYPE] := GLX_TRUE_COLOR;
  Attribs[GLX_RED_SIZE] := 8;
  Attribs[GLX_GREEN_SIZE] := 8;
  Attribs[GLX_BLUE_SIZE] := 8;
  Attribs[GLX_ALPHA_SIZE] := 8;
  Attribs[GLX_DEPTH_SIZE] := 24;
  Attribs[GLX_STENCIL_SIZE] := 8;
  Attribs[GLX_DOUBLEBUFFER] := GL_TRUE;
  Attribs[GLX_SAMPLE_BUFFERS] := 1;
  Attribs[GLX_SAMPLES] := 8;
  Config := glXChooseFBConfig(Display, Screen, Attribs.Data, @ConfigCount);
  if Assigned(Config) then
  begin
    //iterate over ConfigCount to pick the most appropriate config
    FBConfig := Config^;
    VisualInfo := glXGetVisualFromFBConfig(Display, FBConfig);
    {
    it is possible to acquire a compatible visual info from glXChooseVisual
    every component size must match the visual from FB config.
    it may be useful when creating context in dependent of window
    VisualAttribs := [
      GLX_RGBA,
      GLX_RED_SIZE, 8,
      GLX_GREEN_SIZE, 8,
      GLX_BLUE_SIZE, 8,
      GLX_ALPHA_SIZE, 8,
      GLX_DEPTH_SIZE, 24,
      GLX_STENCIL_SIZE, 8,
      GLX_DOUBLEBUFFER,
      None
    ];
    VisualInfo := glXChooseVisual(Display, Screen, @VisualAttribs[0]);
    }
    XFree(Config);
  end
  else
  begin
    WriteLn('No appropriate frame buffer configs detected!');
    VisualAttribs := [
      GLX_RGBA, GLX_DEPTH_SIZE, 24,
      GLX_DOUBLEBUFFER, None
    ];
    VisualInfo := glXChooseVisual(Display, Screen, @VisualAttribs[0]);
  end;
  ColorMap := XCreateColormap(Display, DefaultRootWindow(Display), VisualInfo^.visual, AllocNone);
  WindowAttribsInit := Default(TXSetWindowAttributes);
  WindowAttribsInit.colormap := ColorMap;
  WindowAttribsInit.event_mask := ExposureMask or KeyPressMask;
  WindowHandle := XCreateWindow(
    Display, RootWindow(Display, Screen), 0, 0, W, H, 1,
    VisualInfo^.depth, InputOutput, VisualInfo^.visual,
    CWColormap or CWEventMask, @WindowAttribsInit
  );
  XStoreName(Display, WindowHandle, 'Window Title');
  XSelectInput(Display, WindowHandle, ExposureMask or KeyPressMask);
  Windowed;
  XMapWindow(Display, WindowHandle);
  //Maximize;
  //Fullscreen;
end;

procedure FreeWindow;
begin
  XFree(VisualInfo);
  XDestroyWindow(Display, WindowHandle);
  XCloseDisplay(Display);
end;

procedure CreateGL;
  var ContextAttribs: TGLAttribs;
begin
  if Assigned(FBConfig) then
  begin
    ContextAttribs[GLX_CONTEXT_MAJOR_VERSION_ARB] := 3;
    ContextAttribs[GLX_CONTEXT_MINOR_VERSION_ARB] := 0;
    //ContextAttribs[GLX_CONTEXT_FLAGS_ARB] := GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
    Context := glXCreateContextAttribsARB(Display, FBConfig, nil, GL_TRUE, ContextAttribs.Data);
  end
  else
  begin
    Context := glXCreateContext(Display, VisualInfo, nil, GL_TRUE);
  end;
  glXMakeCurrent(Display, WindowHandle, Context);
end;

procedure FreeGL;
begin
  glXDestroyContext(Display, Context);
end;

procedure Loop;
  var Event: TXEvent;
  var WindowAttribs: TXWindowAttributes;
  var Running: Boolean;
begin
  Running := True;
  while Running do
  begin
    if (XCheckMaskEvent(Display, ExposureMask or KeyPressMask, @Event)) then
    begin
      case Event._type of
        Expose:
        begin
          XGetWindowAttributes(Display, WindowHandle, @WindowAttribs);
          glViewport(0, 0, WindowAttribs.width, WindowAttribs.height);
        end;
        KeyPress:
        begin
          Running := False;
        end;
      end;
    end
    else
    begin
      OnUpdate;
      OnRender;
      glXSwapBuffers(Display, WindowHandle);
    end;
  end;
end;
{$endif}

begin
  CreateWindow(800, 600);
  CreateGL;
  Initialize;
  Loop;
  Finalize;
  FreeGL;
  FreeWindow;
end.


