unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  CommonUtils, MediaUtils, Windows;

type
  TShader = class(TURefClass)
  private
    var _Handle: TGLuint;
  public
    property Handle: TGLuint read _Handle;
    class function AutoShader(const VertexDescriptor: TUVertexDescriptor): TShader;
    constructor Create(const vs, ps: String);
    destructor Destroy; override;
    procedure Use;
    function UniformLocation(const UniformName: String): TGLint;
  end;
  TShaderShared = specialize TUSharedRef<TShader>;

  TMesh = class (TURefClass)
  public
    type TSubset = class
    public
      var BufferIndex: Int32;
      var VertexOffset: Int32;
      var VertexCount: Int32;
      var IndexOffset: Int32;
      var IndexCount: Int32;
    end;
    type TSubsetList = array of TSubset;
    type TMeshBuffer = record
      VertexDescriptor: TUVertexDescriptor;
      VertexArray: TGLuint;
      VertexBuffer: TGLuint;
      VertexSize: TGLuint;
      VertexCount: TGluint;
      IndexBuffer: TGLuint;
      IndexSize: TGLuint;
      IndexCount: TGLuint;
      IndexFormat: TGLenum;
    end;
    type TMeshBufferList = array of TMeshBuffer;
  private
    var _Subsets: TSubsetList;
    var _Buffers: TMeshBufferList;
  public
    property Subsets: TSubsetList read _Subsets;
    property Buffers: TMeshBufferList read _Buffers;
    constructor Create(const MeshData: TUSceneData.TMeshInterface);
    destructor Destroy; override;
    procedure DrawSubset(const Index: Int32);
  end;
  type TMeshShared = specialize TUSharedRef<TMesh>;

  TTexture = class (TURefClass)
  private
    var _Handle: TGLuint;
  public
    property Handle: TGLuint read _Handle;
    constructor Create(const ImageData: TUSceneData.TImageInterface);
    destructor Destroy; override;
  end;
  type TTextureShared = specialize TUSharedRef<TTexture>;

  TMaterial = class (TURefClass)
  public
    type TMaterialList = array of TMaterial;
  private
    var _Texture: TTextureShared;
  public
    property Texture: TTextureShared read _Texture;
    constructor Create(const MaterialData: TUSceneData.TMaterialInterface);
    destructor Destroy; override;
  end;
  type TMaterialShared = specialize TUSharedRef<TMaterial>;

  type TNode = class (TURefClass)
  public
    type TNodeList = array of TNode;
    type TAttachment = class
    end;
    type TAttachmentMesh = class (TAttachment)
    private
      _Node: TNode;
      var _Mesh: TMesh;
      var _Materials: TMaterial.TMaterialList;
      procedure SetNode(const Value: TNode);
    public
      property Node: TNode read _Node write SetNode;
      property Mesh: TMesh read _Mesh;
      property Materials: TMaterial.TMaterialList read _Materials;
      constructor Create(const AttachData: TUSceneData.TAttachmentMesh);
    end;
    type TAttachmentList = array of TAttachment;
  private
    var _Name: String;
    var _Parent: TNode;
    var _Children: TNodeList;
    var _Attachments: TAttachmentList;
    var _Transform: TUMat;
    procedure ChildAdd(const Child: TNode); inline;
    procedure ChildRemove(const Child: TNode); inline;
    procedure AttachAdd(const Attach: TAttachment);
    procedure AttachRemove(const Attach: TAttachment);
    procedure SetParent(const Value: TNode);
  public
    property Name: String read _Name;
    property Parent: TNode read _Parent write SetParent;
    property Children: TNodeList read _Children;
    property Attachments: TAttachmentList read _Attachments;
    property Transform: TUMat read _Transform write _Transform;
    constructor Create(
      const AParent: TNode;
      const NodeData: TUSceneData.TNodeInterface
    );
    destructor Destroy; override;
  end;
  type TNodeShared = specialize TUSharedRef<TNode>;

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    var RenderContext: HGLRC;
    var DeviceContext: HDC;
    var UniformWVP: TGLint;
    var Shader: TShaderShared;
    var UniformTex0: TGLint;
    var Meshes: array of TMeshShared;
    var Textures: array of TTextureShared;
    var Materials: array of TMaterialShared;
    var RootNode: TNodeShared;
    var TextureRemap: specialize TUMap<Pointer, TTextureShared>;
    var MeshRemap: specialize TUMap<Pointer, TMeshShared>;
    var MaterialRemap: specialize TUMap<Pointer, TMaterialShared>;
    var NodeRemap: specialize TUMap<Pointer, TNode>;
    procedure Tick;
    procedure InitializeOpenGL;
    procedure FinalizeOpenGL;
    procedure PrintInfo;
    procedure Initialize;
    procedure Finalize;
    procedure ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
    procedure Load(const FileName: String);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

class function TShader.AutoShader(const VertexDescriptor: TUVertexDescriptor): TShader;
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
  var vs, ps, Inputs, Outputs, AttName, AttSize: String;
  var i: Int32;
begin
  vs := '#version 430 core'#$D#$A;
  Inputs := '';
  Outputs := '';
  for i := 0 to High(VertexDescriptor) do
  begin
    AttName := AttributeName(VertexDescriptor[i]);
    AttSize := IntToStr(VertexDescriptor[i].DataCount);
    Inputs += 'layout (location = ' + IntToStr(i) + ') in vec' + AttSize + ' in_' + AttName + ';'#$D#$A;
    if VertexDescriptor[i].Semantic <> as_position then
    begin
      Outputs += 'layout (location = ' + IntToStr(i) + ') out vec' + AttSize + ' out_' + AttName + ';'#$D#$A;
    end;
  end;
  vs += Inputs + Outputs;
  vs += 'uniform mat4x4 WVP;'#$D#$A;
  vs += 'void main() {'#$D#$A;
  for i := 0 to High(VertexDescriptor) do
  begin
    if VertexDescriptor[i].Semantic = as_position then
    begin
      vs += '  gl_Position = vec4(in_position, 1.0) * WVP;'#$D#$A;
    end
    else if VertexDescriptor[i].Semantic = as_texcoord then
    begin
      AttName := AttributeName(VertexDescriptor[i]);
      vs += '  out_' + AttName + ' = vec2(in_' + AttName + '.x, ' + '1-in_' + AttName + '.y);'#$D#$A;
    end
    else
    begin
      AttName := AttributeName(VertexDescriptor[i]);
      vs += '  out_' + AttName + ' = in_' + AttName + ';'#$D#$A;
    end;
  end;
  vs += '}'#$D#$A;
  ps := '#version 430 core'#$D#$A;
  for i := 0 to High(VertexDescriptor) do
  begin
    if VertexDescriptor[i].Semantic = as_position then Continue;
    AttName := AttributeName(VertexDescriptor[i]);
    AttSize := IntToStr(VertexDescriptor[i].DataCount);
    ps += 'layout (location = ' + IntToStr(i) + ') in vec' + AttSize + ' in_' + AttName + ';'#$D#$A;
  end;
  ps += 'out vec4 out_color;'#$D#$A;
  ps += 'uniform sampler2D tex0;'#$D#$A;
  ps += 'void main() {'#$D#$A;
  ps += '  out_color = texture(tex0, in_texcoord0.xy);'#$D#$A;
  ps += '}'#$D#$A;
  Result := TShader.Create(vs, ps);
end;

constructor TShader.Create(const vs, ps: String);
  var VertexShader, PixelShader: TGLuint;
  var Ptr: Pointer;
  var i: Int32;
  var ErrorBuffer: array[0..511] of AnsiChar;
begin
  VertexShader := glCreateShader(GL_VERTEX_SHADER);
  Ptr := PAnsiChar(vs);
  glShaderSource(VertexShader, 1, @Ptr, nil);
  glCompileShader(VertexShader);
  glGetShaderiv(VertexShader, GL_COMPILE_STATUS, @i);
  if i = GL_FALSE then
  begin
    glGetShaderInfoLog(VertexShader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  PixelShader := glCreateShader(GL_FRAGMENT_SHADER);
  Ptr := PAnsiChar(ps);
  glShaderSource(PixelShader, 1, @Ptr, nil);
  glCompileShader(PixelShader);
  glGetShaderiv(PixelShader, GL_COMPILE_STATUS, @i);
  if i = GL_FALSE then
  begin
    glGetShaderInfoLog(PixelShader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  _Handle := glCreateProgram();
  glAttachShader(_Handle, VertexShader);
  glAttachShader(_Handle, PixelShader);
  glLinkProgram(_Handle);
  glGetProgramiv(_Handle, GL_LINK_STATUS, @i);
  if i = GL_FALSE then
  begin
    glGetProgramInfoLog(_Handle, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  glDeleteShader(PixelShader);
  glDeleteShader(VertexShader);
end;

destructor TShader.Destroy;
begin
  glDeleteProgram(_Handle);
  inherited Destroy;
end;

procedure TShader.Use;
begin
  glUseProgram(_Handle);
end;

function TShader.UniformLocation(const UniformName: String): TGLint;
begin
  Result := glGetUniformLocation(_Handle, PGLchar(PAnsiChar(UniformName)));
end;

constructor TMesh.Create(const MeshData: TUSceneData.TMeshInterface);
  var cb, IndexCount, VertexCount: Int32;
  procedure FinalizeBuffer;
  begin
    if IndexCount < High(UInt16) then
    begin
      _Buffers[cb].IndexFormat := GL_UNSIGNED_SHORT;
      _Buffers[cb].IndexSize := 2;
    end
    else
    begin
      _Buffers[cb].IndexFormat := GL_UNSIGNED_INT;
      _Buffers[cb].IndexSize := 4;
    end;
    _Buffers[cb].VertexCount := VertexCount;
    _Buffers[cb].IndexCount := IndexCount;
  end;
  var Subset: TSubset;
  var vd: TUVertexDescriptor;
  var AttribOffset, Buffer: Pointer;
  var i, j, s: Int32;
begin
  cb := -1;
  SetLength(_Subsets, Length(MeshData.Subsets));
  for i := 0 to High(MeshData.Subsets) do
  begin
    vd := MeshData.Subsets[i].VertexDescriptor;
    Subset := TSubset.Create;
    _Subsets[i] := Subset;
    if (cb = -1) or (not UCmpVertexDescriptors(_Buffers[cb].VertexDescriptor, vd)) then
    begin
      if cb > -1 then FinalizeBuffer;
      VertexCount := 0;
      IndexCount := 0;
      cb := Length(_Buffers);
      SetLength(_Buffers, Length(_Buffers) + 1);
      _Buffers[cb].VertexDescriptor := vd;
      _Buffers[cb].VertexSize := MeshData.Subsets[i].VertexSize;
    end;
    Subset.BufferIndex := cb;
    Subset.VertexOffset := VertexCount;
    Subset.VertexCount := MeshData.Subsets[i].VertexCount;
    Subset.IndexOffset := IndexCount;
    Subset.IndexCount := MeshData.Subsets[i].IndexCount;
    VertexCount += Subset.VertexCount;
    IndexCount += Subset.IndexCount;
  end;
  FinalizeBuffer;
  for i := 0 to High(_Buffers) do
  begin
    glGenVertexArrays(1, @_Buffers[i].VertexArray);
    glGenBuffers(1, @_Buffers[i].VertexBuffer);
    glGenBuffers(1, @_Buffers[i].IndexBuffer);
    glBindVertexArray(_Buffers[i].VertexArray);
    glBindBuffer(GL_ARRAY_BUFFER, _Buffers[i].VertexBuffer);
    glBufferData(GL_ARRAY_BUFFER, _Buffers[i].VertexCount * _Buffers[i].VertexSize, nil, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _Buffers[i].IndexBuffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, _Buffers[i].IndexCount * _Buffers[i].IndexSize, nil, GL_STATIC_DRAW);
    Buffer := glMapBuffer(GL_ELEMENT_ARRAY_BUFFER, GL_WRITE_ONLY);
    vd := _Buffers[i].VertexDescriptor;
    AttribOffset := nil;
    for j := 0 to High(vd) do
    begin
      glVertexAttribPointer(
        j, vd[i].DataCount, GL_FLOAT, GL_FALSE,
        _Buffers[i].VertexSize, AttribOffset
      );
      glEnableVertexAttribArray(j);
      AttribOffset += vd[i].Size;
    end;
    for s := 0 to High(_Subsets) do
    if _Subsets[s].BufferIndex = i then
    begin
      glBufferSubData(
        GL_ARRAY_BUFFER,
        _Subsets[s].VertexOffset * _Buffers[i].VertexSize,
        MeshData.Subsets[s].VertexBufferSize,
        MeshData.Subsets[s].VertexData
      );
      if _Buffers[i].IndexFormat = GL_UNSIGNED_INT then
      for j := 0 to MeshData.Subsets[s].IndexCount - 1 do
      begin
        PUInt32Arr(Buffer)^[_Subsets[s].IndexOffset + j] := (
          _Subsets[s].VertexOffset + MeshData.Subsets[s].Index[j]
        );
      end
      else
      for j := 0 to MeshData.Subsets[s].IndexCount - 1 do
      begin
        PUInt16Arr(Buffer)^[_Subsets[s].IndexOffset + j] := (
          _Subsets[s].VertexOffset + MeshData.Subsets[s].Index[j]
        );
      end;
    end;
    glUnmapBuffer(GL_ELEMENT_ARRAY_BUFFER);
  end;
  glBindVertexArray(0);
end;

destructor TMesh.Destroy;
  var i: Int32;
begin
  specialize UArrClear<TSubset>(_Subsets);
  for i := 0 to High(_Buffers) do
  begin
    glDeleteBuffers(1, @_Buffers[i].VertexBuffer);
    glDeleteBuffers(1, @_Buffers[i].IndexBuffer);
    glDeleteVertexArrays(1, @_Buffers[i].VertexArray);
  end;
  inherited Destroy;
end;

procedure TMesh.DrawSubset(const Index: Int32);
  var Subset: TSubset;
  var IndexOffset: PtrUInt;
begin
  Subset := _Subsets[Index];
  IndexOffset := Subset.IndexOffset * _Buffers[Subset.BufferIndex].IndexSize;
  glDrawRangeElements(
    GL_TRIANGLES,
    Subset.VertexOffset,
    Subset.VertexOffset + Subset.VertexCount - 1,
    Subset.IndexCount,
    _Buffers[Subset.BufferIndex].IndexFormat,
    UIntToPtr(IndexOffset)
  );
end;

constructor TTexture.Create(const ImageData: TUSceneData.TImageInterface);
  var Image: TUImageDataShared;
  var TextureFormat, TextureType: TGLenum;
begin
  Image := ULoadImageData('../Assets/siren/' + ImageData.FileName);
  if not Image.IsValid then
  begin
    _Handle := 0;
    Exit;
  end;
  Form1.ImageFormatToGL(Image.Ptr.Format, TextureFormat, TextureType);
  glGenTextures(1, @_Handle);
  glBindTexture(GL_TEXTURE_2D, _Handle);
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
  glBindTexture(GL_TEXTURE_2D, 0);
end;

destructor TTexture.Destroy;
begin
  if _Handle > 0 then glDeleteTextures(1, @_Handle);
  inherited Destroy;
end;

constructor TMaterial.Create(const MaterialData: TUSceneData.TMaterialInterface);
  var i, j: Int32;
  var Image: TUSceneData.TMaterialInterface.TParamImage;
begin
  for i := 0 to High(MaterialData.Params) do
  if MaterialData.Params[i] is TUSceneData.TMaterialInterface.TParamImage then
  begin
    Image := TUSceneData.TMaterialInterface.TParamImage(MaterialData.Params[i]);
    j := Form1.TextureRemap.FindIndexByKey(Image.Image);
    if j > -1 then
    begin
      _Texture := Form1.TextureRemap[j];
      Break;
    end;
  end;
end;

destructor TMaterial.Destroy;
begin
  inherited Destroy;
end;

procedure TNode.TAttachmentMesh.SetNode(const Value: TNode);
begin
  if _Node = Value then Exit;
  if Assigned(_Node) then _Node.AttachRemove(Self);
  _Node := Value;
  if Assigned(_Node) then _Node.AttachAdd(Self);
end;

constructor TNode.TAttachmentMesh.Create(
  const AttachData: TUSceneData.TAttachmentMesh
);
  var i: Int32;
begin
  _Mesh := Form1.MeshRemap.FindValueByKey(AttachData.Mesh).Ptr;
  SetLength(_Materials, Length(AttachData.MaterialBindings));
  for i := 0 to High(_Materials) do
  begin
    if Assigned(AttachData.MaterialBindings[i].BaseMaterial) then
    begin
      _Materials[i] := Form1.MaterialRemap.FindValueByKey(
        AttachData.MaterialBindings[i].BaseMaterial
      ).Ptr;
    end
    else
    begin
      _Materials[i] := nil;
    end;
  end;
end;

procedure TNode.ChildAdd(const Child: TNode);
begin
  specialize UArrAppend<TNode>(_Children, Child);
end;

procedure TNode.ChildRemove(const Child: TNode);
begin
  specialize UArrRemove<TNode>(_Children, Child);
end;

procedure TNode.AttachAdd(const Attach: TAttachment);
begin
  specialize UArrAppend<TAttachment>(_Attachments, Attach);
end;

procedure TNode.AttachRemove(const Attach: TAttachment);
begin
  specialize UArrRemove<TAttachment>(_Attachments, Attach);
end;

procedure TNode.SetParent(const Value: TNode);
begin
  if _Parent = Value then Exit;
  if Assigned(_Parent) then _Parent.ChildRemove(Self);
  _Parent := Value;
  if Assigned(_Parent) then _Parent.ChildAdd(Self);
end;

constructor TNode.Create(
  const AParent: TNode;
  const NodeData: TUSceneData.TNodeInterface
);
  var i: Int32;
  var AttachMesh: TAttachmentMesh;
begin
  _Name := NodeData.Name;
  Parent := AParent;
  _Transform := NodeData.Transform;
  for i := 0 to High(NodeData.Attachments) do
  begin
    if NodeData.Attachments[i] is TUSceneData.TAttachmentMesh then
    begin
      AttachMesh := TAttachmentMesh.Create(
        TUSceneData.TAttachmentMesh(NodeData.Attachments[i])
      );
      AttachMesh.Node := Self;
    end;
  end;
  Form1.NodeRemap.Add(NodeData, Self);
  for i := 0 to High(NodeData.Children) do
  begin
    TNode.Create(Self, NodeData.Children[i]);
  end;
end;

destructor TNode.Destroy;
begin
  specialize UArrClear<TNode>(_Children);
  inherited Destroy;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Tick;
  SwapBuffers(DeviceContext);
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
  procedure DrawNode(const Node: TNode);
    var Attach: TNode.TAttachment;
    var MeshAttach: TNode.TAttachmentMesh;
    var CurBuffer, NewBuffer, CurTexture, NewTexture: TGLuint;
    var Subset: TMesh.TSubset;
    var Xf: TUMat;
    var i: Int32;
  begin
    for Attach in Node.Attachments do
    if Attach is TNode.TAttachmentMesh then
    begin
      MeshAttach := TNode.TAttachmentMesh(Attach);
      Xf := MeshAttach.Node.Transform;
      WVP := Xf * W * V * P;
      glUniformMatrix4fv(UniformWVP, 1, GL_TRUE, @WVP);
      CurBuffer := 0;
      CurTexture := 0;
      for i := 0 to High(MeshAttach.Mesh.Subsets) do
      begin
        Subset := MeshAttach.Mesh.Subsets[i];
        NewBuffer := MeshAttach.Mesh.Buffers[Subset.BufferIndex].VertexArray;
        if NewBuffer <> CurBuffer then
        begin
          CurBuffer := NewBuffer;
          glBindVertexArray(CurBuffer);
        end;
        NewTexture := MeshAttach.Materials[i].Texture.Ptr.Handle;
        if NewTexture <> CurTexture then
        begin
          CurTexture := NewTexture;
          glActiveTexture(GL_TEXTURE0);
          glBindTexture(GL_TEXTURE_2D, CurTexture);
          glUniform1i(UniformTex0, 0);
        end;
        MeshAttach.Mesh.DrawSubset(i);
      end;
    end;
    for i := 0 to High(Node.Children) do
    begin
      DrawNode(Node.Children[i]);
    end;
  end;
begin
  W := TUMat.RotationY(((GetTickCount mod 4000) / 4000) * UTwoPi);
  v := TUMat.View(TUVec3.Make(0, 1.5, -2), TUVec3.Make(0, 1, 0), TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(UPi * 0.3, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.4, 1, 0.8, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  Shader.Ptr.Use;
  if RootNode.IsValid then
  begin
    DrawNode(RootNode.Ptr);
  end;
  glBindVertexArray(0);
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
begin
  Load('../Assets/siren/siren.dae');
end;

procedure TForm1.Finalize;
begin
  Meshes := nil;
  Shader := nil;
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

procedure TForm1.Load(const FileName: String);
  var i: Integer;
  var Scene: TUSceneDataDAE;
begin
  Scene := TUSceneDataDAE.Create;
  try
    Scene.Load(FileName);
    SetLength(Textures, Length(Scene.ImageList));
    for i := 0 to High(Textures) do
    begin
      Textures[i] := TTexture.Create(Scene.ImageList[i]);
      TextureRemap.Add(Scene.ImageList[i], Textures[i]);
    end;
    SetLength(Meshes, Length(Scene.MeshList));
    for i := 0 to High(Meshes) do
    begin
      Meshes[i] := TMesh.Create(Scene.MeshList[i]);
      MeshRemap.Add(Scene.MeshList[i], Meshes[i]);
    end;
    SetLength(Materials, Length(Scene.MaterialList));
    for i := 0 to High(Materials) do
    begin
      Materials[i] := TMaterial.Create(Scene.MaterialList[i]);
      MaterialRemap.Add(Scene.MaterialList[i], Materials[i]);
    end;
    RootNode := TNode.Create(nil, Scene.RootNode);
    Shader := TShader.AutoShader(Meshes[0].Ptr.Buffers[0].VertexDescriptor);
    UniformWVP := Shader.Ptr.UniformLocation('WVP');
    UniformTex0 := Shader.Ptr.UniformLocation('tex0');
  finally
    FreeAndNil(Scene);
  end;
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

