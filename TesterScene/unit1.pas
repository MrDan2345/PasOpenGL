unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  CommonUtils, MediaUtils, Setup;

type TGLuintArray = array of TGLuint;

type TShader = class(TURefClass)
public
  type TSkinInfo = record
    BoneWeights: Int32;
  end;
  type PSkinInfo = ^TSkinInfo;
private
  var _Handle: TGLuint;
  var _UniformWVP: TGLint;
  var _UniformTex0: TGLint;
  var _UniformBone: TGLint;
  class var _ShaderMap: specialize TUMap<UInt64, TShader>;
public
  property Handle: TGLuint read _Handle;
  property UniformWVP: TGLint read _UniformWVP;
  property UniformTex0: TGLint read _UniformTex0;
  property UniformBone: TGLint read _UniformBone;
  class function AutoShader(const VertexDescriptor: TUVertexDescriptor; const SkinInfo: PSkinInfo = nil): TShader;
  constructor Create(const vs, ps: String);
  destructor Destroy; override;
  procedure Use;
  function UniformLocation(const UniformName: String): TGLint;
end;
type TShaderShared = specialize TUSharedRef<TShader>;
type TShaderList = array of TShaderShared;

type TMesh = class (TURefClass)
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

type TSkin = class (TURefClass)
public
  type TSubset = class
  public
    var BufferIndex: Int32;
    var VertexOffset: Int32;
  end;
  type TSubsetList = array of TSubset;
  type TSkinBuffer = record
    VertexBuffer: TGLuint;
    VertexCount: TGluint;
    WeightCount: Int32;
  end;
  type TSkinBufferList = array of TSkinBuffer;
  type TJoint = record
    Node: String;
    Bind: TUMat;
  end;
  type TJointList = array of TJoint;
private
  var _Mesh: TMesh;
  var _Subsets: TSubsetList;
  var _Buffers: TSkinBufferList;
  var _Joints: TJointList;
  var _Bind: TUMat;
public
  property Mesh: TMesh read _Mesh;
  property Subsets: TSubsetList read _Subsets;
  property Buffers: TSkinBufferList read _Buffers;
  property Joints: TJointList read _Joints;
  property Bind: TUMat read _Bind;
  constructor Create(const SkinData: TUSceneData.TSkinInterface);
  destructor Destroy; override;
end;
type TSkinShared = specialize TUSharedRef<TSkin>;

type TTexture = class (TURefClass)
private
  var _Handle: TGLuint;
public
  property Handle: TGLuint read _Handle;
  constructor Create(const ImageData: TUSceneData.TImageInterface);
  destructor Destroy; override;
end;
type TTextureShared = specialize TUSharedRef<TTexture>;

type TMaterial = class (TURefClass)
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
  private
    var _Node: TNode;
    procedure SetNode(const Value: TNode);
  public
    property Node: TNode read _Node write SetNode;
  end;
  type TAttachmentMesh = class (TAttachment)
  private
    var _Mesh: TMesh;
    var _Materials: TMaterial.TMaterialList;
    var _Shaders: TShaderList;
    var _VertexArrays: TGLuintArray;
  public
    property Mesh: TMesh read _Mesh;
    property Materials: TMaterial.TMaterialList read _Materials;
    property Shaders: TShaderList read _Shaders;
    property VertexArrays: TGLuintArray read _VertexArrays;
    constructor Create(const AttachData: TUSceneData.TAttachmentMesh);
    destructor Destroy; override;
  end;
  type TAttachmentSkin = class (TAttachment)
  public
    type TJointBinding = record
      var Bind: TUMat;
      var Node: TNode;
    end;
    type TJointBindingList = array of TJointBinding;
  private
    var _Skin: TSkin;
    var _Materials: TMaterial.TMaterialList;
    var _Shaders: TShaderList;
    var _VertexArrays: TGLuintArray;
    var _Pose: TUMatArray;
    var _JointBindings: TNodeList;
  public
    property Skin: TSkin read _Skin;
    property Materials: TMaterial.TMaterialList read _Materials;
    property Shaders: TShaderList read _Shaders;
    property VertexArrays: TGLuintArray read _VertexArrays;
    property Pose: TUMatArray read _Pose;
    constructor Create(const AttachData: TUSceneData.TAttachmentSkin);
    destructor Destroy; override;
    procedure UpdatePose;
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

type TForm1 = class(TCommonForm)
private
  var Meshes: array of TMeshShared;
  var Skins: array of TSkinShared;
  var Textures: array of TTextureShared;
  var Materials: array of TMaterialShared;
  var RootNode: TNodeShared;
  var TextureRemap: specialize TUMap<Pointer, TTextureShared>;
  var MeshRemap: specialize TUMap<Pointer, TMeshShared>;
  var SkinRemap: specialize TUMap<Pointer, TSkinShared>;
  var MaterialRemap: specialize TUMap<Pointer, TMaterialShared>;
  var NodeRemap: specialize TUMap<Pointer, TNode>;
  procedure ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
  procedure Load(const FileName: String);
public
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
end;

var Form1: TForm1;

implementation

{$R *.lfm}

class function TShader.AutoShader(const VertexDescriptor: TUVertexDescriptor; const SkinInfo: PSkinInfo): TShader;
  function MakeHash: UInt64;
    var n: UInt32;
  begin
    Result := 0;
    n := Length(VertexDescriptor);
    Result := UCRC64(Result, @n, SizeOf(n));
    Result := UCRC64(Result, @VertexDescriptor[0], n * SizeOf(VertexDescriptor));
    if Assigned(SkinInfo) then
    begin
      Result := UCRC64(Result, SkinInfo, SizeOf(SkinInfo^));
    end;
  end;
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
  var Hash: UInt64;
  var Attrib: TUVertexAttribute;
  var vs, ps, Inputs, Outputs, AttName, AttSize: String;
  var i, AttribIndex: Int32;
begin
  Hash := MakeHash;
  Result := _ShaderMap.FindValueByKey(Hash);
  if Assigned(Result) then Exit;
  vs := '#version 430 core'#$D#$A;
  Inputs := '';
  Outputs := '';
  AttribIndex := 0;
  for Attrib in VertexDescriptor do
  begin
    AttName := AttributeName(Attrib);
    AttSize := IntToStr(Attrib.DataCount);
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in vec' + AttSize + ' in_' + AttName + ';'#$D#$A;
    if Attrib.Semantic <> as_position then
    begin
      Outputs += 'layout (location = ' + IntToStr(AttribIndex) + ') out vec' + AttSize + ' out_' + AttName + ';'#$D#$A;
    end;
    Inc(AttribIndex);
  end;
  if Assigned(SkinInfo) then
  begin
    AttSize := IntToStr(SkinInfo^.BoneWeights);
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in uvec' + AttSize + ' in_bone_index;'#$D#$A;
    Inc(AttribIndex);
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in vec' + AttSize + ' in_bone_weight;'#$D#$A;
    Inc(AttribIndex);
  end;
  vs += Inputs + Outputs;
  vs += 'uniform mat4x4 WVP;'#$D#$A;
  if Assigned(SkinInfo) then
  begin
    vs += 'uniform mat4x4 Bone[200];'#$D#$A;
  end;
  vs += 'void main() {'#$D#$A;
  if Assigned(SkinInfo) then
  begin
    vs += '  mat4x4 S = ';
    for i := 0 to SkinInfo^.BoneWeights - 1 do
    begin
      vs += 'bone[in_bone_index[' + IntToStr(i) + ']] * in_bone_weight[' + IntToStr(i) + ']';
      if i < SkinInfo^.BoneWeights - 1 then vs += ' + ' else vs += ';'#$D#$A;
    end;
  end;
  for i := 0 to High(VertexDescriptor) do
  begin
    if VertexDescriptor[i].Semantic = as_position then
    begin
      if Assigned(SkinInfo) then
      begin
        vs += '  vec4 position = vec4((vec4(in_position, 1.0) * S).xyz, 1.0);'#$D#$A;
      end
      else
      begin
        vs += '  vec4 position = vec4(in_position, 1.0);'#$D#$A;
      end;
      vs += '  gl_Position = position * WVP;'#$D#$A;
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
  UStrToFile('vs' + IntToStr(Hash) + '.txt', vs);
  UStrToFile('ps' + IntToStr(Hash) + '.txt', ps);
  Result := TShader.Create(vs, ps);
  _ShaderMap.Add(Hash, Result);
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
  _UniformWVP := UniformLocation('WVP');
  _UniformTex0 := UniformLocation('tex0');
  _UniformBone := UniformLocation('Bone');
end;

destructor TShader.Destroy;
begin
  _ShaderMap.RemoveByValue(Self);
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
  function FindOrCreateBuffer(const VertexDescriptor: TUVertexDescriptor): Int32;
    var i: Int32;
  begin
    for i := 0 to High(_Buffers) do
    if UCmpVertexDescriptors(_Buffers[i].VertexDescriptor, VertexDescriptor) then
    begin
      Exit(i);
    end;
    Result := Length(_Buffers);
    SetLength(_Buffers, Result + 1);
    _Buffers[Result].VertexDescriptor := VertexDescriptor;
    _Buffers[Result].VertexSize := UComputeVertexSize(VertexDescriptor);
    _Buffers[Result].VertexCount := 0;
    _Buffers[Result].IndexCount := 0;
  end;
  var Subset: TSubset;
  var SubsetId, BufferId, i: Int32;
  var IndexBuffer: Pointer;
  var IndexBuffer16: PUInt16Arr absolute IndexBuffer;
  var IndexBuffer32: PUInt32Arr absolute IndexBuffer;
begin
  SetLength(_Subsets, Length(MeshData.Subsets));
  for SubsetId := 0 to High(_Subsets) do
  begin
    Subset := TSubset.Create;
    _Subsets[SubsetId] := Subset;
    BufferId := FindOrCreateBuffer(MeshData.Subsets[SubsetId].VertexDescriptor);
    Subset.BufferIndex := BufferId;
    Subset.VertexCount := MeshData.Subsets[SubsetId].VertexCount;
    Subset.IndexCount := MeshData.Subsets[SubsetId].IndexCount;
    Subset.VertexOffset := _Buffers[BufferId].VertexCount;
    Subset.IndexOffset := _Buffers[BufferId].IndexCount;
    _Buffers[BufferId].VertexCount += Subset.VertexCount;
    _Buffers[BufferId].IndexCount += Subset.IndexCount;
  end;
  for BufferId := 0 to High(_Buffers) do
  begin
    if _Buffers[BufferId].IndexCount < High(UInt16) then
    begin
      _Buffers[BufferId].IndexFormat := GL_UNSIGNED_SHORT;
      _Buffers[BufferId].IndexSize := 2;
    end
    else
    begin
      _Buffers[BufferId].IndexFormat := GL_UNSIGNED_INT;
      _Buffers[BufferId].IndexSize := 4;
    end;
    glGenBuffers(1, @_Buffers[BufferId].VertexBuffer);
    glGenBuffers(1, @_Buffers[BufferId].IndexBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _Buffers[BufferId].VertexBuffer);
    glBufferData(GL_ARRAY_BUFFER, _Buffers[BufferId].VertexCount * _Buffers[BufferId].VertexSize, nil, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _Buffers[BufferId].IndexBuffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, _Buffers[BufferId].IndexCount * _Buffers[BufferId].IndexSize, nil, GL_STATIC_DRAW);
    IndexBuffer := glMapBuffer(GL_ELEMENT_ARRAY_BUFFER, GL_WRITE_ONLY);
    for SubsetId := 0 to High(_Subsets) do
    if _Subsets[SubsetId].BufferIndex = BufferId then
    begin
      Subset := _Subsets[SubsetId];
      glBufferSubData(
        GL_ARRAY_BUFFER, Subset.VertexOffset,
        Subset.VertexCount * _Buffers[BufferId].VertexSize,
        MeshData.Subsets[SubsetId].VertexData
      );
      if _Buffers[BufferId].IndexFormat = GL_UNSIGNED_INT then
      for i := 0 to Subset.IndexCount - 1 do
      begin
        IndexBuffer32^[Subset.IndexOffset + i] := (
          Subset.VertexOffset + MeshData.Subsets[SubsetId].Index[i]
        );
      end
      else
      for i := 0 to Subset.IndexCount - 1 do
      begin
        IndexBuffer16^[Subset.IndexOffset + i] := (
          Subset.VertexOffset + MeshData.Subsets[SubsetId].Index[i]
        );
      end;
    end;
    glUnmapBuffer(GL_ELEMENT_ARRAY_BUFFER);
  end;
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

destructor TMesh.Destroy;
  var i: Int32;
begin
  specialize UArrClear<TSubset>(_Subsets);
  for i := 0 to High(_Buffers) do
  begin
    glDeleteBuffers(1, @_Buffers[i].VertexBuffer);
    glDeleteBuffers(1, @_Buffers[i].IndexBuffer);
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

constructor TSkin.Create(const SkinData: TUSceneData.TSkinInterface);
  function FindOrCreateBuffer(const WeightCount: Int32): Int32;
    var i: Int32;
  begin
    for i := 0 to High(_Buffers) do
    if _Buffers[i].WeightCount = WeightCount then
    begin
      Exit(i);
    end;
    i := Length(_Buffers);
    SetLength(_Buffers, i + 1);
    _Buffers[i].WeightCount := WeightCount;
    _Buffers[i].VertexCount := 0;
    glCreateBuffers(1, @_Buffers[i].VertexBuffer);
    Result := i;
  end;
  var SubsetId, BufferId, JointId: Int32;
  var Buffer: Pointer;
  const WeightSize = SizeOf(UInt32) + SizeOf(TUFloat);
begin
  _Mesh := Form1.MeshRemap.FindValueByKey(SkinData.Mesh).Ptr;
  SetLength(_Subsets, Length(_Mesh.Subsets));
  for SubsetId := 0 to High(_Subsets) do
  begin
    _Subsets[SubsetId] := TSubset.Create;
    BufferId := FindOrCreateBuffer(SkinData.Subsets[SubsetId].WeightCount);
    _Subsets[SubsetId].BufferIndex := BufferId;
    _Subsets[SubsetId].VertexOffset := _Buffers[BufferId].VertexCount;
    _Buffers[BufferId].VertexCount += _Mesh.Subsets[SubsetId].VertexCount;
  end;
  for BufferId := 0 to High(_Buffers) do
  begin
    glBindBuffer(GL_ARRAY_BUFFER, _Buffers[BufferId].VertexBuffer);
    glBufferData(GL_ARRAY_BUFFER, _Buffers[BufferId].VertexCount * _Buffers[BufferId].WeightCount * WeightSize, nil, GL_STATIC_DRAW);
    Buffer := glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    for SubsetId := 0 to High(_Subsets) do
    if _Subsets[SubsetId].BufferIndex = BufferId then
    begin
      Move(
        SkinData.Subsets[SubsetId].VertexData^,
        (Buffer + _Subsets[SubsetId].VertexOffset * WeightSize)^,
        _Mesh.Subsets[SubsetId].VertexCount * _Buffers[BufferId].WeightCount * WeightSize
      );
    end;
    glUnmapBuffer(GL_ARRAY_BUFFER);
  end;
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  _Bind := SkinData.ShapeBind;
  SetLength(_Joints, Length(SkinData.Joints));
  for JointId := 0 to High(_Joints) do
  begin
    _Joints[JointId].Node := SkinData.Joints[JointId].Name;
    _Joints[JointId].Bind := SkinData.Joints[JointId].Bind;
  end;
end;

destructor TSkin.Destroy;
  var i: Int32;
begin
  specialize UArrClear<TSubset>(_Subsets);
  for i := 0 to High(_Buffers) do
  begin
    glDeleteBuffers(1, @_Buffers[i].VertexBuffer);
  end;
  inherited Destroy;
end;

constructor TTexture.Create(const ImageData: TUSceneData.TImageInterface);
  var Image: TUImageDataShared;
  var TextureFormat, TextureType: TGLenum;
begin
  //Image := ULoadImageData('../Assets/siren/' + ImageData.FileName);
  Image := ULoadImageData('../Assets/' + ImageData.FileName);
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

procedure TNode.TAttachment.SetNode(const Value: TNode);
begin
  if _Node = Value then Exit;
  if Assigned(_Node) then _Node.AttachRemove(Self);
  _Node := Value;
  if Assigned(_Node) then _Node.AttachAdd(Self);
end;

constructor TNode.TAttachmentMesh.Create(
  const AttachData: TUSceneData.TAttachmentMesh
);
  var SubsetId, BufferId, i: Int32;
  var Subset: TMesh.TSubset;
  var AttribOffset: Pointer;
  var vd: TUVertexDescriptor;
begin
  inherited Create;
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
  SetLength(_VertexArrays, Length(_Mesh.Subsets));
  glGenVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  for SubsetId := 0 to High(_Mesh.Subsets) do
  begin
    Subset := _Mesh.Subsets[SubsetId];
    BufferId := Subset.BufferIndex;
    vd := _Mesh.Buffers[BufferId].VertexDescriptor;
    specialize UArrAppend<TShaderShared>(
      _Shaders, TShader.AutoShader(vd)
    );
    glBindVertexArray(_VertexArrays[SubsetId]);
    glBindBuffer(GL_ARRAY_BUFFER, _Mesh.Buffers[BufferId].VertexBuffer);
    AttribOffset := nil;
    for i := 0 to High(vd) do
    begin
      glVertexAttribPointer(
        i, vd[i].DataCount, GL_FLOAT, GL_FALSE,
        _Mesh.Buffers[BufferId].VertexSize, AttribOffset
      );
      glEnableVertexAttribArray(i);
      AttribOffset += vd[i].Size;
    end;
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _Mesh.Buffers[BufferId].IndexBuffer);
  end;
  glBindVertexArray(0);
end;

destructor TNode.TAttachmentMesh.Destroy;
begin
  glDeleteVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  inherited Destroy;
end;

constructor TNode.TAttachmentSkin.Create(const AttachData: TUSceneData.TAttachmentSkin);
  var SubsetId, SkinBufferId, MeshBufferId, i: Int32;
  var SkinInfo: TShader.TSkinInfo;
  var AttribOffset: Pointer;
  var vd: TUVertexDescriptor;
  const WeightSize = SizeOf(UInt32) + SizeOf(TUFloat);
begin
  inherited Create;
  _Skin := Form1.SkinRemap.FindValueByKey(AttachData.Skin).Ptr;
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
  SetLength(_VertexArrays, Length(_Skin.Subsets));
  glGenVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  for SubsetId := 0 to High(_Skin.Subsets) do
  begin
    SkinBufferId := _Skin.Subsets[SubsetId].BufferIndex;
    MeshBufferId := _Skin.Mesh.Subsets[SubsetId].BufferIndex;
    vd := _Skin.Mesh.Buffers[MeshBufferId].VertexDescriptor;
    SkinInfo.BoneWeights := _Skin.Buffers[SkinBufferId].WeightCount;
    specialize UArrAppend<TShaderShared>(
      _Shaders, TShader.AutoShader(vd, @SkinInfo)
    );
    glBindVertexArray(_VertexArrays[SubsetId]);
    glBindBuffer(GL_ARRAY_BUFFER, _Skin.Mesh.Buffers[MeshBufferId].VertexBuffer);
    AttribOffset := nil;
    for i := 0 to High(vd) do
    begin
      glVertexAttribPointer(
        i, vd[i].DataCount, GL_FLOAT, GL_FALSE,
        _Skin.Mesh.Buffers[MeshBufferId].VertexSize, AttribOffset
      );
      glEnableVertexAttribArray(i);
      AttribOffset += vd[i].Size;
    end;
    glBindBuffer(GL_VERTEX_ARRAY, _Skin.Buffers[SkinBufferId].VertexBuffer);
    i := Length(vd);
    glVertexAttribPointer(
      i, SkinInfo.BoneWeights, GL_INT, GL_FALSE,
      SkinInfo.BoneWeights * WeightSize, AttribOffset
    );
    glEnableVertexAttribArray(i);
    Inc(i);
    AttribOffset += SkinInfo.BoneWeights * SizeOf(UInt32);
    glVertexAttribPointer(
      i, SkinInfo.BoneWeights, GL_FLOAT, GL_FALSE,
      SkinInfo.BoneWeights * WeightSize, AttribOffset
    );
    glEnableVertexAttribArray(i);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _Skin.Mesh.Buffers[MeshBufferId].IndexBuffer);
  end;
  glBindVertexArray(0);
  SetLength(_Pose, Length(_Skin.Joints));
  SetLength(_JointBindings, Length(_Skin.Joints));
  for i := 0 to High(_Pose) do
  begin
    _JointBindings[i] := Form1.NodeRemap.FindValueByKey(AttachData.JointBindings[i]);
  end;
  UpdatePose;
end;

destructor TNode.TAttachmentSkin.Destroy;
begin
  glDeleteVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  inherited Destroy;
end;

procedure TNode.TAttachmentSkin.UpdatePose;
  var i: Int32;
begin
  for i := 0 to High(_Pose) do
  begin
    _Pose[i] := _Skin.Bind * _Skin.Joints[i].Bind * _JointBindings[i].Transform;
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
  var AttachSkin: TAttachmentSkin;
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
    end
    else if NodeData.Attachments[i] is TUSceneData.TAttachmentSkin then
    begin
      AttachSkin := TAttachmentSkin.Create(
        TUSceneData.TAttachmentSkin(NodeData.Attachments[i])
      );
      AttachSkin.Node := Self;
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

procedure TForm1.ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
begin
  case ImageFormat of
    uif_g8: begin Format := GL_LUMINANCE; DataType := GL_UNSIGNED_BYTE; end;
    uif_g16: begin Format := GL_LUMINANCE; DataType := GL_UNSIGNED_SHORT; end;
    uif_g8a8: begin Format := GL_LUMINANCE_ALPHA; DataType := GL_UNSIGNED_BYTE; end;
    uif_g16a16: begin Format := GL_LUMINANCE_ALPHA; DataType := GL_UNSIGNED_SHORT; end;
    uif_r8g8b8: begin Format := GL_RGB; DataType := GL_UNSIGNED_BYTE; end;
    uif_r16g16b16: begin Format := GL_RGB; DataType := GL_UNSIGNED_SHORT; end;
    uif_r8g8b8a8: begin Format := GL_RGBA; DataType := GL_UNSIGNED_BYTE; end;
    uif_r16g16b16a16: begin Format := GL_RGBA; DataType := GL_UNSIGNED_SHORT; end;
    uif_r32g32b32_f: begin Format := GL_RGB; DataType := GL_FLOAT; end;
    else begin Format := 0; DataType := 0; end;
  end;
end;

procedure TForm1.Load(const FileName: String);
  var i: Integer;
  var Scene: TUSceneDataDAE;
begin
  Scene := TUSceneDataDAE.Create([]);
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
    SetLength(Skins, Length(Scene.SkinList));
    for i := 0 to High(Skins) do
    begin
      Skins[i] := TSkin.Create(Scene.SkinList[i]);
      SkinRemap.Add(Scene.SkinList[i], Skins[i]);
    end;
    SetLength(Materials, Length(Scene.MaterialList));
    for i := 0 to High(Materials) do
    begin
      Materials[i] := TMaterial.Create(Scene.MaterialList[i]);
      MaterialRemap.Add(Scene.MaterialList[i], Materials[i]);
    end;
    RootNode := TNode.Create(nil, Scene.RootNode);
  finally
    FreeAndNil(Scene);
  end;
end;

procedure TForm1.Initialize;
begin
  //Load('../Assets/siren/siren_anim.dae');
  Load('../Assets/skin.dae');
end;

procedure TForm1.Finalize;
begin
  TextureRemap.Clear;
  MaterialRemap.Clear;
  SkinRemap.Clear;
  MeshRemap.Clear;
  NodeRemap.Clear;
  RootNode := nil;
  Skins := nil;
  Meshes := nil;
  Textures := nil;
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
  var CurShader: TShader;
  procedure DrawNode(const Node: TNode);
    var Attach: TNode.TAttachment;
    var AttachMesh: TNode.TAttachmentMesh;
    var AttachSkin: TNode.TAttachmentSkin;
    var CurBuffer, NewBuffer, CurTexture, NewTexture: TGLuint;
    var NewShader: TShader;
    var Xf: TUMat;
    var i: Int32;
  begin
    for Attach in Node.Attachments do
    if Attach is TNode.TAttachmentMesh then
    begin
      AttachMesh := TNode.TAttachmentMesh(Attach);
      Xf := AttachMesh.Node.Transform;
      WVP := Xf * W * V * P;
      CurBuffer := 0;
      CurTexture := 0;
      for i := 0 to High(AttachMesh.Mesh.Subsets) do
      begin
        NewShader := AttachMesh.Shaders[i].Ptr;
        if NewShader <> CurShader then
        begin
          CurShader := NewShader;
          CurShader.Use;
          CurTexture := 0;
        end;
        NewBuffer := AttachMesh.VertexArrays[i];
        if NewBuffer <> CurBuffer then
        begin
          CurBuffer := NewBuffer;
          glBindVertexArray(CurBuffer);
        end;
        NewTexture := AttachMesh.Materials[i].Texture.Ptr.Handle;
        if NewTexture <> CurTexture then
        begin
          CurTexture := NewTexture;
          glActiveTexture(GL_TEXTURE0);
          glBindTexture(GL_TEXTURE_2D, CurTexture);
          glUniform1i(CurShader.UniformTex0, 0);
        end;
        glUniformMatrix4fv(CurShader.UniformWVP, 1, GL_TRUE, @WVP);
        AttachMesh.Mesh.DrawSubset(i);
      end;
    end
    else if Attach is TNode.TAttachmentSkin then
    begin
      AttachSkin := TNode.TAttachmentSkin(Attach);
      Xf := AttachSkin.Node.Transform;
      WVP := Xf * W * V * P;
      CurBuffer := 0;
      CurTexture := 0;
      for i := 0 to High(AttachSkin.Skin.Subsets) do
      begin
        NewShader := AttachSkin.Shaders[i].Ptr;
        if NewShader <> CurShader then
        begin
          CurShader := NewShader;
          CurShader.Use;
          CurTexture := 0;
        end;
        NewBuffer := AttachSkin.VertexArrays[i];
        if NewBuffer <> CurBuffer then
        begin
          CurBuffer := NewBuffer;
          glBindVertexArray(CurBuffer);
        end;
        NewTexture := AttachSkin.Materials[i].Texture.Ptr.Handle;
        if NewTexture <> CurTexture then
        begin
          CurTexture := NewTexture;
          glActiveTexture(GL_TEXTURE0);
          glBindTexture(GL_TEXTURE_2D, CurTexture);
          glUniform1i(CurShader.UniformTex0, 0);
        end;
        glUniformMatrix4fv(CurShader.UniformWVP, 1, GL_TRUE, @WVP);
        glUniformMatrix4fv(CurShader.UniformBone, Length(AttachSkin.Pose), GL_TRUE, @AttachSkin.Pose[0]);
        AttachSkin.Skin.Mesh.DrawSubset(i);
      end;
    end;
    for i := 0 to High(Node.Children) do
    begin
      DrawNode(Node.Children[i]);
    end;
  end;
begin
  W := TUMat.RotationZ(((GetTickCount64 mod 4000) / 4000) * UTwoPi);
  v := TUMat.View(TUVec3.Make(10, 10, 10), TUVec3.Make(0, 0, 5), TUVec3.Make(0, 0, 1));
  P := TUMat.Proj(UPi * 0.3, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.4, 1, 0.8, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  CurShader := nil;
  //Shader.Ptr.Use;
  if RootNode.IsValid then
  begin
    DrawNode(RootNode.Ptr);
  end;
  glBindVertexArray(0);
end;

end.

