use super::aux::report::{report, Report};
use std::collections::HashMap;
use std::fmt;

type Byte = u8;
type Bytes = Vec<Byte>;

pub trait Encode {
  fn encode(&self) -> Bytes;
}

macro_rules! encode_section {
  ($prefix:expr, $members:expr) => {{
    let mut bytes: Bytes = vec![$prefix];
    let mut contents = $members.encode();
    bytes.append(&mut contents.len().encode());
    bytes.append(&mut contents);
    bytes
  }};
}

macro_rules! encode {
    ($first:expr $(, $rest:expr)*) => {{
        let mut bytes: Bytes = $first.encode();
        $( bytes.append(&mut $rest.encode()); )+
        bytes
    }};
}

macro_rules! encode_with_prefix {
  ($prefix:expr) => {{
    vec![$prefix]
  }};
  ($prefix:expr $(, $arg:expr)+) => {{
    let mut bytes: Bytes = vec![$prefix];
    $( bytes.append(&mut $arg.encode()); )+
    bytes
  }};
}

pub struct Module {
  pub type_section: TypeSection,
  pub import_section: ImportSection,
  pub func_section: FuncSection,
  pub export_section: ExportSection,
  pub start_section: Option<FuncIndex>,
  pub code_section: CodeSection,
}

impl Module {
  pub fn new() -> Self {
    Self {
      type_section: TypeSection::new(),
      import_section: ImportSection::new(),
      func_section: FuncSection::new(),
      export_section: ExportSection::new(),
      start_section: None,
      code_section: CodeSection::new(),
    }
  }

  pub fn add_func(&mut self, sig: Type, code: Code) -> FuncIndex {
    let type_index = self.type_section.push(sig);
    let func_index = self.func_section.push(type_index);
    self.code_section.push(code);
    func_index
  }
}

impl Default for Module {
  fn default() -> Self {
    Self::new()
  }
}

impl Encode for Module {
  fn encode(&self) -> Bytes {
    let mut bytes: Bytes = vec![
      0x00, b'a', b's', b'm', // magic cookie
      0x01, 0x00, 0x00, 0x00, // version number
    ];

    bytes.append(&mut self.type_section.encode());
    bytes.append(&mut self.import_section.encode());
    bytes.append(&mut self.func_section.encode());

    if let Some(start_section) = &self.start_section {
      bytes.append(&mut encode_section!(0x08, start_section));
    }

    bytes.append(&mut self.code_section.encode());
    bytes
  }
}

impl From<&Module> for Report {
  fn from(module: &Module) -> Self {
    let ty_iter = module.type_section.types.iter().enumerate();
    let im_iter = module.import_section.0.iter();
    let fn_iter = module
      .func_section
      .0
      .iter()
      .zip(module.code_section.0.iter())
      .enumerate();

    report! {
      paren_left
      write("module")
      increment_indent
      for_each(ty_iter, { |(idx, ty)|
        report! {
          newline
          indent
          paren_left
          write("type")
          space
          write(format!("(;{};)", idx))
          space
          then_from(ty)
          paren_right
        }
      })
      for_each(im_iter, { |im|
        report! {
          newline
          indent
          paren_left
          write("import")
          space
          write(format!("\"{}\"", im.module))
          space
          write(format!("\"{}\"", im.name))
          space
          then_from(&im.desc)
          paren_right
        }
      })
      for_each(fn_iter, { |(func_idx, (type_idx, code))|
        report! {
          newline
          indent
          paren_left
          write(format!("func (;{};) (type {})", func_idx, type_idx))
          increment_indent
          for_each(code.locals.iter(), {|local| report! {
            newline
            indent
            then_from(local)
          }})
          for_each(code
          .insts
          .iter()
          .filter(|i| !matches!(i, Inst::BlockEnd)), {|inst| report! {
            newline
            indent
            then_from(inst)
          }})
          decrement_indent
          paren_right
        }
      })
      if_some(&module.start_section, {|start| report! {
        newline
        indent
        paren_left
        write("start")
        space
        write(format!("{}", start))
        paren_right
      }})
      decrement_indent
      paren_right
    }
  }
}

pub struct Import {
  pub module: String,
  pub name: String,
  pub desc: ImportDesc,
}

impl Encode for Import {
  fn encode(&self) -> Bytes {
    let mut bytes = self.module.as_str().encode();
    bytes.append(&mut self.name.as_str().encode());
    bytes.append(&mut self.desc.encode());
    bytes
  }
}

pub enum ImportDesc {
  TypeIndex(TypeIndex),
}

impl Encode for ImportDesc {
  fn encode(&self) -> Bytes {
    match self {
      Self::TypeIndex(idx) => encode_with_prefix!(0x0, idx),
    }
  }
}

impl From<&ImportDesc> for Report {
  fn from(desc: &ImportDesc) -> Self {
    match desc {
      ImportDesc::TypeIndex(idx) => report!(write(format!("(func {})", idx))),
    }
  }
}

pub struct ImportSection(LengthPrefixedVec<Import>);

impl ImportSection {
  fn new() -> Self {
    Self(LengthPrefixedVec::new())
  }

  pub fn push<S1, S2>(&mut self, module: S1, name: S2, desc: ImportDesc)
  where
    S1: ToString,
    S2: ToString,
  {
    self.0.push(Import {
      module: module.to_string(),
      name: name.to_string(),
      desc,
    });
  }
}

impl Encode for ImportSection {
  fn encode(&self) -> Bytes {
    encode_section!(0x02, self.0)
  }
}

#[derive(Debug, Copy, Clone)]
pub struct TypeIndex(usize);

impl Encode for TypeIndex {
  fn encode(&self) -> Bytes {
    self.0.encode()
  }
}

impl fmt::Display for TypeIndex {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

pub struct TypeSection {
  types: LengthPrefixedVec<Type>,
  cache: HashMap<Type, TypeIndex>,
}

impl TypeSection {
  fn new() -> Self {
    Self {
      types: LengthPrefixedVec::new(),
      cache: HashMap::new(),
    }
  }

  pub fn push(&mut self, ty: Type) -> TypeIndex {
    if let Some(cached_id) = self.cache.get(&ty) {
      *cached_id
    } else {
      let id = TypeIndex(self.types.len());
      self.cache.insert(ty.clone(), id);
      self.types.push(ty);
      id
    }
  }
}

impl Encode for TypeSection {
  fn encode(&self) -> Bytes {
    encode_section!(0x01, self.types)
  }
}

#[derive(Debug)]
pub struct FuncIndex(usize);

impl FuncIndex {
  pub fn new(idx: usize) -> Self {
    Self(idx)
  }
}

impl Encode for FuncIndex {
  fn encode(&self) -> Bytes {
    self.0.encode()
  }
}

impl fmt::Display for FuncIndex {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

const TOTAL_INTRINSICS: usize = 0;

pub struct FuncSection(LengthPrefixedVec<TypeIndex>);

impl FuncSection {
  fn new() -> Self {
    Self(LengthPrefixedVec::new())
  }

  fn push(&mut self, index: TypeIndex) -> FuncIndex {
    let func_index = FuncIndex(self.0.len() + TOTAL_INTRINSICS);
    self.0.push(index);
    func_index
  }
}

impl Encode for FuncSection {
  fn encode(&self) -> Bytes {
    encode_section!(0x03, self.0)
  }
}

pub enum ExportDesc {
  FuncIndex(FuncIndex),
}

impl Encode for ExportDesc {
  fn encode(&self) -> Bytes {
    match self {
      Self::FuncIndex(idx) => encode_with_prefix!(0x00, idx),
    }
  }
}

pub struct Export {
  pub name: String,
  pub desc: ExportDesc,
}

impl Encode for Export {
  fn encode(&self) -> Bytes {
    encode!(self.name.as_str(), self.desc)
  }
}

pub struct ExportSection(LengthPrefixedVec<Export>);

impl ExportSection {
  fn new() -> Self {
    Self(LengthPrefixedVec::new())
  }
}

pub struct CodeSection(LengthPrefixedVec<Code>);

impl CodeSection {
  fn new() -> Self {
    Self(LengthPrefixedVec::new())
  }

  fn push(&mut self, code: Code) {
    self.0.push(code);
  }
}

impl Encode for CodeSection {
  fn encode(&self) -> Bytes {
    encode_section!(0x0a, self.0)
  }
}

pub struct Local(i32, Type);

impl Encode for Local {
  fn encode(&self) -> Bytes {
    let mut bytes: Bytes = vec![];
    bytes.append(&mut self.0.encode());
    bytes.append(&mut self.1.encode());
    bytes
  }
}

impl From<&Local> for Report {
  fn from(local: &Local) -> Self {
    report! {
      paren_left
      write("local")
      space
      then_from(&local.1)
      paren_right
    }
  }
}

#[derive(Debug, Copy, Clone)]
pub struct LocalIndex(usize);

impl Encode for LocalIndex {
  fn encode(&self) -> Bytes {
    self.0.encode()
  }
}

impl fmt::Display for LocalIndex {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Debug)]
pub enum Inst {
  Nop,
  BlockEnd,
  Call(FuncIndex),

  Drop,

  I32Const(i32),
  I32Add,
  I32Mul,

  LocalGet(LocalIndex),
  LocalSet(LocalIndex),
}

impl Encode for Inst {
  fn encode(&self) -> Bytes {
    match self {
      Inst::Nop => encode_with_prefix!(0x01),
      Inst::BlockEnd => encode_with_prefix!(0x0b),
      Inst::Call(idx) => encode_with_prefix!(0x10, idx),

      Inst::Drop => encode_with_prefix!(0x1a),

      Inst::I32Const(val) => encode_with_prefix!(0x41, val),
      Inst::I32Add => encode_with_prefix!(0x6a),
      Inst::I32Mul => encode_with_prefix!(0x6c),

      Inst::LocalGet(idx) => encode_with_prefix!(0x20, idx),
      Inst::LocalSet(idx) => encode_with_prefix!(0x21, idx),
    }
  }
}

impl From<&Inst> for Report {
  fn from(inst: &Inst) -> Self {
    match inst {
      Inst::Nop => report!(write("nop")),
      Inst::BlockEnd => report!(write("end")),
      Inst::Call(idx) => report!(write(format!("call {}", idx))),
      Inst::Drop => report!(write("drop")),
      Inst::I32Const(val) => report!(write(format!("i32.const {}", val))),
      Inst::I32Add => report!(write("i32.add")),
      Inst::I32Mul => report!(write("i32.mul")),
      Inst::LocalGet(idx) => report!(write(format!("local.get {}", idx))),
      Inst::LocalSet(idx) => report!(write(format!("local.set {}", idx))),
    }
  }
}

pub struct Code {
  pub locals: LengthPrefixedVec<Local>,
  pub insts: Vec<Inst>,
}

impl Code {
  pub fn new() -> Self {
    Self {
      locals: LengthPrefixedVec::new(),
      insts: vec![],
    }
  }

  pub fn push_local(&mut self, ty: Type) -> LocalIndex {
    let index = LocalIndex(self.locals.len());
    self.locals.push(Local(1, ty));
    index
  }

  pub fn push_inst(&mut self, inst: Inst) -> &mut Self {
    self.insts.push(inst);
    self
  }
}

impl Default for Code {
  fn default() -> Self {
    Self::new()
  }
}

impl Encode for Code {
  fn encode(&self) -> Bytes {
    let mut contents: Bytes = vec![];
    contents.append(&mut self.locals.encode());
    for inst in &self.insts {
      contents.append(&mut inst.encode());
    }

    let mut bytes: Bytes = vec![];
    bytes.append(&mut contents.len().encode());
    bytes.append(&mut contents);
    bytes
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
  I32,
  I64,
  F32,
  F64,
  Func(LengthPrefixedVec<Type>, LengthPrefixedVec<Type>),
}

impl Encode for Type {
  fn encode(&self) -> Bytes {
    match self {
      Type::I32 => encode_with_prefix!(0x7f),
      Type::I64 => encode_with_prefix!(0x7e),
      Type::F32 => encode_with_prefix!(0x7d),
      Type::F64 => encode_with_prefix!(0x7c),
      Type::Func(params, returns) => encode_with_prefix!(0x60, params, returns),
    }
  }
}

impl From<&Type> for Report {
  fn from(ty: &Type) -> Self {
    match ty {
      Type::I32 => report!(write("i32")),
      Type::I64 => report!(write("i64")),
      Type::F32 => report!(write("f32")),
      Type::F64 => report!(write("f64")),
      Type::Func(params, rets) => report! {
        paren_left
        write("func")
        for_each(params.iter(), |param| report! {
          space
          paren_left
          write("param")
          space
          then_from(param)
          paren_right
        })
        for_each(rets.iter(), |ret| report! {
          space
          paren_left
          write("result")
          space
          then_from(ret)
          paren_right
        })
        paren_right
      },
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LengthPrefixedVec<T: Encode>(Vec<T>);

impl<T: Encode> LengthPrefixedVec<T> {
  pub fn new() -> Self {
    Self(vec![])
  }

  pub fn push(&mut self, value: T) {
    self.0.push(value);
  }
}

impl<T> From<Vec<T>> for LengthPrefixedVec<T>
where
  T: Encode,
{
  fn from(vector: Vec<T>) -> Self {
    Self(vector)
  }
}

impl<T: Encode> Default for LengthPrefixedVec<T> {
  fn default() -> Self {
    Self::new()
  }
}

impl<T: Encode> IntoIterator for LengthPrefixedVec<T> {
  type Item = T;
  type IntoIter = std::vec::IntoIter<Self::Item>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<T: Encode> std::ops::Deref for LengthPrefixedVec<T> {
  type Target = Vec<T>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T: Encode> std::ops::DerefMut for LengthPrefixedVec<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<T: Encode> Encode for LengthPrefixedVec<T> {
  fn encode(&self) -> Bytes {
    let mut bytes: Bytes = vec![];
    bytes.append(&mut self.0.len().encode());
    for elem in &self.0 {
      bytes.append(&mut elem.encode());
    }
    bytes
  }
}

macro_rules! encode_using_signed_leb128 {
  ($i:ident) => {
    impl Encode for $i {
      fn encode(&self) -> Bytes {
        let mut val = self.clone();
        let mut bs: Bytes = vec![];
        loop {
          let b = (val & 0x7f) as Byte;
          val >>= 7;
          if (val == 0 && (b & 0x40) == 0) || (val == -1 && (b & 0x40) != 0) {
            bs.push(b);
            return bs;
          }
          bs.push(b | 0x80);
        }
      }
    }
  };
}

encode_using_signed_leb128!(i8);
encode_using_signed_leb128!(i16);
encode_using_signed_leb128!(i32);
encode_using_signed_leb128!(i64);
encode_using_signed_leb128!(i128);
encode_using_signed_leb128!(isize);

macro_rules! encode_using_unsigned_leb128 {
  ($i:ident) => {
    impl Encode for $i {
      fn encode(&self) -> Bytes {
        let mut val = self.clone();
        let mut bs: Bytes = vec![];
        loop {
          let b = (val & 0x7f) as Byte;
          val >>= 7;
          if val == 0 {
            bs.push(b);
            return bs;
          }
          bs.push(b | 0x80);
        }
      }
    }
  };
}

encode_using_unsigned_leb128!(u8);
encode_using_unsigned_leb128!(u16);
encode_using_unsigned_leb128!(u32);
encode_using_unsigned_leb128!(u64);
encode_using_unsigned_leb128!(u128);
encode_using_unsigned_leb128!(usize);

impl Encode for &str {
  fn encode(&self) -> Bytes {
    self.as_bytes().to_vec()
  }
}
