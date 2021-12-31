use std::collections::HashMap;

type Byte = u8;
type Bytes = Vec<Byte>;

pub trait Encode {
  fn encode(&self) -> Bytes;
}

macro_rules! encode {
  ($prefix:expr) => {{
    let mut bytes: Bytes = Vec::new();
    bytes.push($prefix);
    bytes
  }};
  ($prefix:expr, $a:expr) => {{
    let mut bytes: Bytes = vec![$prefix];
    bytes.append(&mut $a.encode());
    bytes
  }};
  ($prefix:expr, $a:expr, $b:expr) => {{
    let mut bytes: Bytes = vec![$prefix];
    bytes.append(&mut $a.encode());
    bytes.append(&mut $b.encode());
    bytes
  }};
}

pub struct Module {
  pub start: Option<FuncIndex>,
  pub type_section: TypeSection,
  pub func_section: FuncSection,
  pub code_section: CodeSection,
}

impl Module {
  pub fn new() -> Self {
    Self {
      start: None,
      type_section: TypeSection::new(),
      func_section: FuncSection::new(),
      code_section: CodeSection::new(),
    }
  }

  pub fn add_func(&mut self, sig: Type, code: Code) -> FuncIndex {
    let type_index = self.type_section.append(sig);
    let func_index = self.func_section.push(type_index);
    self.code_section.push(code);
    func_index
  }
}

impl Encode for Module {
  fn encode(&self) -> Bytes {
    let mut bytes: Bytes = vec![
      0x00, b'a', b's', b'm', // magic cookie
      0x01, 0x00, 0x00, 0x00, // version number
    ];

    bytes.append(&mut self.type_section.encode());
    bytes.append(&mut self.func_section.encode());

    if let Some(start) = &self.start {
      let mut contents = start.encode();
      let mut section = encode!(0x08);
      section.append(&mut contents.len().encode());
      section.append(&mut contents);
      bytes.append(&mut section);
    }

    bytes.append(&mut self.code_section.encode());
    bytes
  }
}

#[derive(Debug, Copy, Clone)]
pub struct TypeIndex(usize);

impl Encode for TypeIndex {
  fn encode(&self) -> Bytes {
    self.0.encode()
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

  pub fn append(&mut self, ty: Type) -> TypeIndex {
    if let Some(cached_id) = self.cache.get(&ty) {
      cached_id.clone()
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
    let mut contents = self.types.encode();
    let mut bytes: Bytes = vec![0x01];
    bytes.append(&mut contents.len().encode());
    bytes.append(&mut contents);
    bytes
  }
}

pub struct FuncIndex(usize);

impl Encode for FuncIndex {
  fn encode(&self) -> Bytes {
    self.0.encode()
  }
}

pub struct FuncSection(LengthPrefixedVec<TypeIndex>);

impl FuncSection {
  fn new() -> Self {
    Self(LengthPrefixedVec::new())
  }

  fn push(&mut self, index: TypeIndex) -> FuncIndex {
    let func_index = FuncIndex(self.0.len());
    self.0.push(index);
    func_index
  }
}

impl Encode for FuncSection {
  fn encode(&self) -> Bytes {
    let mut contents = self.0.encode();
    let mut bytes: Bytes = vec![0x03];
    bytes.append(&mut contents.len().encode());
    bytes.append(&mut contents);
    bytes
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
    let mut contents = self.0.encode();
    let mut bytes: Bytes = vec![0x0a];
    bytes.append(&mut contents.len().encode());
    bytes.append(&mut contents);
    bytes
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

#[derive(Debug, Copy, Clone)]
pub struct LocalIndex(usize);

impl Encode for LocalIndex {
  fn encode(&self) -> Bytes {
    self.0.encode()
  }
}

#[derive(Debug)]
pub enum Inst {
  BlockEnd,

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
      Inst::BlockEnd => encode!(0x0b),

      Inst::Drop => encode!(0x1a),

      Inst::I32Const(val) => encode!(0x41, val),
      Inst::I32Add => encode!(0x6a),
      Inst::I32Mul => encode!(0x6c),

      Inst::LocalGet(idx) => encode!(0x20, idx),
      Inst::LocalSet(idx) => encode!(0x21, idx),
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

impl Encode for Code {
  fn encode(&self) -> Bytes {
    let mut contents: Bytes = vec![];
    contents.append(&mut self.locals.encode());
    contents.append(&mut self.insts.encode());

    let mut bytes: Bytes = vec![];
    bytes.append(&mut contents.len().encode());
    bytes.append(&mut contents);
    bytes
  }
}

#[allow(dead_code)]
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
      Type::I32 => encode!(0x7f),
      Type::I64 => encode!(0x7e),
      Type::F32 => encode!(0x7d),
      Type::F64 => encode!(0x7c),
      Type::Func(params, returns) => encode!(0x60, params, returns),
    }
  }
}

impl<T: Encode> Encode for Vec<T> {
  fn encode(&self) -> Bytes {
    let mut bytes: Bytes = vec![];
    for elem in self {
      bytes.append(&mut elem.encode());
    }
    bytes
  }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LengthPrefixedVec<T: Encode>(Vec<T>);

impl<T: Encode> LengthPrefixedVec<T> {
  pub fn new() -> Self {
    Self(vec![])
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
    bytes.append(&mut self.0.encode());
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
