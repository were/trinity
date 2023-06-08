use std::collections::HashMap;

/// Target triple of the machine.
pub struct TargetTriple {
  /// Instruction set.
  isa: String,
  /// Hardware vendor, mostly unknown.
  vendor: String,
  /// Operating system.
  os: String
}

impl TargetTriple {

  /// Construct a new target triple from the given string.
  pub fn new(tt: String) -> Self {
    let triple = tt.split(r"-").map(|x| x.to_string()).collect::<Vec<_>>();
    if triple.len() == 3 {
      TargetTriple {
        isa: triple[0].clone(),
        vendor: triple[1].clone(),
        os: triple[2].clone()
      }
    } else {
      TargetTriple {
        isa: String::from(""), vendor: String::from(""), os: String::from("")
      }
    }
  }

  /// Dump it to text.
  pub fn to_string(&self) -> String {
    format!("{}-{}-{}", self.isa, self.vendor, self.os)
  }

}

/// The data layout of the target machine.
pub struct DataLayout {
  /// The endian of the machine
  big_endian: bool,
  /// The pointer alignment
  pointer_alignment: HashMap<usize, (usize, usize, usize, usize)>,
  /// LLVM naming mangling
  mangling: char,
  /// The value alignment in bits on the stack
  stack_alignment: usize
}

impl DataLayout {

  pub fn new(layout: String) -> Self {
    let mut res = DataLayout {
      big_endian: false,
      pointer_alignment: HashMap::new(),
      mangling: 'e',
      stack_alignment: 64
    };
    for elem in layout.split(r"-") {
      // TODO(@were): Support i, v, f, a, F, n, ni
      match *elem.as_bytes().first().unwrap() as char {
        'e' => {
          // e -> small endian
          res.big_endian = false
        }
        'E' => {
          // E -> big endian
          res.big_endian = true
        }
        'm' => {
          // m:<mangling>
          assert!(elem.as_bytes()[1] as char == ':');
          res.mangling = elem.as_bytes()[2] as char
        }
        'S' => {
          // S:<pref>
          assert!(elem.as_bytes()[1] as char == ':');
          res.stack_alignment = elem[2..].parse::<usize>().unwrap()
        }
        'p' => {
          // p[n]:<size>:<abi>[:<pref>][:<idx>]
          let palign = elem.split(":").into_iter().collect::<Vec<_>>();
          let n = if palign[0] == "p" {
            0 as usize
          } else {
            palign[0].parse::<usize>().unwrap()
          };
          let psize = palign[1].parse::<usize>().unwrap();
          let pabi = palign[2].parse::<usize>().unwrap();
          let ppref = if 3 < palign.len() {
            palign[2].parse::<usize>().unwrap()
          } else {
            psize
          };
          let pidx = if 4 < palign.len() {
            palign[3].parse::<usize>().unwrap()
          } else {
            psize
          };
          res.pointer_alignment.insert(n, (psize, pabi, ppref, pidx));
        }
        _ => {
          panic!("{} is not valid", elem)
        }
      }
    }
    res
  }

  /// Dump it to string.
  pub fn to_string(&self) -> String {
    let endian = if self.big_endian { 'E' } else { 'e' };

    let mut res = format!("{}-m:{}-S:{}", endian, self.mangling, self.stack_alignment);

    if self.pointer_alignment.len() != 0 {
      let pointers = self.pointer_alignment.iter().map(|x| {
        format!("p{}:{}:{}:{}:{}", x.0, x.1.0, x.1.1, x.1.2, x.1.3)
      }).collect::<Vec<_>>().join("-");
      res = format!("{}-{}", res, pointers)
    }

    res
  }

}
