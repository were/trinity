/// Target triple of a module.
pub struct TargetTriple {
  /// Instruction set.
  isa: String,
  /// Hardware vendor, mostly unknown.
  vendor: String,
  /// Operating system.
  os: String
}

impl TargetTriple {

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

  pub fn to_string(&self) -> String {
    format!("{}-{}-{}", self.isa, self.vendor, self.os)
  }

}
