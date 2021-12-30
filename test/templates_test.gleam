import gleeunit
import gleeunit/should

import template/identifier

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn identifier_test() {
  identifier.render("Gleam")
  |> should.equal("Hello Gleam, good to meet you\n")
}
