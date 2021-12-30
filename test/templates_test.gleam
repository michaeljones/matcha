import gleeunit
import gleeunit/should

import template/identifier
import template/two_identifiers

pub fn main() {
  gleeunit.main()
}

pub fn identifier_test() {
  identifier.render("Gleam")
  |> should.equal("Hello Gleam, good to meet you\n")

  identifier.render("User")
  |> should.equal("Hello User, good to meet you\n")
}

pub fn two_identifiers_test() {
  two_identifiers.render("Gleam", "good")
  |> should.equal("Hello Gleam, good to meet you\n")

  two_identifiers.render("User", "nice")
  |> should.equal("Hello User, nice to meet you\n")
}
