import gleeunit
import gleeunit/should

import template/identifier
import template/two_identifiers
import template/if_statement
import template/if_else_statement

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

pub fn if_statement_test() {
  if_statement.render(True)
  |> should.equal("Hello User\n")

  if_statement.render(False)
  |> should.equal("Hello\n")
}

pub fn if_else_statement_test() {
  if_else_statement.render(True)
  |> should.equal("Hello User\n")

  if_else_statement.render(False)
  |> should.equal("Hello Unknown\n")
}
