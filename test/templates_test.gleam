import gleeunit
import gleeunit/should

import template/identifier
import template/two_identifiers
import template/double_identifier_usage
import template/if_statement
import template/if_else_statement
import template/nested_if_statement
import template/for_loop

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

pub fn double_identifier_usage_test() {
  double_identifier_usage.render("Double")
  |> should.equal("Double usage, Double usage\n")
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

pub fn nested_if_statement_test() {
  nested_if_statement.render(is_user: True, is_admin: True)
  |> should.equal("Hello Admin\n")

  nested_if_statement.render(is_user: True, is_admin: False)
  |> should.equal("Hello User\n")

  nested_if_statement.render(is_user: False, is_admin: False)
  |> should.equal("Hello\n")
}

pub fn for_loop_test() {
  for_loop.render(["Anna", "Bill", "Christine"])
  |> should.equal("Hello, to Anna and to Bill and to Christine and everyone else\n")

  for_loop.render([])
  |> should.equal("Hello, everyone else\n")
}
