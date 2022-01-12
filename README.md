# Gleam Templates

Generate type-safe Gleam modules from text-based template files.

This project provides a Rust program that parses a basic template format and outputs Gleam modules
with 'render' functions that can be imported and called to render the template with different
parameters.

## Installation

Download pre-built binaries for the latest release from the
[Releases](https://github.com/michaeljones/gleam-templates/releases) page.

Build from source with:

```
cargo install --path .
```

## Usage

Run:

```
templates
```

At the root of your project and it will walk your project folder structure and compile any template
files it finds.

Template files should have a `.gleamx` extension though that needs to change as it promises JSX-like
behaviour instead of text templates. Templates are compiled into `.gleam` files that can be imported
like any other regular module. The modules expose a `render` function, that returns a `String`, and
`render_builder` function that returns a `StringBuilder`.

Some errors, mostly syntax, will be picked up by the Rust code but it is possible to generate
invalid modules and so the Gleam compiler will pick up further errors.

## Status

Alpha. The error messages are poor and there will be plenty of flaws and oversights.

## Syntax

The syntax is inspired by [Jinja](https://jinja.palletsprojects.com/).

### Value

You can use `{{ name }}` syntax to insert the value of name into the rendered template. The
function generated from the template will have a labelled argument matching the identifier used.

```jinja
{{ name }}
```

### If

You can use `{% %}` blocks to create an if-statement using the `if`, `else` and `endif` keywords.
The function generated from the template will have a labelled argument matching the identifier used
after `if`.

The `else` is optional.

```jinja
{% if is_admin %}Admin{% else %}User{% endif %}
```

### For

You can use `{% %}` blocks to create for-loops using the `for`, `in` and `endfor` keywords.  The
function generated from the template will have a labelled argument matching the identifier used
after `in`.

```html+jinja
<ul>
{% for entry in list %}
    <li>{{ entry }}</li>
{% endfor %}
</ul>
```

### Import

You can use the `{>` syntax to add import statements to the template. These are used to import types
to use with the `with` syntax below to help Gleam check variables used in the template.

```
{> import my_user.{MyUser}
```

### With

You can use `{>` syntax to add `with` statements with associate types with variables in the
template. The type is used for the corresponding argument in the function generated from the
template. The Gleam compiler can infer simple types like booleans and strings but needs type
annotations for more complex structures like a custom types.

```
{> import my_user.{MyUser}

{> with user_record as MyUser

{{ user_record.name }}
```

## Output

A template like:

```
{> import my_user.{User}
{> with user_obj as User
Hello{% if user_obj.is_admin %} Admin{% endif %}
```

is compiled to a Gleam module:

```gleam
import gleam/string_builder.{StringBuilder}
import gleam/list
import my_user.{User}

pub fn render_builder(user_obj user_obj: User) -> StringBuilder {
  let builder = string_builder.from_string("")
  let builder = string_builder.append(builder, "Hello")
  let builder = case user_obj.is_admin {
    True -> {
      let builder = string_builder.append(builder, " Admin")
      builder
    }
    False -> builder
  }
  let builder = string_builder.append(builder, "
")

  builder
}

pub fn render(user_obj user_obj: User) -> String {
  string_builder.to_string(render_builder(user_obj: user_obj))
}
```

Which you can import and call `render` or `render_builder` on with the appropriate arguments.

## Tests

Rust tests can be run with `cargo test`. They use [insta](http://insta.rs/) for snapshots.

Gleam tests can be run with `cargo run && gleam test`.


