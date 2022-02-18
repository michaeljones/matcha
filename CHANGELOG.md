# Changelog

## 0.9.0

- Use non-zero exit code for process if any error is encountered. Basic behaviour that was missing.

  Thank you to @lpil for the suggestion.

## 0.8.0

- Breaking change: all parameters to the template must now be declared using the `{> with ... as
  ...` syntax. The template generation will no longer try to automatically deduce parameters from
  the template contents. Being explicit allows us to include static content from imported modules
  without the generation getting confused about what is a parameter and what is an import.

## 0.7.0

- Added support for '{[ name ]}' syntax to allow for inserting string builder values into the
  template using the underlying `append_builder` call instead of the `append` functon that we use
  for strings.

  Thank you to @michallepicki for the suggestion.

## 0.6.0

- Added support for 'for .. as .. in ..' syntax to allow associating a type with the items in the
  list being iterated.

  Thank you to @lpil for the suggestion.

## 0.5.0

- Added --version flag to executable to print out current version.

## 0.4.0

- Automatic release builds uploaded to GitHub.

  Thanks to @michallepicki.

## 0.3.0

- Improve error reporting
  - Added suggestions when encountering an unexpected token
  - Added colours to error output
- Added 'verbose' flag for printed output of processed files

## 0.2.0

- Improve error reporting

  Some errors now including code snippets highlighting the problem.

## 0.1.0 - Initial release

- Basic functionality

