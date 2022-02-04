# Changelog

## Unreleased

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

