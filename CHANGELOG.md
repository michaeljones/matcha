# Changelog

## 0.19.0

- **Breaking change**: Templates now use the `StringTree` type from the standard library instead of the `StringBuilder`
  type which has been deprecated. This means that projects using `matcha` should use `gleam_stdlib >= 0.42.0`.

  Generated template modules now expose a `render_tree` function instead of `render_builder`. They continue to expose
  a `render` function as before.

## 0.18.0

- Changed to only generating the `.gleam` output if the `.matcha` file has a newer modified timestamp than the `.gleam`
  file. This avoids constantly regenerating the output files which in turn avoids messing up the formatting if the
  output file had been formatted.

  Thank you to @perrygeo for noticing and fixing the issue.

- Switched from `structopt` to `clap` for command line options parsing. This should be transparent to the user.

## 0.17.0

- Changed to use `type` keyword in `StringBuilder` import in the generated Gleam code as suits recent
  versions of the Gleam compiler.

## 0.16.0

- Support `{> fn` and `{> pub fn` syntax for declaring private and public functions within the
  template file. This allows better locality of helper functions and easy access to template
  behaviour.

  Template files which only have function declarations and no meaningful template content will not
  have the `render` and `render_builder` methods so they can be used library modules with just
  helper functions.

## 0.15.0

- **Breaking change**: No longer includes `gleam/list` import for templates that don't use the `for`
  syntax. This is to avoid the import potentially be unused. If you use `gleam/list` for other
  things in your template then you'll have to explicitly import it.

  Thank you to @lpil for reporting the issue.

## 0.14.0

- Fixed build for MacOS arm64.

  Thank you to @michallepicki for noticing and fixing the issue.

## 0.13.0

- **Breaking change**: Rename the project to 'matcha' to remove the double-barrelled nature of the
  previous name and avoid the clash with the `.tea` extension name.

- **Breaking change**: The template files should now have a `.matcha` suffix instead of `.tea`.

## 0.12.0

- **Breaking change**: Rename the project to 'green-tea' as the previous name was essentially
  'templates' which is too plain and generic to be comfortable using.

- **Breaking change**: The template files should now have a `.tea` suffix instead of `.gleamx`. This
  might change again as there is a "Tea" language already.

## 0.11.0

- Support arbitrary Gleam syntax in value, if-condition and for-loop source locations. This means
  that more work can be done by the compiled template file rather than having to preprocess data
  before rendering the template.

  The template system does not parse the Gleam expressions and so will allow invalid Gleam syntax to
  be added which will cause an error with the Gleam compiler. 

  Thank you to @lpil for the suggestion.

## 0.10.0

- Add comment to generated files to indicate that they are auto-generated and ideally should not be
  edited.

  Thank you to @lpil for the suggestion.

## 0.9.0

- Use non-zero exit code for process if any error is encountered. Basic behaviour that was missing.

  Thank you to @lpil for the suggestion.

## 0.8.0

- **Breaking change**: all parameters to the template must now be declared using the `{> with ... as
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

