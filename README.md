# parse_command_line.R
This utility simplifies command line parsing for R scripts. It supports optional commands and subcommands, and both Boolean and value-based arguments. 

To use:
1) Comment out the call to `test_parser()` -- a test function to demonstrate the parser -- at the bottom of the source file.
- A helper function `debug_print()` is provided, plus a `debug` variable, to print informative messages if desired. Set `debug == TRUE` to enable them.
2) Call `init_command_line_parser`, providing the name of the Rscript, a short description string, and an optional version number. (eg `init_command_line_parser ("MyProgram.R", "My test program", "0.0.1")`) 
3) Register arguments with `reg_argument`. For instance, to allow the user to supply a filename on the command line, you might call `reg_argument("--outfile","-o","outfile",NA,argsType$TypeValue,'specify outfile')`. This registers a long parameter (`--outfile`), a short version (`-o`), a variable to hold the provided value (`outfile`), a default value (`NA`), the parameter type (`argsType$TypeValue`), and a description string. 
- Valid parameter types are `argsType$TypeBool` for Boolean values; `argsType$TypeValue` for arguments of type `--outfile=file`, `--outfile file`, and `-o file`; and `argsType$TypeMultiVal` for parameters where multiple values can be supplied, such as keywords: `-k key1 -k key2`. 
- If `argsType$TypeBool` is used, using the argument flips the default Boolean value. So for instance, if you call `reg_argument("--plot","-p","plot",FALSE,argsType$TypeBool,'plot output')`, the default value of `plot` will be `FALSE`. If `--plot` is included in the argument list it will be set to `TRUE`.
- `sparam` is optional in `reg_argument()`. So eg, you could call: `reg_argument("--noshort",NA,"noshort",FALSE,argsType$TypeBool,"no short-form argument")`
4) Register optional commands with `reg_command`, and subcommands with `reg_subcmd`. For instance, you might want to support an `add` command: `reg_command ("add", "Add a value")`, with two subcommands: `reg_subcommand ("name", "add", "Add a new name")` and `reg_subcommand ("phone", "add", "Add a new phone number")`. 
- Note that if a command is registered, one must be provided on the command line. The command is assumed to be the first argument; subcommands are assumed to be the second argument. eg, `MyProgram.R add phone <params>`
5) Collect command line arguments on your script with `args <- commandArgs(trailingOnly = TRUE)`. 
6) Pass those arguments to the parser with `user_args <- parse_command_line(args)`.
7) Access the resulting values as list elements in `user_args`. eg, `user_args$outfile` or `user_args$command`. Unrecognized parameters will be stored in `user_args$unknowns`.

The parser provides a built-in `usage()` function, which generates a formatted help message. If lparam `--help` and sparam `-?` are not otherwise used, it will automatically interpret those to generate a help message. If lparam `--ver` and sparam `-V` are not used, it will interpret those to output the script version information. 

A `parse_date()` function is provided for handling dates. It supports dates in the format `YYYY-MM-DD`, `YYYYMMDD`, `YYYY-MM`, and `YYYY`. The return value is a tuple of integers: `c(year, month, date)`. For instance, `parse_date("2019-12-31")` returns `c(2019, 12, 31)`; `parse_date("2019-12")` returns `c(2019, 12, NA)`.
