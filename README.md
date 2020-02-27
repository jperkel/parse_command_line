# parse_command_line.R
This utility simplifies command line parsing for R scripts. It supports optional commands and subcommands, and both Boolean and value-based arguments. 

Suppose you have a script for creating and reading an address book. You might call it like so: `MyRprogram.R add name 'Jane Smith'`. `parse_command_line.R` interprets the arguments to `Rscript` so you don't have to. 

To use, call `init_command_line_parser()` to initialize the parsing tool. Then call `reg_argument()` (or `reg_argument_list()`) to register expected command line arguments, `reg_command()` or `reg_command_list()` to register expected commands, and `reg_subcmd()` or `reg_subcmd_list()` for subcommands. 

When registering arguments, you must indicate the type of value you expect to receive. Valid parameter types are `argsType$TypeBool` for Boolean values; `argsType$TypeValue` for arguments of type `--outfile=file`, `--outfile file`, and `-o file`; and `argsType$TypeMultiVal` for parameters where multiple values can be supplied, such as keywords: `-k key1 -k key2`. 

If `argsType$TypeBool` is used, using the argument flips the default Boolean value. So for instance, if you call `reg_argument("--plot","-p","plot",FALSE,argsType$TypeBool,'plot output')`, the default value of `plot` will be `FALSE`. If `--plot` is included in the argument list it will be set to `TRUE`. Arguments of the form `--plot=TRUE` are also allowed.

```
  init_command_line_parser('MyRprogram.R','My test program', '1.0.0')

  # we can register arguments one at a time, eg:  
  # an example TypeBool; default == FALSE; if used in cmdline, will be set to TRUE
  reg_argument("--plot","-p","plot",FALSE,argsType$TypeBool,'plot output')
  # example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
  reg_argument("--infile","-i","infile",NA,argsType$TypeValue,'select infile')

  # or we can register in a single call, as a list, eg:
  params <- list(
    # example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
    list("--outfile","-o","outfile",NA,argsType$TypeValue,'specify outfile'),
    list("--date","-d","date",NA,argsType$TypeValue,'specify date'),
    # an example lparam w/ no sparam
    list("--noshort",NA,"noshort",FALSE,argsType$TypeBool, "no short form argument"),
    # an example TypeMultiVal, where all supplied params are stored
    list("--keyword","-k","keyword",NA,argsType$TypeMultiVal,'keywords'),
    #  test to ensure --ver/-V not added if a conflicting param is already registered
    list("--verify","-V","verify",FALSE,argsType$TypeBool,'verify something')
  )
  reg_argument_list(params)
  
  # we can register commands one at a time...
  # reg_command("add", "add a value")
  # reg_command("delete", "delete a value")
  # reg_command("revise", "revise a value")
  
  # or as a list
  cmds <- list(
    list("add", "add a value"),
    list("delete", "delete a value"),
    list("revise", "revise a value")
  )
  reg_command_list(cmds)

  # and we can register subcommands one at a time or as a list.
  reg_subcmd("add1", "add", "add subcmd 1")
  reg_subcmd("add2", "add", "add subcmd 2")
  reg_subcmd("add3", "add", "add subcmd 3")

  subcmds <- list(
    list("del1", "delete", "delete subcommand 1"),
    list("del2", "delete", "delete subcommand 2"),
    list("add1", "revise", "revise subcmd 1")
  )
  reg_subcmd_list(subcmds)
``` 

`parse_command_line.R` provides a `usage()` function to create a formatted help message based on the `desc` strings passed as the final arguments to `reg_argument()`, `reg_command()` and `reg_subcmd()`.

Parse the command line like so: 

```
  args <- commandArgs(trailingOnly = TRUE)
  mydata <- parse_command_line(args)
```

`mydata` is a list in which each entry corresponds to a registered argument. Commands and subcommands are stored in `mydata$command` and `mydata$subcmd` respectively. Unrecognized arguments are stored in `mydata$unknowns`.

```
  writeLines ("\nAfter parse_command_line()...")
  writeLines (paste("plot:", mydata$plot))
  writeLines (paste("infile:", mydata$infile))
  writeLines (paste("outfile:",mydata$outfile))
  writeLines (paste("date:",mydata$date))
  writeLines (paste("unknowns:",mydata$unknowns))
  writeLines (paste("command:",mydata$command))
  writeLines (paste("subcommand:",mydata$subcmd))
  writeLines (paste("noshort:",mydata$noshort))
  writeLines (paste("keyword:",mydata$keyword))
```

To use this tool:
1) Comment out the call to `test_parser()` -- a test function to demonstrate the parser -- at the bottom of the source file.
- A helper function `debug_print()` is provided, plus a `debug` variable, to print informative messages if desired. Set `debug == TRUE` to enable them.
2) Call `init_command_line_parser()` in your code, providing the name of the Rscript, a short description string, and an optional version number. (eg `init_command_line_parser ("MyProgram.R", "My test program", "0.0.1")`) 
3) Register arguments with `reg_argument()`. For instance, to allow the user to supply a filename on the command line, you might call `reg_argument("--outfile","-o","outfile",NA,argsType$TypeValue,'specify outfile')`. This registers a long parameter (`--outfile`), a short version (`-o`), a variable to hold the provided value (`outfile`), a default value (`NA`), the parameter type (`argsType$TypeValue`), and a description string. 
- `sparam` is optional in `reg_argument()`. eg: `reg_argument("--print",NA,"print",FALSE,argsType$TypeBool,"print output")`
- Arguments can also be supplied as a list of lists in a single call. For example, using `args <- list(list("--plot","-p","plot",FALSE,argsType$TypeBool,"plot output"), list("--outfile","-o","outfile",NA,argsType$TypeValue,'specify outfile'))` and `reg_argument_list(args)`.
4) Register optional commands with `reg_command()`, and subcommands with `reg_subcmd()`. For instance, you might want to support an `add` command: `reg_command ("add", "Add a value")`, with two subcommands: `reg_subcommand ("name", "add", "Add a new name")` and `reg_subcommand ("phone", "add", "Add a new phone number")`. 
- Note that if a command is registered, one must be provided on the command line. The command is assumed to be the first argument; subcommands are assumed to be the second argument. eg, `MyRprogram.R add phone <params>`
- Commands and subcommands can also be supplied as a list in a single call using `reg_commands_list()` and `reg_subcommand_list()`, respectively.
5) Collect command line arguments on your script with `args <- commandArgs(trailingOnly = TRUE)`. 
6) Pass those arguments to the parser with eg `myargs <- parse_command_line(args)`.
7) Access the resulting values as list elements in `myargs`. eg, `myargs$outfile`. Commands and subcommands are stored in `myargs$command` and `myargs$subcmd`, respectively. Unrecognized parameters will be stored in `myargs$unknowns`.

The parser provides a built-in `usage()` function, which generates a formatted help message. If lparam `--help` and sparam `-?` are not otherwise used, it will automatically interpret those to generate a help message. If lparam `--ver` and sparam `-V` are not used, it will interpret those to output the script version information. 

A `parse_date()` function is provided for handling dates. It supports dates in the format `YYYY-MM-DD`, `YYYYMMDD`, `YYYY-MM`, and `YYYY`. The return value is a tuple of integers: `c(year, month, date)`. For instance, `parse_date("2019-12-31")` returns `c(2019, 12, 31)`; `parse_date("2019-12")` returns `c(2019, 12, NA)`.
