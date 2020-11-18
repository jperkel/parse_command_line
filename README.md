# parse_command_line.R
This utility simplifies command line parsing for R scripts. It supports optional commands and subcommands, and both Boolean and value-based arguments. 

Suppose you want to write a script for creating and maintaining a check book. You might call it like so: `MyCheckbook.R withdraw cash --amount=100` to indicate that you took out some cash at the ATM. `parse_command_line.R` interprets the arguments to `MyCheckbook.R` so you don't have to. 

To use, call `init_command_line_parser()` to initialize the parsing tool. Then call `reg_argument()` (or `reg_argument_list()`) to register expected command line arguments, `reg_command()` or `reg_command_list()` to register expected commands, and `reg_subcmd()` or `reg_subcmd_list()` for subcommands. 

To indicate a withdrawal of $100 in cash, you might call `MyCheckbook.R withdraw cash --amount=100 --msg='birthday gift'`. Here, `withdraw` is the command, `cash` is a subcommand and `--amount=100` and `msg='birthday gift'` are arguments. You might also add a `deposit` command, and a `check` subcommand, eg `MyCheckbook.R withdraw check --number 123 --amount=50 --payee='Electric Co.'` to log check #123 to the electric company in the amount of $50.)

Note that not all registered arguments must be provided on the command line. The default parameter passed to `reg_argument()` preloads the default. Thus, you could have an argument whose default value is FALSE. When `parse_command_line()` is called, the variable will be set to FALSE, unless it is included in the command line, in which case it will be TRUE.

When registering arguments, you must indicate the type of value you expect to receive. Valid parameter types are `argsType$TypeBool` for Boolean values; `argsType$TypeValue` for arguments of type `--outfile=file`, `--outfile file`, and `-o file`; and `argsType$TypeMultiVal` for parameters where multiple values can be supplied, such as keywords: `-k key1 -k key2`. Also allowed are `argsType$TypeMetered` for parameters whose value increments each time the argument is used. Thus `-v -v -v` would result in a value of 3. 

If `argsType$TypeBool` is used, using the argument flips the default Boolean value. So for instance, if you call `reg_argument("--plot","-p","plot",FALSE,argsType$TypeBool,'plot output')`, the default value of `plot` will be `FALSE`. If `--plot` is included in the argument list it will be set to `TRUE`. Arguments of the form `--plot=TRUE` are also allowed.

```
  init_command_line_parser('MyCheckbook.R','My checkbook program', '1.0.0')

  # an example TypeBool; default == FALSE; if used in cmdline, will be set to TRUE
  reg_argument("--rev-chronological",NA,"revchronological",FALSE,argsType$TypeBool,'Display newest entries first')
  # example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
  reg_argument("--infile","-i","infile",NA,argsType$TypeValue,'location of your checkbook file')

  # or we can register in a single call, as a list, eg:
  arguments <- list(
    # example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
    list("--outfile","-o","outfile",NA,argsType$TypeValue,'location of output file'),
    list("--date","-d","date",NA,argsType$TypeValue,'specify date'),
    list("--msg","-m","msg",NA,argsType$TypeValue,'memo line message'),
    list("--amount","-a","amount",NA,argsType$TypeValue,'specify dollar amount'),
    list("--payee","-p","payee",NA,argsType$TypeValue,'specify payee'),
    list("--number","-n","cknum",NA,argsType$TypeValue,'specify check number'),
    # an example TypeMultiVal, where all supplied params are stored
    list("--keyword","-k","keyword",NA,argsType$TypeMultiVal,'keywords'),
    # an example TypeMetered, where each use of the param increments a variable
    list("--verbose","-v","verbose",0,argsType$TypeMetered,'verbose level')
  )
  reg_argument_list(arguments)
  
  # we can register commands one at a time...
  # reg_command("withdraw", "add a withdrawal")
  # reg_command("deposit", "add a deposit")
  # reg_command("edit", "update a record")
  
  # or as a list
  cmds <- list(
    list("withdraw", "add a withdrawal"),
    list("plot", "graph output"),
    list("deposit", "add a deposit"),
    list("edit", "update a record"),
    list("find", "find a record")
  )
  reg_command_list(cmds)

  # and we can register subcommands one at a time or as a list.
  reg_subcmd("cash", "withdraw", "add a cash withdrawal")
  reg_subcmd("check", "withdraw", "add a check withdrawal")

  subcmds <- list(
    list("paycheck", "deposit", "add a paycheck deposit"),
    list("reimbursement", "deposit", "add a reimbursement"),
    list("bankfee", "withdraw", "add a bank fee")
  )
  reg_subcmd_list(subcmds)
``` 

`parse_command_line()` returns a list of all possible registered arguments. Any arguments not specified on the command line will equal the default values provided in `reg_argument()` or `reg_argument_list()`. 

Each line of the file should take the form `var_name=value`, where `var_name` matches the third parameter in a previous call to `reg_argument()`. For instance, if you typically want to display your checkbook in reverse-chronological order, you could include in your configuration file:

```
revchronological=TRUE
```

`parse_command_line.R` provides a `usage()` function to create a formatted help message based on the `desc` strings passed as the final arguments to `reg_argument()`, `reg_command()` and `reg_subcmd()`. .

Parse the command line like so: 

```
  args <- commandArgs(trailingOnly = TRUE)
  mydata <- parse_command_line(args)
```

`mydata` is a list in which each entry corresponds to a registered argument. Commands and subcommands are stored in `mydata$command` and `mydata$subcmd` respectively. Unrecognized arguments are stored in `mydata$unknowns`.

```
  writeLines ("\nAfter parse_command_line()...")
  writeLines (paste("command:",mydata$command))
  writeLines (paste("subcommand:",mydata$subcmd))
  writeLines (paste("revchronological:", mydata$revchronological))
  writeLines (paste("infile:", mydata$infile))
  writeLines (paste("outfile:",mydata$outfile))
  writeLines (paste("date:",mydata$date))
  writeLines (paste("msg:",mydata$msg))
  writeLines (paste("amount:",mydata$amount))
  writeLines (paste("payee:",mydata$payee))
  writeLines (paste("cknum:",mydata$cknum))
  writeLines (paste("keywords:",mydata$keyword))
  writeLines (paste("unknowns:",mydata$unknowns))
  writeLines (paste("verbose level:", mydata$verbose))
```

To use this tool:
1) Include this module in your program with `source ('/path/to/parse_command_line.R')`
2) Comment out the call to `test_parser()` -- a test function to demonstrate the parser -- at the bottom of the source file.
- A helper function `debug_print()` is provided, plus a `debug` variable, to print informative messages if desired. Set `debug == TRUE` to enable them.
3) Call `init_command_line_parser()` in your code, providing the name of the Rscript, a short description string, and an optional version number. (eg `init_command_line_parser ("MyProgram.R", "My test program", "0.0.1")`) 
4) Register arguments with `reg_argument()`. For instance, to allow the user to supply a filename on the command line, you might call `reg_argument("--outfile","-o","outfile",NA,argsType$TypeValue,'specify outfile')`. This registers a long parameter (`--outfile`), a short version (`-o`), a variable to hold the provided value (`outfile`), a default value (`NA`), the parameter type (`argsType$TypeValue`), and a description string. 
- `sparam` is optional in `reg_argument()`. eg: `reg_argument("--print",NA,"print",FALSE,argsType$TypeBool,"print output")`
- Arguments can also be supplied as a list of lists in a single call. For example, using `args <- list(list("--plot","-p","plot",FALSE,argsType$TypeBool,"plot output"), list("--outfile","-o","outfile",NA,argsType$TypeValue,'specify outfile'))` and `reg_argument_list(args)`.
5) Register optional commands with `reg_command()`, and subcommands with `reg_subcmd()`.  
- Note that if a command is registered, one must be provided on the command line. The command is assumed to be the first argument; subcommands are assumed to be the second argument. eg, `MyRprogram.R add phone <params>`
- Commands and subcommands can also be supplied as a list in a single call using `reg_commands_list()` and `reg_subcommand_list()`, respectively.
6) Collect command line arguments on your script with `args <- commandArgs(trailingOnly = TRUE)`. 
7) Pass those arguments to the parser with eg `myargs <- parse_command_line(args)`.
8) Access the resulting values as list elements in `myargs`. eg, `myargs$outfile`. Commands and subcommands are stored in `myargs$command` and `myargs$subcmd`, respectively. Unrecognized parameters will be stored in `myargs$unknowns`.

A `parse_date()` function is provided for handling dates. It supports dates in the format `YYYY-MM-DD`, `YYYYMMDD`, `YYYY-MM`, and `YYYY`. The return value is a tuple of integers: `c(year, month, date)`. For instance, `parse_date("2019-12-31")` returns `c(2019, 12, 31)`; `parse_date("2019-12")` returns `c(2019, 12, NA)`.
