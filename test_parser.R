source ("parse_command_line.R")

# call this program using any of the arguments and commands below to see what the parser finds
# use --help to see the usage output
init_command_line_parser('MyCheckbook.R','My checkbook program', '1.0.0')

# we can register arguments one at a time, eg:  
# an example TypeBool; default == FALSE; if used in cmdline, will be set to TRUE
reg_argument("--search-or",NA,"search_or",FALSE,argsType$TypeBool,'Any keyword will match (Boolean OR)')
# example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
reg_argument("--infile","-i","infile",NA,argsType$TypeValue,'location of your checkbook file',"command|subcmd")

# or we can register in a single call, as a list, eg:
arguments <- list(
  # example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
  list("--outfile","-o","outfile",NA,argsType$TypeValue,'location of output file'),
  list("--date","-d","date",NA,argsType$TypeValue,'specify date'),
  # an example argument whose scope is limited to command = withdraw (with any subcmd)
  list("--msg","-m","msg",NA,argsType$TypeValue,'memo line message'),
  list("--amount","-a","amount",NA,argsType$TypeValue,'specify dollar amount'),
  # example arguments whose scope is limited to command = withdraw, subcmd = check
  list("--payee","-p","payee",NA,argsType$TypeValue,'specify payee'),
  list("--number","-n","cknum",NA,argsType$TypeValue,'specify check number'),
  # an example TypeMultiVal, where all supplied params are stored
  list("--keyword","-k","keyword",NA,argsType$TypeMultiVal,'keywords'),
  # an example TypeCount, where each use of the param increments a variable
  list("--verbose","-v","verbose",NA,argsType$TypeCount,'verbose level'),
  list("--range","-r","range",NA,argsType$TypeRange,'range') # ,
#  list(NA,NA,"file1",NA,argsType$TypePositional,'specify file 1'),
#  list(NA,NA,"file2",NA,argsType$TypePositional,'specify file 2'),
#  list(NA,NA,"file3",NA,argsType$TypePositional,'specify file 3')
)
reg_argument_list(arguments)

# we can register commands one at a time...
# reg_command("withdraw", "add a withdrawal")
# reg_command("deposit", "add a deposit")
# reg_command("edit", "update a record")

# or as a list
cmds <- list(
  list("withdraw", "add a withdrawal"),
#  list("plot", "graph output"),
  list("deposit", "add a deposit"),
  list("edit", "update a record"),
  list("find", "find a record")
)
reg_command_list(cmds)

# and we can register subcommands one at a time or as a list.
reg_subcmd("cash", "withdraw", "add a cash withdrawal")
reg_subcmd("check", "withdraw", "add a check withdrawal")

subcmds <- list(
  # list("paycheck", "deposit", "add a paycheck deposit"),
  list("reimbursement", "deposit", "add a reimbursement"),
  list("bankfee", "withdraw", "add a bank fee"),
  list("check", "deposit", "add a check deposit")
)
reg_subcmd_list(subcmds)

args <- commandArgs(trailingOnly = TRUE)

writeLines ("\nParsing command line...")
writeLines (paste("Command line: MyCheckbook.R", paste(args, collapse = ' ')))
mydata <- new_parse_command_line(args)
writeLines ("Done!")

writeLines ("\nAfter parse_command_line()...")
writeLines (paste("command:",mydata$command))
writeLines (paste("subcommand:",mydata$subcmd))
writeLines (paste("search-or:", mydata$search_or))
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
writeLines (paste("range:", ifelse(is.na(mydata$range), NA, paste(mydata$range1, '-', mydata$range2))))
# writeLines (paste("file 1:", mydata$file1))
# writeLines (paste("file 2:", mydata$file2))
# writeLines (paste("file 3:", mydata$file3))

# writeLines ("\nParsing dates...")
# writeLines (paste("Date: 2019-12-31"))
# print (parse_date("2019-12-31"))
# writeLines ("Date: 2019-12")
# print (parse_date("2019-12"))
# writeLines ("Date: 2019")
# print (parse_date("2019"))
# writeLines ("Date: 2019-13-31")
# print (parse_date("2019-13-31")) # bad date!
