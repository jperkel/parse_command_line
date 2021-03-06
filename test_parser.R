source ("parse_command_line.R")

# call this program using any of the arguments and commands below to see what the parser finds
# use --help to see the usage output
init_command_line_parser('MyCheckbook.R','My checkbook program', '1.0.0')

# we can register arguments one at a time, eg:  
# an example TypeBool; default == FALSE; if used in cmdline, will be set to TRUE
reg_argument("--rev-chronological",NA,"revchronological",FALSE,argsType$TypeBool,'Display newest entries first')
# example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
reg_argument("--infile","-i","infile",NA,argsType$TypeValue,'location of your checkbook file',"command|subcmd")

# or we can register in a single call, as a list, eg:
arguments <- list(
  # example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
  list("--outfile","-o","outfile",NA,argsType$TypeValue,'location of output file'),
  list("--date","-d","date",NA,argsType$TypeValue,'specify date'),
  # an example argument whose scope is limited to command = withdraw (with any subcmd)
  list("--msg","-m","msg",NA,argsType$TypeValue,'memo line message',"withdraw"),
  list("--amount","-a","amount",NA,argsType$TypeValue,'specify dollar amount'),
  # example arguments whose scope is limited to command = withdraw, subcmd = check
  list("--payee","-p","payee",NA,argsType$TypeValue,'specify payee',c("withdraw|check","deposit")),
  list("--number","-n","cknum",NA,argsType$TypeValue,'specify check number',"withdraw|check"),
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

args <- commandArgs(trailingOnly = TRUE)

writeLines ("\nParsing command line...")
writeLines (paste("Command line: MyCheckbook.R", paste(args, collapse = ' ')))
mydata <- parse_command_line(args)
writeLines ("Done!")

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

# writeLines ("\nParsing dates...")
# writeLines (paste("Date: 2019-12-31"))
# print (parse_date("2019-12-31"))
# writeLines ("Date: 2019-12")
# print (parse_date("2019-12"))
# writeLines ("Date: 2019")
# print (parse_date("2019"))
# writeLines ("Date: 2019-13-31")
# print (parse_date("2019-13-31")) # bad date!
