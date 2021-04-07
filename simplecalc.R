source ("parse_command_line.R")

init_command_line_parser('simplecalc.R','A simple calculator program', '1.0.0')

arguments <- list(
  list("--verbose","-v","verbosity",0,argsType$TypeCount,"verbosity")
)
reg_argument_list(arguments)

positionals <- list(
  list("lhv",NA,"left-hand value"),
  list("rhv",NA,"right-hand value"),
  list("op",NA,"operation to perform (eg: '*', '+', '**')")
)
reg_positionals_list(positionals)
usage()

args <- commandArgs(trailingOnly = TRUE)
mydata <- new_parse_command_line(args)
print (mydata)

answer <- round(eval(parse(text = paste(mydata$lhv, mydata$op, mydata$rhv))), 3)
mydata$verbosity <- as.character(mydata$verbosity)

switch(mydata$verbosity,
       '0'= print(paste("answer:", answer)),
       writeLines(paste("answer:", mydata$lhv, mydata$op, mydata$rhv, "=", answer)) 
       )
