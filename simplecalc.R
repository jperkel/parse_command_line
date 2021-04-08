source ("parse_command_line.R")

init_command_line_parser('simplecalc.R','A simple calculator program', '1.0.0')

arguments <- list(
  list("--verbose","-v","verbosity",1,argsType$TypeCount,"verbosity")
)
reg_argument_list(arguments)

positionals <- list(
  list("lhv",NA,"left-hand value"),
  list("rhv",NA,"right-hand value"),
  list("op",NA,"operation to perform (eg: '*', '+', '**')")
)
reg_positionals_list(positionals)

args <- commandArgs(trailingOnly = TRUE)
mydata <- new_parse_command_line(args)

if (mydata$lhv == "pi") mydata$lhv <- pi
if (mydata$rhv == "pi") mydata$rhv <- pi
mydata$lhv <- as.numeric(mydata$lhv)
mydata$rhv <- as.numeric(mydata$rhv)
mydata$verbosity <- as.numeric(mydata$verbosity)
if (mydata$verbosity > 4) mydata$verbosity <- 4
mydata$verbosity <- c("none","some","more","lots")[as.numeric(mydata$verbosity)]
print (mydata)
# input check
if (any(is.na(c(mydata$lhv, mydata$rhv))) || 
    !mydata$op %in% c("+","-","*","/","**","%/%","%%","^")) 
  stop (paste("One or more required inputs missing or invalid:", "\nlhv:", mydata$lhv, "\nrhv:", mydata$op, "\nop:", mydata$rhv))

print (mydata)

answer <- round(eval(parse(text = paste(mydata$lhv, mydata$op, mydata$rhv))), 3)

writeLines(switch(mydata$verbosity,
       "none" = as.character(answer),
       "some" = paste0("answer: ", answer),
       "more" = paste0("answer: ", mydata$lhv, ' ', mydata$op, ' ', mydata$rhv, " = ", answer),
       "lots" = paste0("The answer to the question \'", mydata$lhv, ' ', mydata$op, ' ', mydata$rhv, "\' is ", answer, '.')
       ))
