##########################
## parse_command_line.R: functions for parsing command line parameters
##
## (c) 2019 Jeffrey M. Perkel
##
## Version history
## 1.0.0 -- 12 Jul 2019 -- initial release
## 1.0.1 -- 16 Jul 2019 -- autogenerate usage display
## 1.0.2 -- 28 Jul 2019 -- support parsing of commands
## 1.0.3 -- 26 Aug 2019 -- add subcommand support
##########################
suppressPackageStartupMessages({
  library(tidyverse)
})

# set debug to TRUE to see debug messages
debug <- TRUE


debug_print <- function (s) {
   if (debug == TRUE) {
     if (is.character (s)) writeLines (s)
     else print (s)
   }
}


# tables to hold the possible command line params
args_table <- data.frame(lparam = NA, sparam = NA, var = NA, default = NA, argType = NA, 
                          desc = NA, stringsAsFactors = FALSE)
cmds_table <- data.frame(cmd = NA, desc = NA, stringsAsFactors = FALSE)
subcmds_table <- data.frame(subcmd = NA, parent = NA, desc = NA, stringsAsFactors = FALSE)

desc_str <- NA # a short description, eg eg "My test program"
script <- NA # name of the program, eg "MyProgram.R"
ver <- NA # version number, eg "1.0.0"

# Define an enum for different modes; access with argsType$<enum_element>
# ht https://stackoverflow.com/questions/33838392/enum-like-arguments-in-r
#
# TypeBool == TRUE/FALSE
# TypeValue == any value expressed as "--arg=Value", "--arg Value", or "-a Value"
# TypeMultiVal == TypeValue, but allowing multiple values to be stored (ie, keywords)
#
argsEnum <- function() {
  list (TypeBool = 1, TypeValue = 2, TypeMultiVal = 3) 
}
argsType <- argsEnum()


##
## Create a help message; adds --help and --ver params if not provided.
##
usage <- function() {
  # remove first row of the tables, which is all NA
  args_table <- args_table[-1,]
  cmds_table <- cmds_table[-1,]
  subcmds_table <- subcmds_table[-1,]
  
  writeLines(paste('\n', script, ': ', desc_str, sep = ''))
  writeLines(paste('  USAGE: Rscript', script, 
                   ifelse(nrow(cmds_table) > 0, '[COMMAND]', ''),
                   ifelse(nrow(subcmds_table) > 0, '[SUBCOMMAND]', ''),
                   '<params>'))
  if (!is.na(ver)) {
    writeLines(paste('\tVer:', ver))
  }
  writeLines('')
  
  if (!'--help' %in% args_table$lparam && !'-?' %in% args_table$sparam) {
    my_df <- data.frame(lparam = '--help', sparam = '-?', var = NA, default = NA, 
                        argType = argsType$TypeBool, desc = 'Display help', stringsAsFactors = FALSE)
    args_table <- rbind(args_table, my_df)
  }
  if (!'--ver' %in% args_table$lparam && !'-V' %in% args_table$sparam && !is.na(ver)) {
    my_df <- data.frame(lparam = '--ver', sparam = '-V', var = NA, default = NA, 
                        argType = argsType$TypeBool, desc = 'Display version info', stringsAsFactors = FALSE)
    args_table <- rbind(args_table, my_df)
  }
  
  # sort the tables alphabetically
  args_table <- args_table[order(args_table$lparam),]
  
  if (nrow(cmds_table) > 0) {
    cmds_table <- cmds_table[order(cmds_table$cmd),]
    
    writeLines('  COMMANDS:')
    for (r in 1:nrow(cmds_table)) {
      myrow <- cmds_table[r,]
      writeLines(paste('\t',str_pad(myrow$cmd, max(nchar(myrow$cmd)), "right"), 
                       ':', myrow$desc))
      if (nrow(subcmds_table) > 0) {
        subtable <- subcmds_table[subcmds_table$parent == myrow$cmd, ]
        if (nrow(subtable) > 0) {
          writeLines("\t\tSUBCOMMANDS:")
          subtable <- subtable[order(subtable$subcmd),]
          writeLines(paste('\t\t',str_pad(subtable$subcmd, max(nchar(subtable$subcmd)), "right"), 
                         ':', subtable$desc))
        } # if (nrow(subtable) > 0)
      } # if (nrow(subcmds_table) > 0) 
    } # for
    writeLines('')
  } # if (nrow(cmds_table) > 0)

  writeLines('  PARAMETERS:')
  writeLines(paste0('\t',str_pad(args_table$lparam, max(nchar(args_table$lparam)), "right"),
                    # need 5 spaces to account for missing sparam, eg ' (-m)'
                   ifelse (!is.na(args_table$sparam), paste0(' (', args_table$sparam, ')'), 
                           paste(rep(' ', 5), collapse = '')),
                   ifelse (args_table$desc == '', '', ': '), 
                   args_table$desc, 
                   ifelse(is.na(args_table$default), '', 
                          paste0('\n\t', paste(rep(' ', max(nchar(args_table$lparam)) + 10), collapse = ''), 
                                'default: ', args_table$default))
                   ))
} # usage


##
## Initialize command-line parsing
##    script: name of the R script
##    desc: description of the script
##    ver: tool version number (string)
##
init_command_line_parser <- function (script, desc, ver = NA) {
  script <<- script
  desc_str <<- desc
  ver <<- ver
} # init_command_line_parser


##
## Register required commands. Use for programs with syntax: 
##    myprog.R <COMMAND> [optional-params]
##
##    Call reg_command() for each allowed command. Commands are assumed to be the first 
##       argument after the script name, and only one command is allowed.
##
##       cmd: expected command 
##       desc: description string for the param, for usage()
##
reg_command <- function(cmd, desc = '') {
  if (is.na(desc_str)) {
    stop("Error: reg_command(): Command line parser not initialized.", call. = FALSE)
  }
  
  if (cmd %in% cmds_table$cmd ) {
    stop(paste0("Error: reg_command(): duplicated command: ", cmd), call. = FALSE)
  }
  
  my_df <- data.frame(cmd = cmd, desc = desc, stringsAsFactors = FALSE)
  cmds_table <<- rbind(cmds_table, my_df) 
} # reg_command


##
## Register required subcommands. Use for programs with syntax: 
##    myprog.R <COMMAND> <SUBCOMMMAND> [optional-params]
##
##    Call reg_subcmd() for each allowed subcommand. Subcommands are assumed to be the second
##       argument after the script name, and only one subcommand is allowed.
##
##       subcmd: expected command 
##       parent: parent command
##       desc: description string for the param, for usage()
##
reg_subcmd <- function(subcmd = subcmd, parent = parent, desc = '') {
  if (is.na(desc_str)) {
    stop("Error: reg_subcmd(): Command line parser not initialized.", call. = FALSE)
  }
  
  if (!(parent %in% cmds_table$cmd)) {
    stop(paste0("Error: reg_subcmd(): Parent command not initialized: ", parent), call. = FALSE)
  }
  
  if (subcmd %in% subcmds_table$subcmd) {
    stop(paste0("Error: reg_subcmd(): duplicated subcommand: ", subcmd), call. = FALSE)
  }
  
  my_df <- data.frame(subcmd = subcmd, parent = parent, desc = desc, stringsAsFactors = FALSE)
  subcmds_table <<- rbind(subcmds_table, my_df)
} # reg_subcmd


##
## Register an expected command line argument. Use for programs with syntax:
##    myprog.R [optional-args]
##
##    Call reg_argument() for each allowed parameter.
##       lparam: long-form arg (eg '--outfile')
##       sparam: short-form arg (eg '-o); use NA for none.
##       var: variable name to hold the value
##       default: default value for var
##       argType: argsType$TypeBool for logical values (TRUE/FALSE)
##                argsType$TypeValue for params of type '--outfile=myfile.txt', '--outfile myfile.txt'
##                  or '-o outfile.txt'
##                argsType$TypeMultiVal to store multiple values (ie, keywords)
##       desc: description string for the arg, for usage()
##
reg_argument <- function(lparam, sparam, var, default, argType, desc = '') {
  if (is.na(desc_str)) {
    stop("Error: reg_argument(): Command line parser not initialized.", call. = FALSE)
  }
  
  if (sparam %in% args_table$sparam[!is.na(args_table$sparam)] || 
      lparam %in% args_table$lparam[!is.na(args_table$lparam)]) {
    writeLines(paste("Warning: reg_argument(): duplicated param:", lparam, sparam))
  }
  
  my_df <- data.frame(lparam = lparam, sparam = sparam, var = var, default = default, argType = argType, 
                      desc = desc, stringsAsFactors = FALSE)
  args_table <<- rbind(args_table, my_df) 
} # reg_argument


##
## Parse command line, using the tables of allowed options created using reg_argument() & reg_command()
## Returns a list of variables and values, preloaded with default values unless changed by user
##
parse_command_line <- function(args) {
  # remove the first line of the tables, which are all NA
  args_table <- args_table[-1,]
  cmds_table <- cmds_table[-1,]
  subcmds_table <- subcmds_table[-1,]

  debug_print ("Args table:")
  debug_print (args_table)
  debug_print ("\n")
  debug_print ("Commands table:")
  debug_print (cmds_table)
  debug_print ("\n")
  debug_print ("Subcommands table:")
  debug_print (subcmds_table)
  debug_print ("\n")

  # if neither reg_arguments() nor reg_command() has been called, there's no table to process; 
  # return the args as a list under the name 'unknowns'
  if (nrow(args_table) == 0 && nrow(cmds_table) == 0) {
    writeLines ("Warning: parse_command_line(): no cmdline params or commands registered.")
    return (list(unknowns = args))
  }

  # no sense doing anything if the user just wants help...
  if (args[1] %in% c("--help","-?") && (!'--help' %in% args_table$lparam && !'-?' %in% args_table$sparam)) {
    usage() 
    stop(call. = FALSE)
  }
  if (!is.na(ver) && args[1] %in% c("--ver", "-V") && 
      (!'--ver' %in% args_table$lparam && !'-V' %in% args_table$sparam)) {
    writeLines(paste(script, ': ', desc_str, '\n\tVer: ', ver, '\n', sep = ''))
    stop(call. = FALSE)
  }
  
  # create an empty list to store results, name each entry by its var name, & store defaults
  mydata <- vector("list", nrow(args_table))
  names(mydata) <- args_table$var
  for (name in names(mydata)) {
    mydata[[name]] <- args_table$default[args_table$var == name]
  }
  
  # process commands if any
  i <- 1
  if (nrow(cmds_table) > 0) {
    debug_print ("Parsing commands...")
    if (args[i] %in% cmds_table$cmd) { 
      mydata[["command"]] <- args[i]
      debug_print (paste("Command matched:", args[i]))

      # filter subcmds_table to include only entries where parent == command
      subcmds_table <- subcmds_table[subcmds_table$parent == mydata$command,]
    } # if (args[i] %in% cmds_table$cmd)
    else { 
      stop (paste("parse_command_line(): unknown command:", args[i]), call. = FALSE)
    }
    i <- i + 1
  } # if (nrow(cmds_table) > 0)
  
  # process subcommands if any
  if (nrow(subcmds_table) > 0) {
    debug_print ("Parsing subcommands...")
    if (args[i] %in% c("--help", "-?")) {
      usage() # TO-DO: ALLOW SPECIFIC HELP FOR SUBCOMMANDS
      stop(call. = FALSE)
    } # if (args[i] %in% c("--help", "-?"))
    else if (args[i] %in% subcmds_table$subcmd) {
      mydata[["subcmd"]] <- args[i]
      debug_print (paste("Subcommand matched:", args[i]))
    } # if (args[i] %in% subcmds_table$subcmd)
    else {
      stop (paste0("parse_command_line(): \'", args[i], "\' is not a subcommand of parent \'", 
                   mydata$command, "\'"), call. = FALSE)
    }
    i <- i + 1
  } # if (nrow(subcmds_table) > 0)
  
  # process arguments
  debug_print ("Processing arguments...")
  unk <- 0 # number of unknown params found
  while (i <= length(args)) {
    p = args[i]
    myrow <- NULL
    
    debug_print (paste("Processing argument:",p))
    if (p %in% args_table$lparam[!is.na(args_table$lparam)]) {
      temp <- args_table[!is.na(args_table$lparam),]
      myrow <- temp[temp$lparam == p,]
    }
    else if (p %in% args_table$sparam[!is.na(args_table$sparam)]) {
      temp <- args_table[!is.na(args_table$sparam),]
      myrow <- temp[temp$sparam == p,]
    }
    else if (strsplit(p, "=")[[1]][1] %in% args_table$lparam[!is.na(args_table$lparam)]) {
      temp <- args_table[!is.na(args_table$lparam),]
      myrow <- temp[temp$lparam == strsplit(p, "=")[[1]][1],]
    }
    else if (p %in% c("--help", "-?")) {
      usage() # TO-DO: usage(mydata$command)
      stop(call. = FALSE)
    }
    else {
      # unrecognized argument
      unk <- unk + 1
      mydata[["unknowns"]][unk] <- p
      writeLines (paste("Warning: parse_command_line(): unknown param:", p))
    }
    
    debug_print (myrow)
    if (!is.null(myrow)) {
      if(myrow$argType == argsType$TypeBool) { # if the param is a logical type, save the opposite logical type
        mydata[[myrow$var]] <- !as.logical(myrow$default)
      }
      # TypeValue; store either what is after the '=', or the next param in the arg string
      # sparam <value> || lparam <value>
      else if ((p == myrow$sparam && !is.na(myrow$sparam)) || (p == myrow$lparam && !is.na(myrow$lparam))) { 
        if (i == length(args)) { # ie, there is no i+1
            stop(paste("Expected value missing after param:", p), call. = FALSE)
        }
        if (myrow$argType == argsType$TypeValue) {
          mydata[[myrow$var]] <- args[i+1]
        }
        # if the same arg is passed multiple times, collect all responses (ie, for keywords)
        else if (myrow$argType == argsType$TypeMultiVal) {
          idx <- ifelse(is.na(mydata[[myrow$var]][1]), 1, length(mydata[[myrow$var]])+1)
          mydata[[myrow$var]][idx] <- args[i+1]
        }
        i = i + 1 # iterate the counter to ignore the next param
      } # else (lparam Value)
      else { # lparam=Value
        if (myrow$argType == argsType$TypeValue) {
          mydata[[myrow$var]] <- strsplit(p, "=")[[1]][2]
        }
        else if (myrow$argType == argsType$TypeMultiVal) {
        idx <- ifelse(is.na(mydata[[myrow$var]][1]), 1, length(mydata[[myrow$var]])+1)
          mydata[[myrow$var]][idx] <- strsplit(p, "=")[[1]][2]
        }
      } # else (lparam = Value)
    } # if (is.null(myrow))
    i = i + 1 # advance to next param
  } # while
  return (mydata)
} # parse_command_line


##
## Parses a date in YYYY-MM-DD, YYYY-MM or YYYY format and returns a tuple: c(y, m, d)
## 
parse_date <- function(d) {
  year <- NA
  month <- NA
  day <- NA
  
  if (grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', d) == TRUE) {
    myDate <- try(as.Date (d, format = "%Y-%m-%d"))
    if (class (myDate) == "try-error" || is.na(myDate)) {
      stop(paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
    year <- as.integer(format(myDate, "%Y"))
    month <- as.integer(format(myDate, "%m"))
    day <- as.integer(format(myDate, "%d"))
  }
  else if (grepl('^[0-9]{8}$', d) == TRUE) {
    myDate <- try(as.Date (d, format = "%Y%m%d"))
    if (class (myDate) == "try-error" || is.na(myDate)) {
      stop(paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
    year <- as.integer(format(myDate, "%Y"))
    month <- as.integer(format(myDate, "%m"))
    day <- as.integer(format(myDate, "%d"))
  }
  else if (grepl('^[0-9]{4}-[0-9]{2}$', d) == TRUE) {
    year <- as.integer(substr(d, 1, 4))
    month <- as.integer(substr(d, 6, 7))
    if ( (is.na(year)) ||
         (is.na(month)) ||
         !(month %in% 1:12)) {
      stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
  }
  else if (grepl('^[0-9]{4}$', d) == TRUE) {
    year <- as.integer(substr(d, 1, 4))
    if (is.na(year)) {
      stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
  }
  else {
    stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
  }
  return(as.integer(c(year, month, day)))
} # parse_date


test_parser <- function() {
  ver <- "1.0.0"
  init_command_line_parser('MyRprogram.R','My test program', ver)
  
  # an example TypeBool; default == FALSE; if used in cmdline, will be set to TRUE
  reg_argument("--plot","-p","plot",FALSE,argsType$TypeBool,'plot output')
  # example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
  reg_argument("--infile","-i","infile",NA,argsType$TypeValue,'select infile')
  reg_argument("--outfile","-o","outfile",NA,argsType$TypeValue,'specify outfile')
  reg_argument("--date","-d","date",NA,argsType$TypeValue,'specify date')
  # an example lparam w/ no sparam
  reg_argument("--noshort",NA,"noshort",FALSE,argsType$TypeBool, "no short form argument")
  # an example TypeMultiVal, where all supplied params are stored
  reg_argument("--keyword","-k","keyword",NA,argsType$TypeMultiVal,'keywords')
  #  test to ensure --ver/-V not added if a conflicting param is already registered
  reg_argument("--verify","-V","verify",FALSE,argsType$TypeBool,'verify something')
  
  reg_command("add", "add a value")
  reg_command("delete", "delete a value")
  reg_command("revise", "revise a value")

  reg_subcmd("add1", "add", "add subcmd 1")
  reg_subcmd("add2", "add", "add subcmd 2")
  reg_subcmd("add3", "add", "add subcmd 3")
  reg_subcmd("del1", "delete", "delete subcommand 1")
  reg_subcmd("del2", "delete", "delete subcommand 2")
  
  cmdline <- c("delete", "del1", "-o", "myfilename.txt", "-p", "--date=2019-12-31", 
               "--noshort", "-z", "-k", "key1", "-k", "key2")
  
  usage()
  
  writeLines ("Command line:")
  writeLines (paste(cmdline, collapse = ' '))
  writeLines ('')
  mydata <- parse_command_line(cmdline)
  
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

  writeLines ("\nParsing dates...")
  writeLines (paste("Date:", mydata$date))
  print (parse_date(mydata$date))
  writeLines ("Date: 2019-12")
  print (parse_date("2019-12"))
  writeLines ("Date: 2019")
  print (parse_date("2019"))
  writeLines ("Date: 2019-13-31")
  print (parse_date("2019-13-31")) # bad date!
} # test_parser()

# comment out for regular use
test_parser()
