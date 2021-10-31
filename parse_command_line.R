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
## 1.0.4 -- 27 Feb 2020 -- args, cmds and subcmds can be supplied as lists
## 1.0.5 -- 29 Feb 2020 -- support config.txt file
## 1.1.0 -- 23 Dec 2020 -- supports scoping of arguments, ie associating with specific commands/subcmds
##########################

library(stringr)

# tables to hold the possible command line params
args_table <- data.frame(lparam = NA, sparam = NA, var = NA, default = NA, argType = NA, 
                          help = NA, scope = NA, stringsAsFactors = FALSE)
cmds_table <- data.frame(cmd = NA, help = NA, stringsAsFactors = FALSE)
subcmds_table <- data.frame(subcmd = NA, parent = NA, help = NA, stringsAsFactors = FALSE)

desc_str <- NA # a short description, eg eg "My test program"
script <- NA # name of the program, eg "MyProgram.R"
ver <- NA # version number, eg "1.0.0"

# Define an enum for different modes; access with argsType$<enum_element>
# ht https://stackoverflow.com/questions/33838392/enum-like-arguments-in-r
#
# TypeBool: TRUE/FALSE
# TypeValue: any value expressed as "--arg=Value", "--arg Value", or "-a Value"
# TypeMultiVal: TypeValue, but allowing multiple values to be stored (ie, keywords)
# TypeCount: value increments each time the param is used. eg, -v -v yields 2
# TypeRange: splits a TypeValue like "1:3" into two variables, "1" and "3"
# TypePositional: required, final argument 
# 
argsEnum <- function() {
  list (TypeBool = 1, TypeValue = 2, TypeMultiVal = 3, TypeCount = 4, TypeRange = 5, TypePositional = 6) 
}
argsType <- argsEnum()


##
## Create a help message; adds --help and --ver params if not provided.
##
usage <- function() {
  # number of spaces for each indentation level
  lvl1_indent <- 2
  lvl2_indent <- 6
  lvl3_indent <- 10
  lvl4_indent <- 14

  buffer_str <- function(spacer) {
    return (paste0(rep(' ', spacer), collapse = ''))
  }
  
  # remove first row of the tables, which is all NA
  args_table <- args_table[-1,]
  cmds_table <- cmds_table[-1,]
  subcmds_table <- subcmds_table[-1,]
  
  positionals <- NULL
  pos_string <- ""
  if (any(args_table$argType == argsType$TypePositional)) {
    positionals <- args_table$var[args_table$argType == argsType$TypePositional]
    pos_string <- paste0('[', positionals, ']', collapse = ' ')
  }
  
  writeLines(paste0('\n', script, ': ', desc_str))
  writeLines(paste0(buffer_str(lvl1_indent), 'USAGE: Rscript ', script, ' ',
                   ifelse(nrow(cmds_table) > 0, '[COMMAND] ', ''),
                   ifelse(nrow(subcmds_table) > 0, '[SUBCOMMAND] ', ''),
                   ifelse(nrow(args_table) > 0, '<optional arguments> ', ''),
                   pos_string
                   ))
  if (!is.na(ver)) {
    writeLines(paste0(buffer_str(lvl2_indent), 'Ver: ', ver))
  }
  writeLines('')
  
  # sort the tables alphabetically
  args_table <- args_table[order(args_table$lparam),]
  args_table$scope <- ifelse(args_table$scope == "NA", NA, args_table$scope)
  
  if (nrow(cmds_table) > 0) {
    cmds_table <- cmds_table[order(cmds_table$cmd),]
    
    writeLines(paste0(buffer_str(lvl1_indent), 'COMMANDS:'))
    for (r in 1:nrow(cmds_table)) {
      myrow <- cmds_table[r,]
      writeLines(paste0(buffer_str(lvl2_indent),str_pad(myrow$cmd, max(nchar(cmds_table$cmd)), "right"), 
                       ' : ', myrow$help))
      if (nrow(subcmds_table) > 0) {
        subtable <- subcmds_table[subcmds_table$parent == myrow$cmd, ]
        if (nrow(subtable) > 0) {
          writeLines(paste0(buffer_str(lvl3_indent), "SUBCOMMANDS:"))
          subtable <- subtable[order(subtable$subcmd),]
          writeLines(paste0(buffer_str(lvl4_indent),str_pad(subtable$subcmd, max(nchar(subtable$subcmd)), "right"), 
                         ' : ', subtable$help))
        } # if (nrow(subtable) > 0)
      } # if (nrow(subcmds_table) > 0) 
    } # for
    writeLines('')
  } # if (nrow(cmds_table) > 0)

  if (!is.null(positionals[1])) {
    writeLines(paste0(buffer_str(lvl1_indent), "REQUIRED ARGUMENTS: "))
    for (p in positionals) {
      writeLines(paste0(buffer_str(lvl2_indent),str_pad(p, max(nchar(args_table$lparam), na.rm = TRUE), "right"), 
                        buffer_str(5), ": ", args_table$help[args_table$var == p]))
    }
  }
  
  # remove positionals from the table
  args_table <- args_table[args_table$argType != argsType$TypePositional,]

  writeLines('')
  writeLines(paste0(buffer_str(lvl1_indent), 'OPTIONAL ARGUMENTS:'))
  for (r in 1:nrow(args_table)) {
    myrow <- args_table[r,]
    writeLines(paste0(
      buffer_str(lvl2_indent),str_pad(myrow$lparam, max(nchar(args_table$lparam)), "right"),
      # need 5 spaces to account for sparam if none provided, eg ' (-m)'
      ifelse (!is.na(myrow$sparam), paste0(' (', myrow$sparam, ')'), buffer_str(5)),
      ifelse (myrow$help == '', '', ': '),
      myrow$help, 
      ifelse(is.na(myrow$default), '',
             paste0('\n', buffer_str(lvl2_indent + max(nchar(args_table$lparam)) + 10),
                    'default: ', 
                    ifelse (myrow$argType == argsType$TypeBool, as.logical(myrow$default), myrow$default))
      ),
      ifelse(is.na(myrow$scope), '', 
             paste0('\n', buffer_str(lvl2_indent + max(nchar(args_table$lparam)) + 10),
                    "Valid for: ", gsub('_', ', ', myrow$scope)))
    ))
  }
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
##       help: help string for the param, for usage()
##
reg_command <- function(cmd, help = '') {
  if (is.na(desc_str)) {
    stop("Error: reg_command(): Command line parser not initialized.", call. = FALSE)
  }
  
  if (cmd %in% cmds_table$cmd) {
    stop(paste0("Error: reg_command(): duplicated command: ", cmd), call. = FALSE)
  }
  
  my_df <- data.frame(cmd = cmd, help = help, stringsAsFactors = FALSE)
  cmds_table <<- rbind(cmds_table, my_df) 
} # reg_command


##
## register commands using a list, eg:
## cmds <- list (list("cmd1", "help1"), list("cmd2", "help2"))
## reg_command_list (cmds)
##
reg_command_list <- function(clist) {
  ids <- c("cmd","help")
  for (c in clist) {
    stopifnot(length(c) == length(ids))
    reg_command(cmd = c[[1]], help = c[[2]])
  }
} # reg_command_list

  
##
## Register required subcommands. Use for programs with syntax: 
##    myprog.R <COMMAND> <SUBCOMMMAND> [optional-params]
##
##    Call reg_subcmd() for each allowed subcommand. Subcommands are assumed to be the second
##       argument after the script name, and only one subcommand is allowed.
##
##       subcmd: expected command 
##       parent: parent command
##       help: help string for the param, for usage()
##
reg_subcmd <- function(subcmd = subcmd, parent = parent, help = '') {
  if (is.na(desc_str)) {
    stop("Error: reg_subcmd(): Command line parser not initialized.", call. = FALSE)
  }
  
  # if (!(parent %in% cmds_table$cmd)) {
  #   stop(paste0("Error: reg_subcmd(): Parent command not initialized: ", parent), call. = FALSE)
  # }
  
  subtable <- subcmds_table[subcmds_table$parent == parent,]
  if (subcmd %in% subtable$subcmd) {
    stop(paste0("Error: reg_subcmd(): duplicated subcommand: ", subcmd), call. = FALSE)
  }
  
  my_df <- data.frame(subcmd = subcmd, parent = parent, help = help, stringsAsFactors = FALSE)
  subcmds_table <<- rbind(subcmds_table, my_df)
} # reg_subcmd


##
## register subcommands using a list, eg:
## subcmds <- list (list("subcmd1","parent1", "help1"), list("subcmd2", "parent2", "help2"))
## reg_subcmd_list (subcmds)
##
reg_subcmd_list <- function(slist) {
  ids <- c("subcmd","parent","help")
  for (s in slist) {
    stopifnot(length(s) == length(ids))
    reg_subcmd(subcmd = s[[1]], parent = s[[2]], help = s[[3]])
  }
} # reg_subcmd_list


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
##       help: help string for the arg, for usage()
##       scope: a list of commands & subcmds for which the arg is valid, given as a vector, 
##                eg, "c("command1|subcmd1", "command2")
##
reg_argument <- function(lparam, sparam, var, default, argType, help, scope = NA) {
  if (is.na(desc_str)) {
    stop("Error: reg_argument(): Command line parser not initialized.", call. = FALSE)
  }

  if (sparam %in% args_table$sparam[!is.na(args_table$sparam)] || 
      lparam %in% args_table$lparam[!is.na(args_table$lparam)]) {
    stop(paste("Error: reg_argument(): duplicated param:", lparam, sparam), call. = FALSE)
  }
  
  if (!is.na(scope[1])) {
    # scope is a list of commands and subcommands in which the argument is valid, separated by 
    # pipes and underscores. Only a command is req'd, and are separated from subcmds (if req'd) with a pipe.
    # eg, "command1|subcmd1_command2|subcmd2_command3"
    scope <- paste(scope, collapse = '_') 
  }

  my_df <- data.frame(lparam = lparam, sparam = sparam, var = var, default = default, argType = argType, 
                      help = help, scope = scope, stringsAsFactors = FALSE)
  args_table <<- rbind(args_table, my_df) 
} # reg_argument


##
## Register command line arguments using a list, eg
## args <- list (list("lparam1", "sparam1", "var1", default1, argType1, "help1", scope1), 
##               list("lparam2", "sparam2", "var2", default2, argType2, "help2", scope2))
## reg_argument_list(args)
##
reg_argument_list <- function(plist) {
  # scope is not required. So, check for the 6 required params, and if no scope provided, set to NA
  ids <- c("lparam","sparam","var","default","argType","help")
  
  for (p in plist) {
    if (length(p) > length(ids)) scope <- p[[7]]
    else scope <- NA
    
    reg_argument (lparam = p[[1]], sparam = p[[2]], var = p[[3]], default = p[[4]],
                  argType = p[[5]], help = p[[6]], 
                  scope = scope)
  }
} # reg_argument_list


# 
# Register a 'positional' command line argument (ie, the last argument in the list)
reg_positionals <- function(var, default, help) {
  reg_argument (lparam = NA, sparam = NA, var = var, default = default, argType = argsType$TypePositional, help = help)
} # reg_positionals


#
# Register a list of 'positional' arguments
reg_positionals_list <- function(plist) {
  ids <- c("var","default","help")
  
  for (p in plist) {
    stopifnot(length(p) == length(ids))
    reg_positionals(var = p[[1]], default = p[[2]], help = p[[3]])
  }
} # reg_positionals_list


##
## Parse command line, using the tables of allowed options created using reg_argument() & reg_command()
## Returns a list of variables and values, preloaded with default values unless changed by user
##
parse_command_line <- function(args) {
  # remove the first line of the tables, which are all NA
  args_table <- args_table[-1,]
  cmds_table <- cmds_table[-1,]
  subcmds_table <- subcmds_table[-1,]

  # if neither reg_arguments() nor reg_command() has been called, there's no table to process; 
  # return the args as a list under the name 'unknowns'
  if (nrow(args_table) == 0 && nrow(cmds_table) == 0) {
    writeLines ("Warning: parse_command_line(): no cmdline params or commands registered.")
    return (list(unknowns = args))
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
    if (args[i] %in% cmds_table$cmd) { 
      mydata[["command"]] <- args[i]

      # filter subcmds_table to include only entries where parent == command
      subcmds_table <- subcmds_table[subcmds_table$parent == mydata$command,]
    } # if (args[i] %in% cmds_table$cmd)
    
    else if (args[i] %in% c("--help", "-?")) {
      usage() 
      stop(call. = FALSE)
    } 
    
    else { 
      stop (paste("parse_command_line(): unknown command:", args[i]), call. = FALSE)
    }
    i <- i + 1
  } # if (nrow(cmds_table) > 0)
  
  # process subcommands if any
  if (nrow(subcmds_table) > 0) {
    if (args[i] %in% c("--help", "-?")) {
      usage() # TO-DO: ALLOW SPECIFIC HELP FOR SUBCOMMANDS
      stop(call. = FALSE)
    } # if (args[i] %in% c("--help", "-?"))
    else if (args[i] %in% subcmds_table$subcmd) {
      mydata[["subcmd"]] <- args[i]
    } # if (args[i] %in% subcmds_table$subcmd)
    else {
      stop (paste0("parse_command_line(): \'", args[i], "\' is not a subcommand of parent \'", 
                   mydata$command, "\'"), call. = FALSE)
    }
    i <- i + 1
  } # if (nrow(subcmds_table) > 0)
  
  # process arguments
  unk <- 0 # number of unknown params found
  while (i <= length(args)) {
    p = args[i]
    myrow <- NULL
    index <- NULL
    
    if (p %in% args_table$lparam[!is.na(args_table$lparam)]) {
      index <- which(args_table$lparam == p)
    }
    else if (p %in% args_table$sparam[!is.na(args_table$sparam)]) {
      index <- which(args_table$sparam == p)
    }
    else if (strsplit(p, "=")[[1]][1] %in% args_table$lparam[!is.na(args_table$lparam)]) {
      index <- which(args_table$lparam == strsplit(p, "=")[[1]][1])
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

    if (!is.null(index)) {
      myrow <- args_table[index,]
      
      # if this argument has specified parents, make sure they match
      if (!all(is.na(myrow$scope))) {
        # list cmd or cmd/subcmd pairs for this argument, eg: "withdraw|check" and "deposit"
        scope <- strsplit(strsplit(myrow$scope, '_')[[1]], '\\|')
        # query holds the cmd/subcmds used on the command line. if only a command was specified as a parent,
        # then only the command is checked. thus, an argument can apply to a specific cmd/subcmd pair,
        # or to all subcommands of a given command.
        query <- NA 
        if (nrow(cmds_table) > 0) query[1] <- mydata$command
        if (nrow(subcmds_table) > 0) query[2] <- mydata$subcmd
        
        # query_match is a vector of length (parents); each element is a Boolean that indicates a match
        # to the corresponding element of parents 
        query_match <- rep(FALSE, length(scope))
        # iterate over each parent
        for (j in 1:length(scope)) {
          # if only a command is provided as parent, test only the command
          if (length(scope[[j]]) == 1) query_match[j] <- (scope[[j]] == query[1])
          # otherwise, test both command and subcmd
          else query_match[j] <- (scope[[j]][1] == query[1] && scope[[j]][2] == query[2])
        }

        if (!any(query_match == TRUE)) {
          writeLines(paste0("\nWarning: Argument \'", p, "\' is incompatible with command|subcommand \'", paste(query, collapse = '|'), "\'. Ignoring."))
          unk <- unk + 1
          mydata[["unknowns"]][unk] <- p
          i <- i + 1
          next
        }
      }
      
      if(myrow$argType == argsType$TypeBool) { # if the param is a logical type, save the opposite logical type
        if ((p == myrow$sparam && !is.na(myrow$sparam)) || (p == myrow$lparam && !is.na(myrow$lparam))) {
          # if the argument exactly matches lparam or sparam
          mydata[[myrow$var]] <- !as.logical(myrow$default)
        }
        else { # we also accept param=Val (--lparam=TRUE, =FALSE, =T, =F)
          val <- as.logical(strsplit(p, "=")[[1]][2])
          if (is.na(val)) {
            unk <- unk + 1
            mydata[["unknowns"]][unk] <- p
            writeLines (paste("Warning: parse_command_line(): bad Boolean value (use T/F):", p))
          }
          else mydata[[myrow$var]] <- val
        }
      }
      
      # TypeCount -- each time the param is used, a value is incremented
      else if (myrow$argType == argsType$TypeCount) {
        if ((p == myrow$sparam && !is.na(myrow$sparam)) || (p == myrow$lparam && !is.na(myrow$lparam))) {
          mydata[[myrow$var]] <- as.integer(mydata[[myrow$var]]) + 1
        }
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
        else if (myrow$argType == argsType$TypeRange) {
          mydata[[myrow$var]] <- args[i+1]
          s <- strsplit(args[i+1], ':')[[1]]
          mydata[[paste0(myrow$var, 1)]] <- s[1]
          mydata[[paste0(myrow$var, 2)]] <- s[2]
        }
        i = i + 1 # iterate the counter to ignore the next param
      } # else (lparam Value)
      else { # lparam=Value
        val <- strsplit(p, "=")[[1]][2]
        if (myrow$argType == argsType$TypeValue) {
          # mydata[[myrow$var]] <- strsplit(p, "=")[[1]][2]
          mydata[[myrow$var]] <- val
        }
        else if (myrow$argType == argsType$TypeMultiVal) {
        idx <- ifelse(is.na(mydata[[myrow$var]][1]), 1, length(mydata[[myrow$var]])+1)
          # mydata[[myrow$var]][idx] <- strsplit(p, "=")[[1]][2]
        mydata[[myrow$var]][idx] <- val
        }
        else if (myrow$argType == argsType$TypeRange) {
          # val <- strsplit(p, "=")[[1]][2]
          mydata[[myrow$var]] <- val
          s <- strsplit(val, ':')[[1]]
          mydata[[paste0(myrow$var, 1)]] <- s[1]
          mydata[[paste0(myrow$var, 2)]] <- s[2]
        }
      } # else (lparam = Value)
    } # if (is.null(myrow))
    i = i + 1 # advance to next param
  } # while
  return (mydata)
} # parse_command_line


##
## Parses a date in YYYY-MM-DD, YYYYMMDD, YYYY-MM or YYYY format and returns a tuple: c(y, m, d)
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
    year <- as.integer(d)
    if (is.na(year)) {
      stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
  }
  else {
    stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
  }
  return(c(year, month, day))
} # parse_date


new_parse_command_line <- function(args) {
  # remove the first line of the tables, which are all NA
  args_table <- args_table[-1,]
  cmds_table <- cmds_table[-1,]
  subcmds_table <- subcmds_table[-1,]
  
  # if neither reg_arguments() nor reg_command() has been called, there's no table to process; 
  # return the args as a list under the name 'unknowns'
  if (nrow(args_table) == 0 && nrow(cmds_table) == 0) {
    writeLines ("Warning: new_parse_command_line(): no cmdline params or commands registered.")
    return (list(unknowns = args))
  }
  
  if (any(args %in% c("--help", "-?"))) {
    usage() 
    stop(call. = FALSE)
  }
  
  # create an empty list to store results, name each entry by its var name, & store defaults
  mydata <- vector("list", nrow(args_table))
  names(mydata) <- args_table$var
  for (name in names(mydata)) {
    mydata[[name]] <- args_table$default[args_table$var == name]
  }
  
  # process required args (TypePositional). args are processed in the order they are called.  
  if (any(args_table$argType == argsType$TypePositional)) {
    # reverse-sort so variables are loaded in the order called
    index <- sort(which(args_table$argType == argsType$TypePositional), decreasing = TRUE)
    if (length(args) == 0 || length(args) < length(index)) {
      usage()
      writeLines(paste0("new_parse_command_line(): one or more positional arguments missing"))
      stop(call. = FALSE)
    }
    for (i in index) {
      myrow <- args_table[i,]
      mydata[[myrow$var]] <- args[length(args)]
      args <- args[1:length(args)-1]
    }
  }
  
  # process commands if any
  i <- 1
  if (nrow(cmds_table) > 0) {
    if (args[i] %in% cmds_table$cmd) { 
      mydata[["command"]] <- args[i]
      
      # filter subcmds_table to include only entries where parent == command
      subcmds_table <- subcmds_table[subcmds_table$parent == mydata$command,]
    } 
    
    else if (is.na(args[i])) {
      stop("new_parse_command_line(): command required", call. = FALSE)
    }
    
    else { 
      stop (paste("new_parse_command_line(): unknown command:", args[i]), call. = FALSE)
    }
    i <- i + 1
  } 
  
  # process subcommands if any
  if (nrow(subcmds_table) > 0) {
    if (args[i] %in% subcmds_table$subcmd) {
      mydata[["subcmd"]] <- args[i]
    }
    else if (is.na(args[i])) {
      stop("new_parse_command_line(): subcommand required", call. = FALSE)
    }
    else {
      stop (paste0("new_parse_command_line(): \'", args[i], "\' is not a subcommand of parent \'", 
                   mydata$command, "\'"), call. = FALSE)
    }
    i <- i + 1
  } 
  
  # process arguments
  unk <- 0 # number of unknown params found
  while (i <= length(args)) {
    p <- args[i]
    myrow <- NULL
    index <- NULL
    has_equals <- FALSE
    
    if (is_lparam(p)) {
      if (p %in% args_table$lparam) {
        index <- which(args_table$lparam == p)
      }
      else if (strsplit(p, "=")[[1]][1] %in% args_table$lparam) {
        index <- which(args_table$lparam == strsplit(p, "=")[[1]][1])
        has_equals <- TRUE
      }
      else {
        # unrecognized argument
        unk <- unk + 1
        mydata[["unknowns"]][unk] <- p
        writeLines (paste("Warning: new_parse_command_line(): unknown param:", p))
        i <- i + 1
        next
      }
    }
    else if (is_sparam(p) && p %in% args_table$sparam) {
      index <- which(args_table$sparam == p)
    }

    else {
      # unrecognized argument
      unk <- unk + 1
      mydata[["unknowns"]][unk] <- p
      writeLines (paste("Warning: new_parse_command_line(): unknown param:", p))
      i <- i + 1
      next
    }
    
    myrow <- args_table[index,]
    
    if(myrow$argType == argsType$TypeBool) { # if the param is a logical type, save the opposite logical type
      mydata[[myrow$var]] <- !as.logical(myrow$default)
    }
    
    else if (myrow$argType == argsType$TypeCount) {
      mydata[[myrow$var]] <- ifelse(is.na(mydata[[myrow$var]]), 1, as.integer(mydata[[myrow$var]]) + 1)
    }
    
    # TypeValue, TypeMultiVal, TypeRange: store the next argument, or whatever is after the '='
    else if (myrow$argType %in% c(argsType$TypeValue, argsType$TypeMultiVal, argsType$TypeRange)) {
      if (!has_equals) {
        if (i == length(args)) { # ie, there is no args[i+1]
          stop(paste("new_parse_command_line(): Expected value missing after param:", p), call. = FALSE)
        }
        if (myrow$argType == argsType$TypeValue) {
          mydata[[myrow$var]] <- args[i+1]
        }
        # if the same arg is passed multiple times, collect all responses (ie, for keywords)
        else if (myrow$argType == argsType$TypeMultiVal) {
          idx <- ifelse(is.na(mydata[[myrow$var]][1]), 1, length(mydata[[myrow$var]])+1)
          mydata[[myrow$var]][idx] <- args[i+1]
        }
        else if (myrow$argType == argsType$TypeRange) {
          mydata[[myrow$var]] <- args[i+1]
          s <- strsplit(args[i+1], ':')[[1]]
          mydata[[paste0(myrow$var, 1)]] <- s[1]
          mydata[[paste0(myrow$var, 2)]] <- s[2]
        }
        i <- i + 1 # increment the counter to ignore the next param
      }
      else { # has_equals == TRUE
        val <- strsplit(p, "=")[[1]][2]
        if (myrow$argType == argsType$TypeValue) {
          mydata[[myrow$var]] <- val
        }
        else if (myrow$argType == argsType$TypeMultiVal) {
          idx <- ifelse(is.na(mydata[[myrow$var]][1]), 1, length(mydata[[myrow$var]])+1)
          mydata[[myrow$var]][idx] <- val
        }
        else if (myrow$argType == argsType$TypeRange) {
          mydata[[myrow$var]] <- val
          s <- strsplit(val, ':')[[1]]
          mydata[[paste0(myrow$var, 1)]] <- s[1]
          mydata[[paste0(myrow$var, 2)]] <- s[2]
        }
      }
    }
    i <- i + 1 # advance to next param
  }
  return (mydata)
} # new_parse_command_line


# HELPER FUNCTIONS
remove_dashes <- function(arg) {
  return (gsub('-', '', arg))
} # remove_dashes


is_lparam <- function(arg) {
  return (grepl('^--', arg))
} # is_lparam

is_sparam <- function(arg) {
  return(grepl('^-[^-]', arg))
}