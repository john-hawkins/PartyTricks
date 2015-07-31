#
# PartyTricks -- A small library of functions for manipulating and 
#                printing the trees produced by the 'party' package.
#                These are designed to be used for rule mining.
# John Hawkins 2015
#

library("party")


# ##########################################################
# Print out all of the probabilities of the terminal nodes
# ##########################################################
print_ctree_terminal_predictions <- function(x, res=NULL) {
  if (!x$terminal) {
     print_ctree_terminal_predictions(x$left)
     print_ctree_terminal_predictions(x$right)
  } else {
    cat( ", weights=", sum(x$weights), ", pred=", x$prediction, "\n", sep="")
  }
  invisible(NULL)
}

# ######################################################
# Print out the tree as a set of rules
# ######################################################
print_ctree_rule_list_csv <- function(x, rule_map=list() ) {
  if (!x$terminal) {

    if( length(x$psplit$splitpoint) > 1 ) {
       levelsL <- attr(x$psplit$splitpoint, "levels") [which(!x$psplit$splitpoint==0)]
       levelsLString <- paste(levelsL, collapse="||")
       levelsR <- attr(x$psplit$splitpoint, "levels") [which(x$psplit$splitpoint==0)]
       levelsRString <- paste(levelsR, collapse="||")

   	## LEFT branch
    	rule_map_L <- add_rule_to_map(rule_map,'FACTOR', x$psplit$variableName, levelsLString)

    	## RIGHT branch
    	rule_map_R <- add_rule_to_map(rule_map,'FACTOR', x$psplit$variableName, levelsRString)
    } else {
       levelsLString <- paste ( "x <= ", x$psplit$splitpoint )
       levelsRString <- paste ( x$psplit$splitpoint,  " < x" )
        ## LEFT branch
        rule_map_L <- add_rule_to_map(rule_map,'NUMERIC', x$psplit$variableName, levelsLString)

        ## RIGHT branch
        rule_map_R <- add_rule_to_map(rule_map,'NUMERIC', x$psplit$variableName, levelsRString)
    }

    print_ctree_rule_list_csv(x$left, rule_map_L)
    print_ctree_rule_list_csv(x$right, rule_map_R)

  } else {
    criteria <- ""
    for( key in names(rule_map) ) { criteria <- paste(criteria, " ", key, "={", rule_map[[key]], "}") }
    cat( sum(x$weights), ",", x$prediction, ",\"", criteria, "\"\n")
  }
  invisible(NULL)
}


# ######################################################
# Add a Rule -- If a previous rule for the same variable
# name exists then it is replaced with the minimal common 
# subset.
# ######################################################
add_rule_to_map <- function (rule_map, ruleType, key, values ) {
   return_map <- rule_map
   if( key %in% names(rule_map) ) { 
	if(ruleType=='NUMERIC') {
		return_map[[key]] <- modifyRange(rule_map[[key]], values )
	} else {
		return_map[[key]] <- getCommonSubset(rule_map[[key]], values )
	}
   } else { 
      return_map[[key]] <- values
   }
   return_map
}

# ########################################################
# Return string w/o leading or trailing whitespace 
# ########################################################
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# ########################################################
# Take two factor sets and return common subset
# ########################################################
getCommonSubset <- function( setOne, setTwo ) {
   setOneArray <- lapply( strsplit(setOne, split="\\|\\|"), trim)
   setTwoArray <- lapply( strsplit(setTwo, split="\\|\\|"), trim)
   paste( intersect( setOneArray[[1]], setTwoArray[[1]] ),  collapse="||")  
}

# ########################################################
# Take numeric range rules and return their intersection
# ########################################################
modifyRange <- function( ruleOne, ruleTwo ) {
	lowerBound = "?"
	if( grepl("< x", ruleOne ) ) {
		lowerBound <- strsplit(ruleOne, split= " < x")[[1]][1]
	}
        if( grepl("< x", ruleTwo ) ) {
                temp <- strsplit(ruleTwo, split= " < x")[[1]][1]
		if(lowerBound == "?") lowerBound <- temp
		else if(lowerBound < temp) lowerBound <- temp
        }	
        upperBound = "?"
        if( grepl("x <=", ruleOne ) ) {
                upperBound <- strsplit(ruleOne, split= "x <= ")[[1]][2]
        }
        if( grepl("x <=", ruleTwo ) ) {
                temp <- strsplit(ruleTwo, split= "x <=")[[1]][2]
                if(upperBound == "?") upperBound <- temp
                else if(upperBound > temp) upperBound <- temp
        }
	resultRule = "x"
	if(lowerBound != "?") {
		resultRule = paste( trim(lowerBound), " < x")
	}
	if(upperBound != "?") {
		resultRule = paste(resultRule, " <= ", trim(upperBound) )
	}
	resultRule
}


