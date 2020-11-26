# set theme
theme_set(theme_sjplot())



# function to display results ----

display <- function(num, pct=FALSE){
  if (!pct) return(sprintf("%.3f", num))
  if (pct) return(sprintf("%.1f", num*100))
}


# function to run ITT with LASSO ----
itt_lasso <- function(dv, dv_pre = NULL, covars = NULL, treat_var = NULL, treat_drop = "none", data = NULL, seed = 123, more_vars = NULL, verbose = TRUE) {
  set.seed(seed)
  
  # set up dfs
  dat_na <- na.omit(data[, c(covars, dv, dv_pre, more_vars)])
  dat_model <- model.matrix(~., dat_na[, c(covars, dv_pre, more_vars)])
  
  # run lasso
  lasso_select <- cv.glmnet(x = dat_model,
                            y = as.vector(get(dv, dat_na)),
                            alpha = 1)
  
  coef.out <- coef(lasso_select, exact = TRUE)
  inds <- which(coef.out != 0)
  
  lin_formula <- formula(paste0(dv, " ~ ", treat_var))
  if(length(inds) == 1) {
    lin_cov <- NULL
  } else {
    incl_vars <- grep("(Intercept)", rownames(coef.out)[inds], value = TRUE, invert = TRUE)
    for(v in 1:length(incl_vars)) {
      # if glmnet pulls out a single factor level from a factor var, throw in entire variable
      if(!incl_vars[v] %in% names(data)) { 
        c <- unlist(sapply(1:length(names(data)), function(x) if(grepl(names(data)[x], incl_vars[v])) x))
        incl_vars[v] <- names(data)[tail(unique(c), 1)]
      }
    }
    lin_cov <- paste0(" ~ ", paste(incl_vars, collapse = " + "))
    lin_cov <- str_replace_all(lin_cov, "TRUE", "")
  }
  # run model
  if(!is.null(lin_cov)){
    lin_model <- lm_lin(lin_formula, covariates = formula(lin_cov), data = dplyr::filter(data, treat_var != treat_drop))
  } else{
    lin_model <- lm_robust(lin_formula, data = dplyr::filter(data, treat_var != treat_drop))
  }
  sig <- ifelse(abs(lin_model$statistic[2]) >= 1.96, "STARS!", "")
  
  if(verbose){
    message("--- ITT/LASSO RESULTS ---")
    message("Estimate: ", display(coef(lin_model)[2]))
    message("Std. Error: ", display(sqrt(vcov(lin_model)[2,2])))
    message("CI Lower: ", display(confint(lin_model)[2,1]))
    message("CI Upper: ", display(confint(lin_model)[2,2]))
  }
  # only pre-treatment DV
  if (!is.null(dv_pre)){
    lin_formula <- formula(paste0(dv, " ~ ", treat_var, " + ", 
                                  dv_pre))
    ss <- summary(lm(lin_formula, data = as.data.frame(data), 
                     subset = treat_var != treat_drop)) 
    message("Pre-treatment DV, Adj R2 = ", display(ss$adj.r.squared))
    message("N = ", length(ss$residuals))
  }
  # Lasso covariates
  if(length(inds) != 1) {
  lin_formula <- formula(paste0(dv, " ~ ", treat_var, " + ", 
                                paste(incl_vars, collapse = " + ")))
  ss <- summary(lm(lin_formula, data = as.data.frame(data), 
                   subset = treat_var != treat_drop))
  message("Lasso covariates, Adj R2 = ", display(ss$adj.r.squared))
  message("Covariates: ", paste(incl_vars, collapse = " + "))
  message("N = ", length(ss$residuals))
  }
  
  # all variables
  lin_formula <- formula(paste0(dv, " ~ ", treat_var, " + ", 
                                paste(c(covars, dv_pre), collapse = " + ")))
  ss <- summary(lm(lin_formula, data = as.data.frame(data), 
                   subset = treat_var != treat_drop))
  message("All covariates, Adj R2 = ", display(ss$adj.r.squared)) 
  message("Covariates: ", paste(covars, collapse = " + "))
  message("N = ", length(ss$residuals))
  
  return(list(lin_model, sig, lin_cov))
}


# function to extract covariates from model ---
extract_covariates <- function(mod, data){
  coefs <- names(coef(mod[[1]]))
  coefs <- sapply(c(covars, dv_pre), function(x) str_detect(paste(coefs, collapse = " "), x))
  coefs_matched <- names(coefs)[coefs == TRUE]
  return(coefs_matched)
}

# function to compute CACE ----
estimate_cace <- function(Y, D, Z, X, itt, treat_drop = "none", data = NULL){
  if(length(extract_covariates(itt, data)) != 0) {
  form <- formula(paste0(Y, " ~ ", D, " + ", paste(X, collapse = " + "),
                         " | ", Z, " + ", paste(X, collapse = " + ")))
  }else{
    form <- formula(paste0(Y, " ~ ", D,
                           " | ", Z))    
  }
  dat_sub <- filter(data, !(Z %in% treat_drop))
  mod <- iv_robust(form, data = dat_sub)
  return(mod)
}


# function to compute power analysis for DIM and ITT ---
power <- function(dv, dim, itt, covariates = TRUE, data = NULL){
  message("--- POWER ANALYSIS ---")
  message("MDE without covariate adjustment:")
  pwr <- power.t.test(n = dim$nobs, 
                      sd = sd(unlist(data[dv]), na.rm=TRUE),
                      sig.level = 0.05, power=0.80)
  message(display(pwr$delta), " (",
          display(pwr$delta / sd(unlist(data[,dv]),
                                 na.rm=TRUE), pct=TRUE),
          "% of a 1-SD increase in DV)")
  if (covariates == TRUE & !is.null(itt[[3]])){
    message("MDE with covariate adjustment:")
    message("Covariates: ", itt[[3]])
    reg <- summary(lm(paste0(dv, itt[[3]]), data = data))
    ( pwr <- power.t.test(n = itt[[1]]$nobs, 
                          sd = reg$sigma,
                          sig.level = 0.05, power=0.80
    ) )
    message(display(pwr$delta), " (",
            display(pwr$delta / sd(unlist(data[,dv]),
                                   na.rm=TRUE), pct=TRUE),
            "% of a 1-SD increase in DV)")
  }
  return(c(display(pwr$delta), display(pwr$delta / sd(unlist(data[,dv]),
                                                      na.rm=TRUE))))
}



## function to run all models at once ---
run_models <- function(trt = NULL, dv = NULL, dv_pre = NULL, covars = NULL, D = NULL, data = NULL, run.cace = TRUE) {
  ## DIM
  (dim <- difference_in_means(formula(paste0(dv, " ~ ", trt)),
                              data = data))
  
  ## ITT (covariate-adjusted)
  if(!is.null(dv_pre)){
  itt <- itt_lasso(dv = dv, dv_pre = dv_pre, covars = covars, treat_var = trt, data = data)
  }else{
    itt <- itt_lasso(dv = dv, covars = covars, treat_var = trt, data = data)
  }
  compute_proportion_missing_covars(itt, data)
  
  ## CACE
  if(run.cace == TRUE){
  cace <- estimate_cace(Y = dv, D = D, Z = trt, itt = itt,
                        X = extract_covariates(itt, data),
                        data = data)
  }
  
  # Power analysis
  pwr <- power(dv, dim, itt, data = data)
  
  # output
  if(run.cace == TRUE){
  list(dim = dim, itt = itt, cace = cace, pwr = pwr)
  }else{
    list(dim = dim, itt = itt, pwr = pwr)
    
  }
}


## function to format results in latex -----
format_latex <- function(model_obj, dv_label, trt_label, path = "", add.cace = TRUE){
  if(add.cace == TRUE){
  ltx <- paste0(
    dv_label, " & ", display(model_obj$dim$coefficients), " (", display(model_obj$dim$std.error), ")",
    " & ", display(coef(model_obj$itt[[1]])[2]), " (", display(sqrt(vcov(model_obj$itt[[1]])[2,2])), ")",
    " & ", display(model_obj$cace$coefficients[2]), " (", display(model_obj$cace$std.error[2]), ")",
    " & ", model_obj$pwr[1], "\\\\ \n",
    "(", trt_label, ") & [", display(model_obj$dim$conf.low), ", ", display(model_obj$dim$conf.high), "]",
    " & [", display(confint(model_obj$itt[[1]])[2,1]), ", ", display(confint(model_obj$itt[[1]])[2,2]), "]",
    " & [", display(model_obj$cace$conf.low[2]), ", ", display(model_obj$cace$conf.high[2]), "]",
    " & ($d$=", model_obj$pwr[2], ")\\\\"
  )
  }else{
    ltx <- paste0(
      dv_label, " & ", display(model_obj$dim$coefficients), " (", display(model_obj$dim$std.error), ")",
      " & ", display(coef(model_obj$itt[[1]])[2]), " (", display(sqrt(vcov(model_obj$itt[[1]])[2,2])), ")",
      " & ", model_obj$pwr[1], "\\\\ \n",
      "(", trt_label, ") & [", display(model_obj$dim$conf.low), ", ", display(model_obj$dim$conf.high), "]",
      " & [", display(confint(model_obj$itt[[1]])[2,1]), ", ", display(confint(model_obj$itt[[1]])[2,2]), "]",
      " & ($d$=", model_obj$pwr[2], ")\\\\"
    )    
  }
  if(path == "") {
    message(gsub('_', '\\\\_', ltx))
  }else{
    write(ltx, file = path)
  }
}

## function to compute proportion of observations with missing covariates ---
# to determine whether we need to run multiple imputation, compute the
# % of observations with at least one missing value in the covariates.
# Note that missing values in DV are not included in the estimation.
# If % of missing values is above 20%, then we will do multiple imputation.
compute_proportion_missing_covars <- function(mod, data = NULL, more_info=FALSE){
  # mod = output from run_model()
  
  # identify labels for covariates and dv
  covars <- attr(mod[[1]]$terms, "term.labels")
  dv <- mod[[1]]$outcome
  
  # extract relevant variables from df
  dd <- data[,c(dv, covars)]
  
  # identify rows where DV is not missing
  no_miss_dv <- which(!is.na(dd[,dv]))
  
  # identify rows where at least one covariate has a missing value
  miss_covars <- apply(as.data.frame(data[no_miss_dv,covars]), 1, function(x) any(is.na(x)))
  
  # print % of missing values
  message("--- OBSERVATIONS MISSING DUE TO LIST-WISE DELETION ---")
  
  message(display(mean(miss_covars), pct=TRUE), "% missing")
  
  if (more_info){
    print(summary(data[no_miss_dv,covars]))
  }
  
  return(invisible(mean(miss_covars)))
}


## function to compute heterogeneous ITT effects -----------
heterogeneous_itt <- function(dv, dv_pre = NULL, covars = NULL, trt, moderator, data, verbose = TRUE){
  # compute standard lasso itt
  itt <- itt_lasso(dv = dv, dv_pre = dv_pre, covars = covars, treat_var = trt, data = data, verbose = verbose)
  vars <- unique(c(extract_covariates(itt), moderator))
  # now compute itt but adding moderator (if it wasn't included)
  lin_formula <- formula(paste0(dv, " ~ ", trt))
  lin_covars <- formula(paste0(" ~ ", paste(vars, collapse = " + ")))
  lin_model <- lm_lin(lin_formula, covariates = lin_covars, data = data)
  # extract t stat for interaction
  interaction_t <- lin_model$statistic[grep(paste0(":", moderator), names(lin_model$statistic))]
  sig <- ifelse(
    interaction_t > 1.96, "+",
    ifelse(interaction_t < (-1.96), "-", "n.s.")
  )
  return(list(lin_model, sig))
}


### function to extract p values -------------
t_test_out <- function(variable, groupvar, data, ...){
  out <- t.test(data[,variable][data[,groupvar] == 1],
                data[,variable][data[,groupvar] == 0],
                ...)
  out$p.value
}


### function to crosstab nicely ----
crosstab <- function (..., dec.places = NULL,
                      type = NULL,
                      style = "wide",
                      row.vars = NULL,
                      col.vars = NULL,
                      percentages = TRUE, 
                      addmargins = TRUE,
                      subtotals=TRUE)
  
  # documented at https://rpubs.com/PaulWilliamson/6975
  
  ###################################################################################
#                                                                                 #
# Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
# School of Environmental Sciences, University of Liverpool, UK.                  #
#                                                                                 #
# Adapted from the function ctab() in the catspec packge.                         #
#                                                                                 #
# Version: 12th July 2013                                                         #
#                                                                                 #
# Output best viewed using the companion function print.crosstab()                #
#                                                                                 #
###################################################################################


#Declare function used to convert frequency counts into relevant type of proportion or percentage
{
  mk.pcnt.tbl <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars), 
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percentages) {
      tbl <- tbl * 100
    }
    tbl
  }
  
  #Find no. of vars (all; row; col) for use in subsequent code
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  
  #Check to make sure all user-supplied arguments have valid values
  stopifnot(as.integer(dec.places) == dec.places, dec.places > -1)
  #type: see next section of code
  stopifnot(is.character(style))    
  stopifnot(is.logical(percentages))
  stopifnot(is.logical(addmargins))
  stopifnot(is.logical(subtotals))
  stopifnot(n.vars>=1)
  
  #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
  #If invalid type supplied, failed match gives user automatic error message
  types <- NULL
  choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
  for (tp in type) types <- c(types, match.arg(tp, choices))
  type <- types
  
  #If no type supplied, default to 'frequency + total' for univariate tables and to
  #'frequency' for multi-dimenstional tables
  
  #For univariate table....
  if (n.vars == 1) {
    if (is.null(type)) {
      # default = freq count + total.pct  
      type <- c("frequency", "total.pct")
      #row.vars <- 1
    } else {
      #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
      type <- ifelse(type == "frequency", "frequency", "total.pct")
    }
    #For multivariate tables...
  } else if (is.null(type)) {
    # default = frequency count  
    type <- "frequency"
  }
  
  
  
  #Check for integrity of requested analysis and adjust values of function arguments as required
  
  if ((addmargins==FALSE) & (subtotals==FALSE)) {
    warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (addmargins=FALSE)")
    subtotals <- TRUE
  }
  
  if ((n.vars>1) & (length(type)>1) & (addmargins==TRUE)) {
    warning("WARNING: Only row totals added when more than one table type requested")
    #Code lower down selecting type of margin implements this...
  }
  
  if ((length(type)>1) & (subtotals==FALSE)) { 
    warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
    subtotals <- TRUE
  }
  
  if ((length(type)==1) & (subtotals==FALSE)) {
    choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
    tp <- match.arg(type, choices)
    if (tp %in% c("row.pct","column.pct","joint.pct")) {
      warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
      subtotals<- TRUE
    }
  }
  
  if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE)) 
    warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")
  
  
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    #If subtotals not required AND total table vars > 2
    #Reassign all but last col.var as row vars
    #[because, for simplicity, crosstabs assumes removal of subtotals uses tables with only ONE col var]
    #N.B. Subtotals only present in tables with > 2 cross-classified vars...
    if (length(col.vars)>1) {
      row.vars <- c(row.vars,col.vars[-length(col.vars)])
      col.vars <- col.vars[length(col.vars)]
      n.row.vars <- length(row.vars)
      n.col.vars <- 1
    }
  }
  
  #If dec.places not set by user, set to 2 unlesss only one table of type frequency requested,
  #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
  if (is.null(dec.places)) {
    if ((length(type)==1) & (type[1]=="frequency")) {
      dec.places <- 0
    } else {
      dec.places <-2
    }
  }
  
  #Take the original input data, whatever form originally supplied in,
  #convert into table format using requested row and col vars, and save as 'tbl'
  
  args <- list(...)    
  
  if (length(args) > 1) {
    if (!all(sapply(args, is.factor))) 
      stop("If more than one argument is passed then all must be factors")
    tbl <- table(...)
  }
  else {
    if (is.factor(...)) {
      tbl <- table(...)
    }
    else if (is.table(...)) {
      tbl <- eval(...)
    }
    else if (is.data.frame(...)) {
      #tbl <- table(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        tbl <- table(...)
      }
      else {
        var.names <- c(row.vars,col.vars)
        A <- (...)
        tbl <- table(A[var.names])
        if(length(var.names==1)) names(dimnames(tbl)) <- var.names
        #[table() only autocompletes dimnames for multivariate crosstabs of dataframes]
      }
    }
    else if (class(...) == "ftable") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- names(attr(tbl, "row.vars"))
        col.vars <- names(attr(tbl, "col.vars"))
      }
      tbl <- as.table(tbl)
    }
    else if (class(...) == "ctab") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- tbl$row.vars
        col.vars <- tbl$col.vars
      }
      for (opt in c("dec.places", "type", "style", "percentages", 
                    "addmargins", "subtotals")) if (is.null(get(opt))) 
                      assign(opt, eval(parse(text = paste("tbl$", opt, 
                                                          sep = ""))))
      tbl <- tbl$table
    }
    else {
      stop("first argument must be either factors or a table object")
    }
  }
  
  #Convert supplied table style into full text string (e.g. "l" becomes "long")
  style <- match.arg(style, c("long", "wide"))
  
  #Extract row and col names to be used in creating 'tbl' from supplied input data
  nms <- names(dimnames(tbl))
  z <- length(nms)
  if (!is.null(row.vars) && !is.numeric(row.vars)) {
    row.vars <- order(match(nms, row.vars), na.last = NA)
  }
  if (!is.null(col.vars) && !is.numeric(col.vars)) {
    col.vars <- order(match(nms, col.vars), na.last = NA)
  }
  if (!is.null(row.vars) && is.null(col.vars)) {
    col.vars <- (1:z)[-row.vars]
  }
  if (!is.null(col.vars) && is.null(row.vars)) {
    row.vars <- (1:z)[-col.vars]
  }
  if (is.null(row.vars) && is.null(col.vars)) {
    col.vars <- z
    row.vars <- (1:z)[-col.vars]
  }
  
  #Take the original input data, converted into table format using supplied row and col vars (tbl)
  #and create a second version (crosstab) which stores results as percentages if a percentage table type is requested.
  if (type[1] == "frequency") 
    crosstab <- tbl
  else 
    crosstab <- mk.pcnt.tbl(tbl, type[1])
  
  
  #If multiple table types requested, create and add these to 
  if (length(type) > 1) {
    tbldat <- as.data.frame.table(crosstab)
    z <- length(names(tbldat)) + 1
    tbldat[z] <- 1
    pcntlab <- type
    pcntlab[match("frequency", type)] <- "Count"
    pcntlab[match("row.pct", type)] <- "Row %"
    pcntlab[match("column.pct", type)] <- "Column %"
    pcntlab[match("joint.pct", type)] <- "Joint %"
    pcntlab[match("total.pct", type)] <- "Total %"
    for (i in 2:length(type)) {
      if (type[i] == "frequency") 
        crosstab <- tbl
      else crosstab <- mk.pcnt.tbl(tbl, type[i])
      crosstab <- as.data.frame.table(crosstab)
      crosstab[z] <- i
      tbldat <- rbind(tbldat, crosstab)
    }
    tbldat[[z]] <- as.factor(tbldat[[z]])
    levels(tbldat[[z]]) <- pcntlab
    crosstab <- xtabs(Freq ~ ., data = tbldat)
    names(dimnames(crosstab))[z - 1] <- ""
  }
  
  
  #Add margins if required, adding only those margins appropriate to user request
  if (addmargins==TRUE) {
    
    vars <- c(row.vars,col.vars)
    
    if (length(type)==1) {
      if (type=="row.pct") 
      { crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
      }
      else 
      { if (type=="column.pct") 
      { crosstab <- addmargins(crosstab,margin=c(vars[n.row.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
      }
        else 
        { if (type=="joint.pct") 
        { crosstab <- addmargins(crosstab,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        }
          else #must be total.pct OR frequency
          { crosstab <- addmargins(crosstab)
          tbl <- addmargins(tbl)
          }
        }
      } 
    }
    
    #If more than one table type requested, only adding row totals makes any sense...
    if (length(type)>1) {
      crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
    }
    
  }  
  
  
  #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
  #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
  t1 <- NULL
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    
    #Create version of crosstab in ftable format
    t1 <- crosstab 
    t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)
    
    #Convert to a dataframe
    t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)
    
    #Remove backslashes from category names AND colnames
    t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
    #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
    #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
    t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))
    
    #Reshape dataframe to that variable and category labels display as required
    #(a) Move col category names down one row; and move col variable name one column to right
    t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
    t1[1,] <- ""
    t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]    
    #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
    t1 <- t1[,-(n.row.vars+1)]
    
    #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals) 
    lab <- t1[,1:n.row.vars]
    for (c in 1:n.row.vars) {
      for (r in 2:nrow(lab)) {
        if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]  
      }
    }
    
    lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
    lab <- apply(lab,1,sum)
    #Filter out rows of dataframe containing subtotals
    
    t1 <- t1[((lab==0) | (lab==n.row.vars)),]
    
    #Move the 'Sum' label associated with last row to the first column; in the process
    #setting the final row labels associated with other row variables to ""
    t1[nrow(t1),1] <- "Sum"
    t1[nrow(t1),(2:n.row.vars)] <- ""
    
    #set row and column names to NULL
    rownames(t1) <- NULL
    colnames(t1) <- NULL
    
  }
  
  
  
  #Create output object 'result' [class: crosstab]
  result <- NULL
  #(a) record of argument values used to produce tabular output
  result$row.vars <- row.vars
  result$col.vars <- col.vars
  result$dec.places <- dec.places
  result$type <- type
  result$style <- style
  result$percentages <- percentages
  result$addmargins <- addmargins
  result$subtotals <- subtotals
  
  #(b) tabular output [3 variants]
  result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
  result$crosstab <- crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
  #[class: table]  
  result$crosstab.nosub <- t1  #crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]  
  class(result) <- "crosstab"    
  
  #Return 'result' as output of function
  result
  
}



print.crosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {
  
  ###################################################################################
  #                                                                                 #
  # Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
  # School of Environmental Sciences, University of Liverpool, UK.                  #
  #                                                                                 #
  # Adapted from the function print.ctab() in the catspec packge.                   #
  #                                                                                 #
  # Version: 12th July 2013                                                         #
  #                                                                                 #
  # Designed to provide optimal viewing of the output from crosstab()               #
  #                                                                                 #
  ###################################################################################
  
  row.vars <- x$row.vars
  col.vars <- x$col.vars
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  if (length(x$type)>1) {
    z<-length(names(dimnames(x$crosstab)))
    if (x$style=="long") {
      row.vars<-c(row.vars,z) 
    } else {
      col.vars<-c(z,col.vars)
    }
  }
  
  if (n.vars==1) {
    if (length(x$type)==1) {
      tmp <- data.frame(round(x$crosstab,x$dec.places))
      colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
      print(tmp,row.names=FALSE)
    } else {
      print(round(x$crosstab,x$dec.places))
    }
  }
  
  
  #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
  #print table using ftable() on x$crosstab
  if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {
    
    tbl <- ftable(x$crosstab,row.vars=row.vars,col.vars=col.vars)
    
    if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
    print(tbl,...)
    
  }
  
  #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$crosstab.nosub
  if ((subtotals==FALSE) & (n.vars>2))  {
    
    t1 <- x$crosstab.nosub
    
    #Convert numbers to required decimal places, right aligned
    width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
    dec.places <- x$dec.places
    number.format <- paste("%",width,".",dec.places,"f",sep="")
    t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))
    
    #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
    col.var.format <- paste("%-",width,"s",sep="")
    t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
    #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
    col.cat.format <- paste("%",width,"s",sep="")
    t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])
    
    #Adjust row labels so that each column is of fixed width, using trailing spaces as required
    for (i in 1:n.row.vars) {
      width <- max(nchar(t1[,i])) + 2
      row.lab.format <- paste("%-",width,"s",sep="")
      t1[,i] <- sprintf(row.lab.format,t1[,i])
    }
    
    write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)
    
  }
  
}



