

exp.replace <- function(expression.in){
    alias <- expression.in
    expression.out <- expression.in
    
    # test for percent-based text
    expression.out <- sub(pattern='%', replacement='', x=expression.out)
    expression.out <- sub(pattern='missing', replacement='is.na', x=expression.out)
    
    if (grepl(pattern='==',x=expression.out)){
        return(list(expression=expression.out,alias=alias))
    } else {
        expression.out <- sub(pattern='=', replacement='==', x=expression.out)
        return(list(expression=expression.out,alias=alias))
    }
    
}

date.replace <- function(date.form){
    alias.date <- list("YYYY-mm-dd HH:MM" = "%Y-%m-%d %H:%M",
    "mm/dd/YYYY HH:MM" = "%m/%d/%Y %H:%M")
    
    if (!date.form %in% names(alias.date)){
        stop(paste('date format ',date.form,' not supported'))
    }
    u.i <- which(grepl(date.form, names(alias.date)))
    return(alias.date[[u.i]])
}