library(plyr)


#' captionize 
#'
#' This function splits the given str with the split regex and concatenate them after capitalizing every word
#' @param str a string or a vector of strings
#' @param split regex to split the string. Defaults to ' |_|\\.'
#' @keywords capitalize caption capital
#' @export
#' @examples
#' captionize('where_is_waldo')
captionize <- function(str, split=' |_|\\.') {
	parts <- strsplit(str, split)
	parts <- lapply(parts, function(pp) sapply(pp, function(xlabel) paste(toupper(substr(xlabel, 1, 1)), substr(xlabel, 2, nchar(xlabel)), sep="")))
	unlist(lapply(parts, function(x) paste(x, collapse=' ')))
}


#' if.na 
#'
#' This function sreplaces the NA in a list with a value
#' @param data an object to be checked if it's NA
#' @param subs substitute the NA values with subs (Defauts empty string)
#' @keywords if.na ifna 
#' @export
#' @examples
#' if.na(airquality$Ozone, -1)
if.na <- function(data, subs='') {
    x <- data
	x[is.na(x)] <- subs
	x
}

#' if.null 
#'
#' This function sreplaces the NULL in a list with a value
#' @param data an object to be checked if it's NULL
#' @param subs substitute the NULL values with subs (Defauts empty string)
#' @keywords if.null ifnull null
#' @export
#' @examples
#' if.null(airquality$Ozone, -1)
if.null <- function(data, subs='') {
    x <- data
	x[is.null(x)] <- subs
	x
}


#' goodNum 
#'
#' This function creates a nice representation of a given (vector of) number.
#' @param numbers a number (float or int) or a vector of numbers
#' @param digits number of digits after fraction for float numbers. Defaults to 2.
#' @param percent (bool) show the % sign at the end of the number. Defaults to FALSE.
#' @keywords formating formatC
#' @export
#' @examples
#' goodNum(c(12, 22.1, 112112.119))
#' goodNum(c(12000, 1002000, 3454))
goodNum <- function(numbers, digits=2, percent=FALSE, ...) {
	percent_x <- ifelse(percent, '%', '')
	sapply(numbers, 
				function(x) 
						ifelse(x==round(x),
								{
									kk <- c(1000000000, 1000000, 1000, 1)
									ks <- c('B', 'M', 'k', '')
									# ki <- apply(sapply(kk, function(k) x/k == x%/%k), 1, function(ki) min(which(ki)))
									ki <- min(which(sapply(kk, function(k) x/k == x%/%k)))
									ifelse(ki==4 | x==0, 
										paste0(formatC(x, format='d', big.mark=",", ...), percent_x),
										paste0(formatC(x / kk[ki], format='d', big.mark=",", ...), ks[ki], percent_x)
									)
								}, 
								ifelse(abs(x)>1, 
									paste0(formatC(x, format='f', big.mark=',', digits = digits, drop0trailing=TRUE, ...), percent_x),
									{
										x_abs <- formatC(abs(x), format='f', big.mark=',', digits = digits, drop0trailing=TRUE, ...)
										x_abs <- substr(x_abs, 2, nchar(x_abs))
										paste0( ifelse(x<0,'-',''), x_abs, percent_x)
									}
								)

								)
			)
}



#' splitText
#'
#' This function splits the given column in the data.frame
#' @param vec a vector of char objects
#' @param sep character containing regular expression(s) (unless fixed = TRUE) to use for splitting.
#' @param part the part of the split to return, when 0 returns all. defaults to 0.
#' @param fixed logical. If TRUE match split exactly, otherwise use regular expressions. defaults to FALSE.
#' @param fill logical. fill with NA when needed.  defaults to TRUE.
#' @keywords splitCol split column
#' @export
#' @examples
#' splitText('hello2all4of222you', '\\d+')   # a vector ('hello','all', 'of', 'you')
#' splitText('hello.all.of.you', '.', fixed=TRUE)  # same as above
#' splitText('hello-all-of-you', '-')   # same as above
#' splitText('hello-all-of-you', '-', 2) # 'all'
#' splitText(c('all-2', 'any-3', 'never-4'), '-') # a data.frame with 2 columns
#' splitText(c('all-2', 'any-3', 'never-4-d'), '-') # The third column will be NA for the first two rows
#' splitText(c('all-2', 'any-3', 'never-4'), '-', 1) # a vector c('all', 'any', 'never')
splitText <- function (vec, sep, part=0, fixed = FALSE, fill=TRUE) {
    cols <- strsplit(as.character(vec), sep, fixed = fixed)
    if (fill) {
        max_l <- max(sapply(cols, length))
        cols <- lapply(cols, function(x) c(x, rep(NA, max_l - length(x))))
    }
    cols <- do.call(rbind, cols)
    
    colnames(cols) <- paste0('split.', 1:ncol(cols))
    if(part==0 & length(vec)>1)
        as.data.frame(cols)
	else if(part==0) 
		cols[1,]
    else 
        cols[,part]
}



#' splitCol 
#'
#' This function splits the given column in the data.frame
#' @param data a data frame one of its columns to be split
#' @param col_name the column whose data to be split
#' @param sep regex to split the data.
#' @param fixed regex to split the data.
#' @keywords splitCol split column
#' @export
#' @examples
#' splitCol(infert, 'education', '-')
splitCol <- function(data, col_name, sep, fixed=FALSE) {
	column <- data[,col_name]
	column <- strsplit(as.character(column), sep, fixed=fixed)
	column <- do.call(rbind, column)
	colnames(column) <- paste(col_name, 1:ncol(column), sep='.')
	as.data.frame(column)
}



#' extract_field
#'
#' This function extracts the first group of regex from a vector
#' @param data a vector of char objects
#' @param regex character containing regular expression(s) with a group in it
#' @keywords extract_field extract regular expression
#' @export
#' @examples
#' extract_field('hello_23_node', '(\\d+)')   # '23'
#' extract_field(c('all-2', 'any-3', 'never-4-d'), '(\\d+)') # a vector c('2', '3', '4')
#' extract_field(c('all-2', 'any+3', 'never+4'), '\\+(\\d+)') # a vector c(NA, '3', '4')
extract_field <- function(data, regex)  {
  matches <- regmatches(data, regexec(regex, data))
  unlist(lapply(matches, `[`, 2))
}



#' createColoredBarGraphLabels 
#'
#' This function splits the given column in the data.frame
#' @param data a data frame one of its columns to be split
#' @param col.x the column name used on x aes
#' @param col.fill the column name used for fill/color aes
#' @param col.facet the column name used for facet aes
#' @param middle create the pos to be in the middle. defaults TRUE
#' @param reverse create the pos on reverse. defaults TRUE
#' @keywords ggplot barplot 
#' @examples
#' infert$c <- factor(infert$induced)
#' texts <-  createColoredBarGraphLabels(infert, 'education', 'c')
#' p <- ggplot(infert, aes(education, fill=c)) + geom_bar(alpha=.9)
#' p     
#' p + geom_text(data=texts, aes(y=pos, label=cnt))    
#' p + geom_text(data=texts, aes(y=pos, label=goodNum(100*cnt/tot, percent=TRUE)))    
createColoredBarGraphLabels <- function (data, col.x, col.fill, col.facet=NULL, middle = TRUE, reverse = TRUE) 
{
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("data.table needed for this function to work. Please install it.", 
            call. = FALSE)
    }
	if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr needed for this function to work. Please install it.", 
            call. = FALSE)
    }
	require(data.table)
	require(dplyr)

	if(is.null(col.facet)) {
    	data.cp <- data[, c(col.x, col.fill)]
    	names(data.cp) <- c("x", "fil")
	} else {
		data.cp <- data[, c(col.x, col.facet, col.fill)]
		names(data.cp) <- c("x", "facet", "fil")
	}
    

    if (reverse) {
        fc <- data.cp$fil
        #if (typeof(fc) == "logical" || typeof) 
        if(!is.factor(fc))
            fc <- factor(fc)
        data.cp$fil <- factor(fc, levels = rev(levels(fc)))
    }

	if(is.null(col.facet)) {
		labs <- data.table(data.cp %>% group_by(x, fil) %>% summarize(cnt=length(x))) 
		labs <- labs[, list(fil=fil, cnt=cnt, tot=sum(cnt), pos=cumsum(cnt)), by=list(x)]
	} else {
		labs <- data.table(data.cp %>% group_by(x, facet, fil) %>% summarize(cnt=length(x))) 
		labs <- labs[, list(fil=fil, cnt=cnt, tot=sum(cnt), pos=cumsum(cnt)), by=list(facet,x)]
	}

	labs <- data.frame(labs)
	if (middle) 
        labs$pos <- labs$pos - labs$cnt/2
	
	labs$y <- labs$pos
	labs$label <- labs$cnt
	if(is.null(col.facet)) {
		names(labs)[1] <- col.x
		names(labs)[2] <- col.fill
	} else {
		names(labs)[1] <- col.facet
		names(labs)[2] <- col.x
		names(labs)[3] <- col.fill
	}
    labs
}

		       
		       
		       
#' commandArgs.def
#'
#' A version of commandArgs function with default values
#' @param defaults a list of defaults, NA when no default is set
#' @param error.message Show this error message and stop if not null. Defaults NULL.
#' @export
#' @examples
#' args <- commandArgs.def(c(NA, 2), error.message='This program takes two input. The second input defaults to 2 if not supplied')

commandArgs.def <- function(defaults, error.message=NULL, ...) {
	args <- commandArgs(...)
	if(length(args)<length(defaults)) {
		args <- c(args, rep(NA, length(defaults) - length(args)))
		args <- ifelse(is.na(args) & !is.na(defaults), defaults, args)
	}
	if(!is.null(error.message) & any(is.na(args)))
		stop(paste(error.message,'argument',which(is.na(args)),'not suplied.'), call.=FALSE)
	args
}

