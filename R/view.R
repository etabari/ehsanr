
#' alternate
#'
#' src is better to be a factor
#' alternale 2 colors: base, opt
#' @export
alternate <- function(src, subsrc, def.opt, subsrc.opt, drop=TRUE) {
	if (!is.factor(src)) src <- factor(src)                   # make sure it's a factor
	src_levels <- levels(src)                                 # retrieve the levels in their order
	if (drop)
		src_levels <- src_levels[src_levels %in% src]

	b_vec <- rep(def.opt, length(src_levels))               # make'm all color1
	b_vec[src_levels %in% subsrc] <- subsrc.opt                              # make our targets color2
	b_vec                                                   # return the new vector
}


#' alternate2
#'
#' src is better to be a factor
#' alternale 3 colors: base, opt2, opt3
#' @export
alternate2 <- function(src, subsrc1, subsrc2, def.opt, subsrc1.opt, subsrc2.opt, drop=TRUE) {
	if (!is.factor(src)) src <- factor(src)
	src_levels <- levels(src)
	if (drop)
		src_levels <- src_levels[src_levels %in% src]
	
	b_vec <- rep(def.opt, length(src_levels))               # make'm all color1
	b_vec[src_levels %in% subsrc1] <- subsrc1.opt                              # make our targets color2
	b_vec[src_levels %in% subsrc2] <- subsrc2.opt                              # make our targets color2
	b_vec                                                   # return the new vector
}


#' bolder
#'
#' alternale font style bold/plain
#' @export
bolder <- function(src, subsrc) {
 alternate(src, subsrc, 'plain', 'bold')
}

#' geom_text_y
#'
#' Simpler version of createColoredBarGraphLabels
#' @export
#' @examples
#' ggplot(infert, aes(education, fill=factor(induced))) + geom_bar() + geom_text(stat='count', aes(label=..count.., y=geom_text_y(..count.., ..x..)))
geom_text_y <- function(cnt, x, mid=TRUE) {unlist(lapply(split(cnt, x), function(a) rev(cumsum(rev(a))) - mid * a/2))}

#' geom_text_percent
#'
#' 
#' @export
#' @examples
#' ggplot(infert, aes(education, fill=factor(induced))) + geom_bar() + geom_text_percent(stat='count', aes(label=geom_text_percent(..count.., ..x..), y=0))
geom_text_percent <- function(cnt, x, mid=TRUE) {unlist(lapply(split(cnt, x), function(a) a/sum(a)))}
