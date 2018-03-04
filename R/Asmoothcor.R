#' @import RColorBrewer
#' @export
smoothhydropairs <- function(x, dec=3, use="pairwise.complete.obs", method="pearson",... ) {

  # Checking that the user provied a valid argument for 'x'
  if ( is.na( match( class(x), c("matrix", "data.frame") ) ) )
    stop("Invalid argument: 'class(x)' must be in c('data.frame')")

  panel.cor <- function(x, y, digits=dec, prefix="", cex.cor)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))

    r <- abs(cor(x, y, method= method, use= use))

    txt <- format(c(r, 0.123456789), digits=dec)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)

    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                     cutpoints = c(0, 0.01, 1),
                     symbols = c("*",  " "))

    text(0.5, 0.5, txt, cex = cex.cor)
    text(.8, .8, Signif, cex=cex.cor*2, col=2)

  } # 'panel.cor' END

  panel.hist <- function(x, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
  } # 'panel.hist' END
  panel.density1<- function(x,y,...){
    smoothScatter(x,y,colramp = colorRampPalette(rev(brewer.pal(11, "Spectral")))
                  ,nrpoints = 0,add=TRUE)
 }

  pairs(x, lower.panel=panel.density1, upper.panel=panel.cor,
        diag.panel=panel.hist, ...)
  # 'font.labels' =2 : bold font for the variables
  # 'cex.labels' controls the size of the fonts


} # 'hydropairs' END
