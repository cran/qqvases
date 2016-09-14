###
###  Several global variables are used to avoid copying matrices and features
###  These are held in a package environment
###
###   state            		state of display to decide if new plot
###   the.data				sample if simulated
###   left.vase, right.vase	info for drawing side-by-side
###   x.vase, y.vase		quantile plots
###	  .prob.grid			standard percentiles for grid

#' qqvases: A package for illustrating the construction of QQ plots.
#'
#' This package displays an animation of a normal QQ plot. The animation links points in the
#' plot to 'water levels' in vases whose shape is defined by probability distributions.
#' Probability distributions placed along the plot axes gradually fill (become shaded)
#' as a slider locates larger quantiles of the probability distributions.  The reference
#' distribution along the x-axis is the normal distribution. 
#' See the function qq_vase_plot for further information.
#'
#' @docType package
#' @name qqvases
NULL

pkg.env <- new.env()

pkg.env$prob.grid <- c(0.0005, seq(.001,.999,.002), 0.9995)

#' Interactive QQ Vase Plot
#'
#' This function displays animated normal quantile-quantile plots.  The software
#' uses Shiny to open a window in the default installed web browser. Graphical
#' controls in that window manipulate the construction of normal quantile plots. 
#' The display shows QQ plots for comparing either population distributions or 
#' samples of a chosen size. When samples are shown, the plot includes approximate
#' 95% confidence bands.
#'
#' @param draw.samples Set to true to allow optional samples from distributions
#' @param dists Named list of (d=density, q=quantile, r=sample) distributions (see make_df_list())
#' @param step Stepsize used for animated slider (use larger values if animating with play button)
#' @param breaks Passed to R histogram program to control number of bins in empirical distribution
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # default
#' qq_vase_plot()
#'
#' # suppress sampling option
#' qq_vase_plot(FALSE)
#'
#' # add uniform distribution to dialog
#' dists <- make_df_list()
#' dists[["uniform"]] <- list(d=dunif, q=qunif, r=runif)
#' qq_vase_plot(TRUE, dists)
#'
#' # smaller increments, change the method that determines breaks in histogram
#' qq_vase_plot(TRUE, step=0.01, breaks=function(x) 2*sqrt(length(x)))
#' }
#' @export

qq_vase_plot <- function(draw.samples=TRUE, dists=make_df_list(), step=0.05, breaks="FD") {
	cat("Type an escape at the R prompt when you are finished with the interactive page.\n")
	if(is.function(dists)) df.list <- dists() else df.list <- dists
	cat("Names of possible distributions are:", names(df.list),"\n")
	make.axes.vases("Normal", FALSE, df.list=df.list);
	hlp <- shiny::helpText("Compare the levels as the distributions fill.");
	si.pct <- shiny::sliderInput("pct", label="Percentile", min=0, max=1, value=.25, step=step, animate=T)
	si.dst <- shiny::selectInput("dist", label="Distribution", choices=names(df.list))
	sb <- if(draw.samples) {
		default.n <- 50
		shiny::sidebarPanel(
			hlp, si.pct, si.dst,
			shiny::checkboxInput("emp", "Sample distribution?", FALSE),
			shiny::numericInput("n", "Sample size", default.n, min=25, max=1000,step=50))
		} else shiny::sidebarPanel(hlp, si.pct, si.dst)
	app <- shiny::shinyApp(
		ui = shiny::fluidPage(
			theme = shinythemes::shinytheme("cosmo"),
			shiny::titlePanel("Normal Quantile Plot"),
			shiny::sidebarLayout(sb, shiny::mainPanel(shiny::plotOutput("vases")))  ),
    	server = function(input, output) {
    		output$vases <- shiny::renderPlot({
				par(mfrow=c(1,2))
				show.quantile.plot(input$pct,input$dist,df.list,empirical=input$emp,n=input$n,breaks=breaks)
				draw.side.by.side(input$pct)
			})  })
	shiny::runApp(app)
}

qq.vase.plot <- function(...) {
	cat("Please use the renamed function qq_vase_plot instead.\n")
	}


#' Interactive QQ Plot
#'
#' This function uses Shiny to open the default web browser. Graphical controls
#' manipulate the construction of an interactive normal QQ plot.
#'
#' @param data Numerical data vector.
#' @param step Stepsize used for Shiny slider (use larger values if animating with play button)
#' @param breaks Passed to histogram for empirical distribution (default = "FD")
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' qq_plot(rnorm(50))
#'
#' qq_plot(rnorm(50), step=0.05, breaks="Sturges")
#'
#' if(require("MASS", quietly=TRUE)){ qq_plot(geyser$waiting) }
#' }
#' @export

qq_plot <- function(data, step=0.05, breaks="FD") {
	cat("Type an escape at the R prompt when you are finished with the interactive page.\n")
	df.list <- list("Normal"=make.normal.dist())
	make.axes.vases(data, TRUE, df.list=df.list);
	hlp <- shiny::helpText("Compare the levels as the distributions fill.");
	si.pct <- shiny::sliderInput("pct", label="Percentile", min=0, max=1, value=.25, step=step, animate=T)
	sb <- shiny::sidebarPanel(hlp, si.pct)
	app <- shiny::shinyApp(
		ui = shiny::fluidPage(
			theme = shinythemes::shinytheme("cosmo"),
			shiny::titlePanel("Normal Quantile Plot"),
			shiny::sidebarLayout(sb, shiny::mainPanel(shiny::plotOutput("vases")))  ),
    	server = function(input, output) {
    		output$vases <- shiny::renderPlot({
				par(mfrow=c(1,2))
				show.quantile.plot(input$pct,data,df.list,n=length(data),breaks=breaks)
				draw.side.by.side(input$pct)
			})  })
	shiny::runApp(app)
}

qq.plot <- function (...) {
	cat("Please use the renamed function qq_plot.\n");
	}
	
#		pct   percentiles (e.g. 0.01, 0.02, ... 0.99)
#		dist  name (e.g. 'Normal') or data
#		emp   simulates data if true using input dist
#		n	  sample size if empirical is used (rather than overload emp)

#' @importFrom graphics plot.new
#' @importFrom graphics par
#' @importFrom graphics plot.window
show.quantile.plot <- function(pct, dist, df.list, empirical=FALSE, n=0, breaks="FD") {
	mar <- 0.15; inset <- 0.05; top <- 0.98
	if (is.null(empirical)) empirical <- FALSE
	if (is.numeric(dist))   empirical <- TRUE
	if (is.null(n))         n <- 0
	if (!states.match(dist,empirical,n)) { # rebuild vases
		dq <- make.axes.vases(dist, empirical, n=n, mar, inset, top, df.list=df.list, breaks=breaks)
		make.left.right.vases(pkg.env$prob.grid, dq$d, dq$q, df.list=df.list, hist=empirical, breaks=breaks);   }
	plot.new();
	par(mar=c(0,0,0,0),cex.axis=0.75,mgp=c(0,0.3,0),tcl=-0.25);
	plot.window(xlim = 0:1, ylim = 0:1, xaxs = "i", yaxs = "i",asp=1);
	if (empirical) 	draw.empirical.qq(pct, mar, inset)
	else			draw.population.qq(pct, mar, inset)
	draw.axis(-4:4, 1, mar+inset,top)
	draw.axis(pretty(pkg.env$y.vase$quantile()), 2, mar+inset,top)
}

#' @importFrom graphics lines
draw.population.qq <- function(pct, mar, inset) {
	pkg.env$y.vase$draw(pct)
	last <- pkg.env$x.vase$draw(pct)
	x <- pkg.env$x.vase$grid.x()[,1];
	y <- pkg.env$y.vase$grid.y()[,1];
	lines(x[1:last],y[1:last],lwd=3);						# selected black curve
	last <- min(last+1,length(x))							# extend to rest of points
	lines(x[last:length(x)],y[last:length(y)],col='lightgray', lwd=1)
	lines(c(mar+inset,1),c(mar+inset,1),lty=3,col='gray')	# dashed gray diagonal line
}

#' @importFrom graphics points
draw.empirical.qq <- function(pct, mar, inset) {
	pkg.env$y.vase$draw(pct)
	pkg.env$x.vase$draw(pct)
	y <- pkg.env$y.vase$map(pkg.env$the.data)
	n <- length(y)
	x <- pkg.env$x.vase$map(qnorm(1:n/(n+1)))
	pch <-rep(1,n); color <- rep('gray',n)
	pch  [1:max(1,floor(pct*n))] <- 19
	color[1:max(1,floor(pct*n))] <- 'black'
	color[which_data_outside_ks()] <- 'red'
	points(x,y,cex=0.75,pch=pch,col=color)
	draw.ks.bounds(mar+inset)
	draw.data.points(mar)
	draw.reference.line(mar+inset, 'lightgray', lty=3)
}

#' @importFrom graphics axis
#' @importFrom graphics lines
draw.axis <- function(labels, id, pos, top){
	coord <- list(x=c(pos,top), y=c(pos,pos)) # complete axis outside major tics
	if(id == 1) vase <- pkg.env$x.vase
	else		vase <- pkg.env$y.vase
	at <- vase$map(labels);
	keep <- (pos < at) & (at < 1)
	axis(id, pos=pos, at=at[keep], labels=labels[keep])
	if(id == 1) lines(coord)
	else 		lines(coord$y,coord$x)
}

#' @importFrom stats median
#' @importFrom stats mad
#' @importFrom graphics lines
draw.reference.line <- function(clipAt, color, lty=2, lwd=1) {
	n <- length(pkg.env$the.data)
	a <- median(pkg.env$the.data)
	b <- mad(pkg.env$the.data)
	x <- qnorm(c(1,n)/(n+1))
	y <- a + b*x
	x <- pkg.env$x.vase$map(x)
	y <- pkg.env$y.vase$map(y)
	if(y[1]<clipAt) {
		x[1] <- x[1] + (clipAt-y[1])*diff(x)/diff(y)
		y[1] <- clipAt
	}
	lines(x, y, col=color, lty=lty, lwd=lwd)
}

make.axes.vases <- function (dist, emp, n=0, mar=0.15, inset=0.05, top=0.98, df.list=NULL, breaks="FD") {
	if (is.null(emp)) stop("emp is null");
	if(emp || is.numeric(dist)) {		# show hist of data rather than smooth curve
		if (is.numeric(dist)) {			# user supplied data
			emp <- TRUE
			n <- length(dist)
			assign("the.data",sort(dist), envir=pkg.env)
		} else {						# simulate from dist
			dqr <- df.list[[dist]]
			assign("the.data",sort(dqr$r(n)), envir=pkg.env)
		}
		dq <- num.density.and.quantile(pkg.env$the.data, breaks=breaks) }     # at pkg.env$prob.grid
	else {
		assign("the.data",NULL,envir=pkg.env)
		dq <- num.density.and.quantile(dist, df.list=df.list) }
	size <- 1.9 * mar  					# scale for density so half-of-vase fills to margin
	ctr  <- size/2     					# location of density baseline
	assign("y.vase", make.vase(0, mar+inset, size, top, dq$d, dq$q, half=T, hor=F, hist=emp), envir=pkg.env)
	ndq <- num.density.and.quantile("Normal",df.list=df.list)
	assign("x.vase", make.vase(mar+inset, 0, top, size, ndq$d, ndq$q, half=T, hor=T), envir=pkg.env)
	ratio <- pkg.env$y.vase$grid.area()/pkg.env$x.vase$grid.area()
	if(ratio < 1) {   					# shrink larger vase
		size <- size*ratio
		assign("x.vase",
			make.vase(mar+inset, ctr-size/2, top, ctr+size/2, ndq$d, ndq$q, half=T, hor=T),
			envir=pkg.env)
	} else {
		size <- size/ratio
		assign("y.vase",
			make.vase(ctr-size/2,mar+inset,ctr+size/2,top,dq$d,dq$q,half=T,hor=F,hist=emp),
			envir=pkg.env)
	}
	return(dq)	# information for y vase
}

#' @importFrom stats qnorm
#' @importFrom stats sd
#' @importFrom graphics lines
draw.ks.bounds <- function(bound) {
	n <- length(pkg.env$the.data)
	limit <- 0.886/sqrt(n)  # lillifors 67
	m <- mean(pkg.env$the.data); s <- sd(pkg.env$the.data)
	z <- qnorm(p <- seq(0.001,0.999,0.001))
	ks.hi <- m+s*qnorm(pmin(0.9999,p+limit))
	ks.lo <- m+s*qnorm(pmax(0.0001,p-limit))
	ks.hi[1-1/n<(p+limit)] <- NA
	ks.lo[(p-limit)<1/n] <- NA
	x <- pkg.env$x.vase$map(z)
	y <- pkg.env$y.vase$map(ks.lo)
	y[y<bound] <- NA
	lines(x, y, col='gray', lty=1)
	y <- pkg.env$y.vase$map(ks.hi)
	y[y<bound] <- NA
	lines(x, y, col='gray', lty=1)
}


#' @importFrom stats qnorm
which_data_outside_ks <- function() {
	n <- length(pkg.env$the.data)
	limit <- 0.886/sqrt(n)  # lillifors 67
	m <- mean(pkg.env$the.data); s <- sd(pkg.env$the.data)
	z <- qnorm(p<-(1:n)/(n+1))
	ks.hi <- m+s*qnorm(pmin(0.9999,p+limit))
	ks.hi[0.9999 < p+limit] <-  1 + pkg.env$the.data[n]  # fix min/max; data are sorted
	ks.lo <- m+s*qnorm(pmax(0.0001,p-limit))
	ks.lo[p-limit < 0.0001] <- -1 + pkg.env$the.data[1]
	return( (pkg.env$the.data < ks.lo) | (ks.hi < pkg.env$the.data))
}


#' @importFrom graphics lines
draw.data.points <- function(mar) {	# add dashes at base of hist
	n <- length(pkg.env$the.data)
	x <- rep(c(mar*0.99,mar*1.01,NA),n)
	y <- rep(pkg.env$y.vase$map(pkg.env$the.data),rep(3,n))
	lines(x,y)
}

pkg.env$state <- list("Empty", F, 0)

states.match <- function(name,emp,n) {
	if(is.numeric(name)) name <- sum(name)
	match <- (name == pkg.env$state[[1]]) && (emp == pkg.env$state[[2]]) && (n== pkg.env$state[[3]])
	if(! match) assign("state",list(name,emp,n), envir=pkg.env)
	return (match);
}

#' Side-by-side Slider
#'
#' Opens the default web browser with a display of two continuous distributions
#' shown side-by-side and reflected to suggest a closed container.
#' Interactive options control the display of water levels and the choice of
#' the shapes of the underlying distributions.
#'
#' @param name Name of distribution to contrast with normal
#' @param dists Named list of (d=density, q=quantile, r=sample) distributions (see make_df_list())
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' side_by_side_slider("Gamma")
#'
#' dists <- make_df_list()
#' dists[["uniform"]] <- list(d=dunif, q=qunif, r=runif);
#' side_by_side_slider("uniform", dists)
#' }
#' @export

side_by_side_slider <- function(name, dists = make_df_list()) {
	if(is.function(dists)) df.list <- dists() else df.list <- dists;
	dq <- num.density.and.quantile(name, df.list=df.list)  # at pkg.env$prob.grid
	make.left.right.vases(pkg.env$prob.grid, dq$d, dq$q, df.list=df.list);
	shiny::shinyApp(
	ui = shiny::fluidPage(
		shiny::headerPanel("Side-by-side Distribution vases"),
		shiny::sidebarLayout(
			shiny::sliderInput("pct", "Percentile", min=0, max=1, value=.10, step=.01),
			shiny::mainPanel(shiny::plotOutput("vases"))
      	) ),
    server = function(input, output) {
		output$vases <- shiny::renderPlot(
			draw.side.by.side(input$pct)
			)
		}
	)
}

side.by.side.slider <- function(...) {
	cat("Please use the renamed function side_by_side_slider.\n");
	}

make.left.right.vases <- function (prob, dens, quan, df.list=NULL, hist=F, breaks="FD") {
	mar=0.15; inset=0.05; top=0.98
	cL <- 0.25; cR <- 0.75; wid <- 0.175; bot <- mar+inset  # centers, width, top
	assign("left.vase", make.vase(cL-wid,bot,cL+wid,top,dens,quan,hist=hist,half=F,hor=F), envir=pkg.env)
	dq <- num.density.and.quantile("Normal", prob, df.list=df.list);
	assign("right.vase", make.vase(cR-wid, bot, cR+wid, top, dq$d, dq$q, half=F, hor=F), envir=pkg.env)
	ratio <- pkg.env$left.vase$grid.area()/pkg.env$right.vase$grid.area()
	if(ratio < 1) {   # make the larger vase smaller; simple screen area adjustment
		wid <- wid*ratio;
		assign("right.vase",make.vase(cR-wid, bot, cR+wid, top, dq$d, dq$q, half=F, hor=F), envir=pkg.env)
	} else {
		wid <- wid/ratio;
		assign("left.vase", make.vase(cL-wid,bot,cL+wid,top,dens,quan,hist=hist,half=F,hor=F), envir=pkg.env)
	}
}

#' @importFrom graphics plot.new
#' @importFrom graphics par
#' @importFrom graphics plot.window
draw.side.by.side <- function(pct, frame=F, mar=0.15, inset=0.05) {
	plot.new();
	par(mar=c(0,0,0,0));
	plot.window(xlim = 0:1, ylim = 0:1, xaxs = "i", yaxs = "i");
	if(frame) graphics::rect(0.001,mar,0.999,0.999)
	pkg.env$left.vase$draw(pct)
	pkg.env$right.vase$draw(pct)
}

###  empirical distributions

num.density.and.quantile <- function(name.or.data, df.list=NULL, prob=pkg.env$prob.grid, breaks="FD") {
	if(is.character(name.or.data)) {
		dqr <- df.list[[name.or.data]];
		q <- dqr$q(prob)
		return( list(d=dqr$d(q), q=q) )
	} else {
		return (histogram.and.quantile(name.or.data, prob, breaks=breaks))
	}
}

#' @importFrom stats quantile
#' @importFrom graphics hist
histogram.and.quantile <- function(data, prob, breaks="FD") {  # data is sorted
	quan <- quantile(data,prob,type=4)
	hist <- hist(data, plot=F, breaks=breaks)
	dens <- rep(NA,length(prob))
	b <- 2   # lowest hist bin < min(data)
	quan[1] <- max(hist$breaks[1],quan[1]-0.01*diff(range(data))) # pad edge of plot
	for(i in 1:length(dens)) {
		while(quan[i] > hist$breaks[b]) { b <- b + 1 }
		dens[i] <- hist$density[b-1];
	}
	return(list(p=prob, d=dens, q=quan))
}

# hq <- histogram.and.quantile(pkg.env$the.data,pkg.env$prob.grid)

#' @importFrom stats density
#' @importFrom stats quantile
kernel.dens.and.quantile <- function(data, prob) {  		# data is sorted
	kd <- density(data, bw="nrd0", kernel='gaussian')
	cdf <- cumsum(kd$y); cdf<- cdf/cdf[length(cdf)]
	dens <- rep(NA,length(prob))
	quan <- quantile(data,prob,type=4)					# 1=empirical quantile
	i <- 2;
	for(k in 1:length(prob)) {
		while(cdf[i] < prob[k]) { i <- i + 1 }
		f <- (prob[k]-cdf[i-1])/(cdf[i]-cdf[i-1]);
		dens[k] <- kd$y[i-1] + f * (kd$y[i]-kd$y[i-1])
		# quan[k] <- kd$x[i-1] + f * (kd$x[i]-kd$x[i-1])
		k <- k+1;
	}
	return(list(p=prob, d=dens, q=quan))
}

