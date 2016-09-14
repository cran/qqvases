#####################################################################################
             
## --- construct an object; the input coordinates are used for the full vase
##     only half of the range is used when half is chosen. (mean(xleft,xright) is centerline)
#
#		grid = rows matched to percentiles of dist, so not equal spacing (eg row 658 is 0.658 pct)
#		area = more like screen rectangle size

make.vase <- function(xLeft, yBottom, xRight, yTop, density, quantile, hist=F, half=F, hor=F) {
	area <- vase.area(xLeft, yBottom, xRight, yTop, density, quantile)
	grid <- vase.grid(xLeft, yBottom, xRight, yTop, density, quantile, half, hor)
	if(hist & (!hor)) grid$Y <- patch.hist.grid.y(grid) 
	list(
		area     = function() area,
		grid     = function() grid,
		grid.x   = function() grid$X,
		grid.y   = function() grid$Y,
		quantile = function() quantile,
		grid.area= function()
		{	if (hor) { x <- grid$Y; y <- grid$X }
			else     { x <- grid$X; y <- grid$Y }
			return (sum( (x[,2]-x[,1])*c(diff(y[,1]),0) ))
		},
		map      = function(x) # x to screen coord
		{	if(hor) { g <- c(xLeft,xRight) }
			else    { g <- c(yBottom,yTop) }
			scl <- g[2]-g[1]
			rng <- quantile[length(quantile)]-quantile[1]
			x <- (x-quantile[1])/rng
			return(g[1] + scl*x)
		},
		draw     = function(p)
		{ m <- 502  # length(.prob.grid.)
		  top <- ifelse((1==length(p))&&is.na(p), m, min(m,max(0,floor(m*p))));
		  draw.region(grid, 1, top);
		  outline.region(grid, 1, m);
		  return(top);
		}
	) }


vase.area <- function(xLeft, yBottom, xRight, yTop, density, quantile) {
	c       <- (xLeft+xRight)/2
	qBottom <- quantile[1]
	qTop    <- quantile[length(quantile)]
	sigma <- (yTop-yBottom)/(qTop-qBottom) #  dy/dz
	return(2 * sigma * (xRight-c) / max(density))
}

vase.grid <- function(xLeft, yBottom, xRight, yTop, density, quantile, half, hor) {
	max.dens <- max(density)
	qBottom <- quantile[1]
	qTop    <- quantile[length(quantile)]
	if (hor) {  # swap x-y coordinates for common manipulation
		saveX <- c(xLeft,xRight)
		xLeft <- yBottom; xRight <- yTop;
		yBottom <- saveX[1]; yTop <- saveX[2] }
	sigma <- (yTop-yBottom)/(qTop-qBottom)
	yy    <- yBottom + sigma * (quantile-qBottom)
	xCtr  <- (xLeft + xRight)/2
	f  <- ((xRight-xCtr)/max.dens) * density
	X <- Y <- matrix(NA, nrow=length(yy), ncol=2);
	if (half) {
		for(i in 1:length(yy))	{
			X[i,] <- xCtr + c(-f[i], 0)
			Y[i,] <- rep(yy[i], 2)	} }
	else {
		for(i in 1:length(yy))	{
			X[i,] <- xCtr + c(-f[i], f[i])
			Y[i,] <- rep(yy[i], 2)	} }
	if (hor) return (list(X=Y, Y=X))
	else     return (list(X=X, Y=Y))
	}

patch.hist.grid.y <- function(grid) {  # assumes vertical
	i <- which(0 != c(0,diff(grid$X[,1])))
	grid$Y[i,] <- grid$Y[i-1,]
	return (grid$Y)
}


#' @importFrom graphics polygon
draw.region <- function(grid, lo, hi,col='blue') {
	polygon(list(x=c(grid$X[lo:hi,1],grid$X[hi:lo,2]),
				 y=c(grid$Y[lo:hi,1],grid$Y[hi:lo,2])),
			col=col, border=col)
	}

#' @importFrom graphics lines
outline.region <-function(grid, lo, hi, col='skyblue') {
	lines(grid$X[lo:hi,1], grid$Y[lo:hi,1], col=col)
	lines(grid$X[hi,1:2] , grid$Y[hi,c(1,1)],col=col)
	lines(grid$X[hi:lo,2], grid$Y[hi:lo,2], col=col)
}


