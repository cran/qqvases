## -------------------------------------------------------------------------------------
##
## List elements are named and provide a d (density), q (quantile) and r
## (random number generator that takes the sample size as its argument).  
## 
## -------------------------------------------------------------------------------------

#' Make a named list of distribution functions 
#'
#' Constructs the default nested list of distribution functions used to animated
#' normal QQ plots. By default, the list contains 
#'   Normal  Student t_3  Student t_6  Gamma(3)  Beta(0.6,0.6)   Exotic
#' Each member of the list has 3 named elements [
#'    d = density function  (eg, dnorm)
#'    q = quantile function (eg, qnorm)
#'    r = random generator  (eg, rnorm) ]
#' Each of these functions must take a single argument; others must be bound externally.
#' For examples, see the function make.normal.dist or make.t.dist.
#'
#' @return Named list of (density, quantile, generator) triples for distributions
#'
#' @examples
#' make_df_list()
#'
#' @export

make_df_list <- function() {
	list(	
		"Normal"		= make.normal.dist(),  
		"Student t(3)"	= make.t.dist(3),
        "Student t(6)"	= make.t.dist(6),
        "Beta"          = make.beta.dist(0.8,0.8),
        "Gamma"         = make.gamma.dist(3),
        "Exotic"        = make.exotic.dist()
     ) }

#' @importFrom stats dnorm
#' @importFrom stats qnorm
#' @importFrom stats rnorm
make.normal.dist <- function() {
	list(d=dnorm, q=qnorm, r=rnorm) }
	
#' @importFrom stats dgamma
#' @importFrom stats qgamma
#' @importFrom stats rgamma
make.gamma.dist <- function(shape) {
	list(
		d = function(x) return(dgamma(x,shape=shape)),
		q = function(p) return(qgamma(p,shape=shape)),
		r = function(n) return(rgamma(n,shape=shape))
		)
	}

#' @importFrom stats dbeta
#' @importFrom stats qbeta
#' @importFrom stats rbeta
make.beta.dist <- function(p1, p2) {
	list(
		d = function(x) return(dbeta(x,p1,p2)),
		q = function(p) return(qbeta(p,p1,p2)),
		r = function(n) return(rbeta(n,p1,p2))
		)
	}

#' @importFrom stats dt
#' @importFrom stats qt
#' @importFrom stats rt
make.t.dist <- function(df) {
	list(
		d = function(x) return(dt(x,df=df)),
		q = function(p) return(qt(p,df=df)),
		r = function(n) return(rt(n,df=df))
		)
	}
			
#' @importFrom stats pexp
#' @importFrom stats pnorm
#' @importFrom stats dexp
#' @importFrom stats dnorm
#' @importFrom stats rexp
#' @importFrom stats rnorm
#' @importFrom stats rbinom
#' @importFrom stats uniroot
make.exotic.dist <- function (a=0.5, r=1, mu=4.5, sd=1.25) {
	b <- 1 - a
	cdf  <- function(x) return( a*pexp(x, rate=r) + b*pnorm(x, mean=mu, sd=sd) )
	root <- function(p) return(uniroot(function(x) cdf(x) - p, c(0,10))$root)
	dens <- function(x) return( a*dexp(x, rate=r) + b*dnorm(x, mean=mu, sd=sd))
	quan <- function(p) return( sapply(p, root, simplify=T) )
	rand <- function(n) {	xa<- rexp(n,rate=r); xb <- rnorm(n,mean=mu,sd=sd)
							return (ifelse(rbinom(n,size=1,prob=a),xa,xb)) }
	return (list(d=dens,q=quan,r=rand))
}	
