#' @title Does the analysis for the exercise for Session 6 from the MLW / KUHeS Statistics and R workshop.
#'
#' @description
#' This is an example package for Session 6 of the MLW / KUHeS Statistics and R workshop. This function performs either a t-test or a linear regression on a simulated, internally stored dataset, dat.
#'
#' @param analysis A character string indicating which analysis to do. Needs to be one of "t.test" (for a t-test), "linReg" (for a linear regression).
#'
#' @return An object of class \code{htest} or \code{glm} depending whether a t-test or a linear regression was performed
#'
#' @seealso
#' \code{\link[stats]{t.test}}, \code{\link[stats]{glm}}, \code{\link[examplePack]{doPlot}}
#'
#' @examples
#' ## Example 1 - t-test
#' doAnalysis("t.test")
#'
#' ## Example 2 - linear regression
#' doAnalysis("linReg")
#'
#' @export doAnalysis
#'

doAnalysis<-function(analysis){
  if(analysis=="t.test"){
    res<-stats::t.test(z~type,data=dat)
  }else if(analysis=="linReg"){
    res<-stats::glm(z~x,family=gaussian,data=dat)
  }else{stop(paste(sep="","Argument 'analysis' needs to be one of 't.test' or 'linReg'. Received < ",analysis," >."))}

  return(res)
}


#' @title Does the plot for the exercise for Session 7 from the MLW / COM Statistics and R workshop.
#'
#' @description
#' This is an example package for Session 7 of the MLW / COM Statistics and R workshop. This function plots either a box & jitter plot or a scatter plot with a linear regression model.
#'
#' @param analysisResult The output from the function \code{doAnalysis}.
#'
#' @return Produces a plot, but returns no other output.
#'
#' @seealso
#' \code{\link[examplePack]{doAnalysis}}
#'
#' @examples
#' ## Example
#' res<-doAnalysis("t.test")
#' doPlot(res)
#'
#' @export doPlot
#'

doPlot<-function(analysisResult){
  if(class(analysisResult)[1]=="htest"){
    dat %>%
      ggplot2::ggplot(mapping=aes(x=type,y=z,col=type)) +
      ggplot2::geom_boxplot(alpha=0.5) +
      ggplot2::geom_jitter(width=0.25,height=0) +
      ggplot2::xlab("Type") +
      ggplot2::ylab("z") +
      ggplot2::ggtitle(paste(sep="","p = ",round(digits=4,analysisResult$p.value))) +
      ggplot2::scale_colour_manual(values=c("steelblue","orange"))
  }else if(class(analysisResult)[1]=="glm"){
    dat %>%
      ggplot2::ggplot(mapping=aes(x=x,y=z,col=type)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method="lm",col="black",lty=2,lwd=1.25) +
      ggplot2::scale_colour_manual(values=c("steelblue","orange")) +
      ggplot2::ggtitle("Linear regression model for z regressed on x.")
  }else{stop("The specified input object 'analysisResult' is not a valid output object from the function 'doAnalysis'.")}
}
