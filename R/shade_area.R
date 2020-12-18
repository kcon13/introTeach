
#' Shades the area of a Normal or T density curve
#'
#' Shades an area under a density curve corresponding the being tested in an introductory level 1-sample hypothesis test of means.  Either a probability value or an X value can be provided.
#' Either start or prob must be provided to indicate an area to be shaded.  If the area to be shaded is in the middle, both start and stop or only prob are necessary.
#'
#' @param start the number indicating where the shading starts.  If only tails (one or both) are being shaded, the start should be the value closest to the mean (the function shades from the middle to +/- infinity).  If a middle section is being shaded, start should be the smaller value.
#' @param stop the number indicating where the shading stops for shading the middle of a distribution.
#' @param prob the area or probability to be shaded.  This value should be the area and not the percentile (i.e. shading the area to the right of the 97.5\% would take 0.025).
#' @param mu the mean of the distribution if it isn't a standardized distribution.
#' @param sd the standard deviation of the distribution if it isn't a standardized distribution.
#' @param n the sample size if the start/stop/prob come from (or relate to) a sample.
#' @param direction the area to be shaded.  direction takes 4 possible values: 1"greater", 2"less",3 "middle", 4"both".  "greater" = the right tail.  "less" = the left tail.  "middle" = the middle of the distribution.  "both" = both the left and right tails.
#' @param test the distribution to be used.  Takes either "z" or "t".
#' @param fill the color to be used for shading.
#' @param digits an integer in for the number of decimal places to which the answer should be rounded.  Note: positive numbers refer to the number of digits after the decimal place, negative numbers refer to the number of digits before the decimal place, and 0 refers to the nearest whole number.
#'
#' @return a plot with a shaded area.
#' @export
#' @import ggplot2
#'
#' @examples
#' shade_area(15, mu = 10, sd = 5, direction = "greater", test = "z")
#' shade_area(80, mu = 100, sd = 50, n = 40, direction = "both", test = "t")
#'
#' @seealso
#' https://sebastiansauer.github.io/shade_Normal_curve/
#'
shade_area <- function(start = NULL, stop = NULL, prob = NULL, mu = 0, sd = 1, n = 1,
                       direction = "greater", test = "z", fill = "orange", digits = 4) {
  #shade_area takes some values of interest and plots a normal or
  # standardized t density with an area shaded corresponding to
  # the information provided.  For example, if a student was hoping
  # to visualize what area of a density function an introductory
  # z-score or hypothesis test question was asking for, they could enter
  # the X value, the mean, sd, and direction of interest.  shade_area
  # will spit back a plot of the normal distribution associated with the
  # mean/sd given, and will shade the tail (or middle) that answers the
  # question.
  #shade_area returns a plot.  (Hope to add text with the plot someday.)
  #shade_area requires that at least one of start or prob is given.



  ##Arguments:
  #start = an X value or z-score that defines the z-score or t-score
  # that is of interest.
  #stop = a value to be used if there is a clearly defined end range
  # Note: stop only needs to be used if the interest is in some range
  # over the middle of the distribution.
  #prob = the area under the curve or the probability that is wanted
  # to be shaded.
  #mu = the mean of the distribution.
  #sd = the standard deviation of the distribution.
  #n = the sample size.
  #direction = the direction of the area to be shaded.
  # direction takes 4 possible values, c("greater", "less",
  # "middle", "both").
  # "greater" = the right tail.
  # "less" = the left tail.
  # "middle" = the middle of the distribution.
  # "both" = both the left and right tails.
  # Note: if "both" is selected with a start value, then
  # the start value should be the lower value given.  If the value
  # provided is greater than the mean, then the entire distribution
  # will be shaded.
  #test = the proper distribution to shade.
  # test takes two values ("z", "t"), corresponding to a z test or
  # a t-test.  Currently only 1 sample mean tests are supported.
  #fill = the color to be used to shade the tails.
  #digits = the number of decimal places to which the axis ticks
  # should be rounded.  Note: positive numbers refer to the
  # number of digits after the decimal place, negative numbers
  # refer to the number of digits before the decimal place, and
  # 0 refers to the nearest whole number.

  #direction can be greater, less, middle, both
  se <- sd/sqrt(n)
  deg_free <- ifelse(n == 1, n, n - 1)
  lims <- 3

  if(toupper(test) != "Z" & toupper(test) != "T"){
    stop(paste("Error: please enter either 'z' or 't' for the test value."))
  }
  if(is.null(start) & is.null(prob)){
    stop("Error: either start or prob is required.")
  }#error invalid test

  if(toupper(test) == "T"){
    start <- (start - mu) / se
    stop <- if(!is.null(stop)) {(stop - mu) / se}
    lims <- min(qt(.999, deg_free), 4)
  }


  if(!is.null(prob)){
    p <- prob

    prob <- ifelse(toupper(direction) == "MIDDLE", prob/2 + .50, prob)
    prob <- ifelse(toupper(direction) == "GREATER", 1 - prob, prob)
    prob <- ifelse(toupper(direction) == "BOTH", prob/2, prob)

    start <- ifelse(toupper(test) == "Z", qnorm(prob), qt(prob, df = deg_free))
    stop <- ifelse(toupper(direction) == "MIDDLE", -start, -1000)
    stop <- ifelse(toupper(direction) == "GREATER", -stop, stop)

    if(toupper(test) == "Z"){
      start <- mu + start * se
      stop <- mu + stop * se
    }
  }#end !is.null(prob)

  if(toupper(test) == "Z"){
    x <- seq(from = mu - lims * se, to = mu + lims * se, by = 0.01)
  } else {
    x <- seq(from = -lims, to = lims, by = 0.01)
  }#end test limits

  if(toupper(test) == "Z"){
    y <- dnorm(x, mean = mu, sd = se)
  }else if(toupper(test) == "T"){
    y <- dt(x, df = deg_free)
  }#end dnorm/dt
  df <- data.frame(x = x, y = y)
  stop <- ifelse(is.null(stop), mu + (-1000)*se, stop)
  if(is.null(prob)){
    stop <- ifelse(toupper(direction) == "GREATER", -stop, stop)
  }#end is.null(prob)

  if(stop < start){
    temp <- start
    start <- stop
    stop <- temp
  }

  if(toupper(direction) == "BOTH" & toupper(test) == "Z"){
    fstart <- -start
    fstop <- (mu - stop) + mu
  }else {
    fstart <- -start
    fstop <- -stop
  }#end start/stop flips

  brks <- round(seq(-lims, lims, length = 7), digits = digits)
  if(toupper(test) == "Z"){brks <- round((-3:3) * se + mu, digits = digits)}
  p <- abs(ifelse(toupper(test) == "Z",
              pnorm(stop, mean = mu, sd = sd) - pnorm(start, mean = mu, sd = sd),
              pt(stop, df = deg_free) - pt(start, df = deg_free)))



  if(toupper(direction) == "BOTH"){
    shade_plot <- ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_area(data = subset(df, x >= start
                              & x <= stop),
                aes(y=y), fill = fill) +
      geom_area(data = subset(df, x >= fstop
                              & x <= fstart),
                aes(y=y), fill = fill) +
      scale_x_continuous(breaks = brks) +
      scale_y_continuous(breaks = NULL) +
      theme(panel.background = element_blank(),
            axis.line.x = element_line(),
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      ylab("") +
      xlab("") +
      ggtitle(paste(ifelse(toupper(test) == "Z",
                           "Normal Distribution",
                           paste("T Distribution with ", deg_free,
                                 " Degree(s) of Freedom", sep = "")),
                    sep = ""))

  } else {
    shade_plot <- ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_area(data = subset(df, x >= start
                              & x < stop),
                aes(y = y), fill = "orange") +
      scale_x_continuous(breaks = brks) +
      scale_y_continuous(breaks = NULL) +
      theme(panel.background = element_blank(),
            axis.line.x = element_line(),
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      ylab("") +
      xlab("") +
      ggtitle(paste(ifelse(toupper(test) == "Z",
                           "Normal Distribution",
                           paste("T Distribution with ", deg_free,
                                 " Degrees of Freedom", sep = "")),
                    sep = ""))
  }

  return(shade_plot)


}#end shade_area function




