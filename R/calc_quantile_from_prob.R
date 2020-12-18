#' Calculate quantiles from probabilities
#'
#' Calculates z, t, and non-standardized values for the normal and t distributions from probabilities and the direction the probability refers to.   This function is targeted to help introductory level students and professors to use easily answer questions and learn R, without having to understand for complicated functions like qnorm and qt.
#' @param prob the probability referring to percentile of a distribution.
#' @param mu the mean of the distribution.
#' @param sd the standard deviation of the distribution.
#' @param n the sample size.  Defaults to 1 for normal distributions.
#' @param test takes either "z" or "t" to define the distribution from which the quantile should be pulled.
#' @param direction where the probability refers to.  Direction can take any string value, but any string containing the letter "l" will be assumed to mean the "lower" or "left" tail.  Probabilities are assumed to be either tail.
#' @param shade a logical for if a shaded density curve should be provided.
#' @param digits an integer in for the number of decimal places to which the answer should be rounded.  Note: positive numbers refer to the number of digits after the decimal place, negative numbers refer to the number of digits before the decimal place, and 0 refers to the nearest whole number.
#'
#' @return a statement laying out the values imputed, the subsequent quantile, p-value, and an appropriate conclusion.
#' @export
#' @import stats
#'
#' @examples
#' calc_quantile_from_prob(.9, mu = 20.8, sd = 5.8, direction = "lower", test = "z")
#' calc_quantile_from_prob(.1, mu = 20.8, sd = 5.8, direction = "lower", test = "z")
calc_quantile_from_prob <- function(prob, mu = 0, sd = 1, n = 1, test = "z",
                                    direction = "greater", shade = FALSE, digits = 4) {

  #calc_quantile_from_prob takes a probability and returns a
  # score from the appropriately standardized distribution, as
  # well as a the corresponding quantile from the non-standardized
  # distribution.
  #calc_quantile_from_prob returns a statement highlighting both
  # the appropriate z/t score and the quantile.

  ##Arguments:
  #prob = the probability for which the quantile will be found.
  #mu = the mean of the sample/population
  #sd = the standard deviation of the sample/population.
  #n = the sample size
  #test = the test for which we should calculate a quantile.
  # test takes one of two possible values (z or t).
  #direction = the tail to which the probability corresponds.
  # direction can take any string value, but any string containing
  # the letter "l" will be assumed to mean the "lower" or "left" tail.
  #shade = a logical for if the output should include a plot
  # that shades the area of interest and the resulting p-value.
  #digits = the number of decimal places to which the answer
  # should be rounded.  Note: positive numbers refer to the
  # number of digits after the decimal place, negative numbers
  # refer to the number of digits before the decimal place, and
  # 0 refers to the nearest whole number.

  # Need to add this later
  # I need to figure out how to
  # show the plot and the text together
  # if(shade){
  #   #shade function and output here.
  # }


    low_tail <- grepl("l", direction)
    direction <- ifelse(grepl("l", direction), "less than", "greater than")
    df <- ifelse(n == 1, n, n - 1)
    se <- sd/sqrt(n)

    if (toupper(test) == "Z") {
      z <- qnorm(prob, lower.tail = low_tail)
      x <- z*se + mu

      return(paste("The z-score associated with the p-value ", prob,
             ", for a sample of size ", n,
             " is ", round(z, digits = digits), ".",
             "  Thus, for a distibution with mean ", mu,
             " and standard deviation ", sd,
             " there is a probability of ", prob,
             " that we would observe a value ", direction,
             " or equal to ", round(x, digits = digits), ".",
             sep = ""))
    } else if (toupper(test) == "T") {
      t <- qt(prob, df = df, lower.tail = low_tail)
      x <- t*se + mu

      return(paste("The t-score associated with the p-value ", prob,
             ", for a sample of size ", n,
             " is ", round(t, digits = digits), ".",
             "  Thus, for a distibution with mean ", mu,
             " and standard deviation ", sd,
             " there is a probability of ", prob,
             " that we would observe a value ", direction,
             " or equal to ", round(x, digits = digits), ".",
             sep = ""))
    } else {
      return(paste("Error: Please enter a test of either z or t."))
    }#end test if statements


}#calc_p_value
