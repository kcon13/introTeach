#' Calculate t-score and p-values
#'
#' Calculates t-score and p-values for hypothesis test and t-score questions.  This function is targeted to help introductory level students and professors to use easily answer questions and learn R, without having to understand for complicated functions like pt and dt.
#'
#' @param x mean or value for which a z-score or probability of observing is to be calculated.
#' @param mu mean of the distribution or the hypothesized value.
#' @param s standard deviation of the sample
#' @param n sample size if x is a mean.  n = 1 by default for the case where x is a single value.
#' @param sig_level the significance level for a hypothesis test.
#' @param p_val a logical for if a p-value should be provided.
#' @param alternative takes 3 values indicating the direction of the alternative hypothesis, or the direction of the probability to be calculated.  The value of this does not matter if p_val is FALSE.  The options for this input are "greater", "less", and "not equal to". Currently, anything that isn't "greater" or "less" will be assumed to be "not equal to".  This will be updated in the future.
#' @param shade a logical for if a shaded density curve should be provided.
#' @param digits an integer in for the number of decimal places to which the answer should be rounded.  Note: positive numbers refer to the number of digits after the decimal place, negative numbers refer to the number of digits before the decimal place, and 0 refers to the nearest whole number.
#'
#' @return a statement laying out the values imputed, the subsequent t-score, p-value, and an appropriate conclusion (if p_val = TRUE, and sig_level has a value).
#' @export
#'
#' @examples
#' calc_t_score(x = 80, mu = 100, s = 50, n = 40, p_val = TRUE, sig_level = 0.05,
#' alternative = "not equal to")
#' calc_t_score(x = 10, mu = 5, s = 50, n = 40)
#'
calc_t_score <- function (x, mu, s, n, sig_level = NULL, p_val = FALSE,
                          alternative = "greater", shade = FALSE, digits = 4) {
  #calc_t_score takes some information from a hypothesis test
  # and calculates the appropriate t-score and p-value.
  #calc_t_score returns a sentence (or more than 1) highlighting
  # the sample and population values, the t-score, p-value, and
  # a conclusion for 1 or 2 sided hypothesis of 1 sample mean test.

  ##Arguments:
  #x = the mean or value in question/to be tested.
  #mu = the population mean.
  #s = the population standard deviation.
  #n = the sample size.  n = 1 is the default for the cases where
  # only one value is of interest (not the mean of a sample).
  #sig_level = the significance level for a hypothesis test.
  #p_val = a logical for if the p-value should be included in
  # the response.
  #alternative = the direction of the alternative hypothesis.
  # alternative can take 3 options ("greater", "less", "not equal to").
  # Currently, any value that isn't "greater" or "less" is assumed to be "not equal to".
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

  alternative <- ifelse(toupper(alternative) == "GREATER", 1,
                        ifelse(toupper(alternative) == "LESS", 2, 3))

  df <- ifelse(n == 1, 1, n - 1)
  se <- s/sqrt(n)
  t <- (x - mu) / se
  t <- round(t, digits = digits)

  if (!p_val) {

    return(paste("The t-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", s,
                 " is ", t, ".",
                 sep = "") )

  } else if (p_val & is.null(sig_level)) {
    l_tail <- ifelse(alternative == 1, FALSE, TRUE)
    neq <- ifelse(alternative == 3, TRUE, FALSE)
    if (neq) {
      l_tail <- ifelse(t > 0, FALSE, TRUE)
      p <- 2*pt(t, df = df, lower.tail = l_tail)
      p <- round(p, digits = digits)
      p <- ifelse(p < 0.0001, 0.0001, p)
    } else{
      p <- pt(t, df = df, lower.tail = l_tail)
      p <- round(p, digits = digits)
      p <- ifelse(p < 0.0001, 0.0001, p)
    } #end not equal to if statement

    return(paste("The t-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", s,
                 " is ", t, ".",
                 "  The associated p-value is ", p, ".",
                 sep = "") )

  } else if (p_val & !is.null(sig_level)) {
    l_tail <- ifelse(alternative == 1, FALSE, TRUE)
    neq <- ifelse (alternative == 3, TRUE, FALSE)
    if (neq) {
      l_tail <- ifelse(t > 0, FALSE, TRUE)
      p <- 2*pt(t, df = df, lower.tail = l_tail)
      p <- round(p, digits = digits)
      conc <- p <= sig_level
      p <- ifelse(p < 0.0001, 0.0001, p)
    } else{
      p <- pt(t, df = df, lower.tail = l_tail)
      p <- round(p, digits = digits)
      conc <- p <= sig_level
      p <- ifelse(p < 0.0001, 0.0001, p)
    } #end not equal to if statement

    return(paste("The t-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", s,
                 " is ", t, ".",
                 "  The associated p-value is ", p, ".",
                 "  For a level ", sig_level,
                 " hypothesis test, a p-value of ", p,
                 " suggests that we should ",
                 ifelse(conc, "reject", "not reject"),
                 " the null hypothesis that mu " ,
                 ifelse(alternative == 1,
                        paste("is at most ", mu, sep = ""),
                        ifelse(alternative == 2,
                               paste("is at least ", mu, sep = ""),
                               paste("is equal to ", mu, sep = ""))),
                 ".",
                 sep = "") )
  }
}#end calc_t_score
