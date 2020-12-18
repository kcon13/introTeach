# introTeach 
<hr/>
## Overiew
introTeach is a package for helping students in introductory statistics and professors teaching introductory statistics.  Currently, it is focused on helping students calculate z-scores, t-scores, probabilities, and visualizing the Normal and T density curves with shaded regions. The basic goal is to create a some functions that keep new students from having to fight with the difference between dnorm and pnorm (dt and pt).
Future iterations will include combining text and visual outputs, walkthrough solutions, and functions for helping professors teach introductory classes through easy to follow code and output.

## Installation
Information for how to install will be provided when it is ready for installation.

## Usage
Currently, this package can be used to perform the following 4 tasks:

1. Calculate a z-score and associated p-value when given an x-value or sample mean, a population or hypothesized mean, population standard deviation, and a direction for the alternative hypothesis.  Optional values include a significance level, a sample size, and the number of digits to which rounding should occur.  

1. Calculate a t-score and associated p-value when given an x-value or sample mean, a hypothesized mean, sample standard deviation, sample size, and a direction for the alternative hypothesis.  Optional values include a significance level, a sample size, and the number of digits to which rounding should occur.  

1. Calculate a quantile from a normal or t distribution when given a probability, the mean and standard deviation of the distribution, the sample size, the distribution the quantile should come from, and the direction (or tail) to which the probability refers.  Optional values include the number of digits to include for rounding.

1. Shade area of density function for which a 1 sample hypothesis test of means is looking.  The function can take standardized or non-standardized values for either a t or normal distribution.  1 or 2 tail tests, plus middle areas can all be specified.  Optional specifications include color and number of digits for rounding.


## Examples
To calculate a z-score and p-value:
Tahir is a selling lemonade and thinks that today was a big day. Today's profit was \$15.  To determine if this is a lot compared to normal, Tahir looks back over the last year and sees that the daily average is \$10 in profit, and that daily profits are normal.  Let the true standard deviation be \$5.  At the 5% significance level, determine if today was a better day than normal.

calc_z_score(x = 15, mu = 10, sig = 5, p_val = TRUE, sig_level = 0.05, alternative = 1)

[1] "The z-score for the mean 15, for a sample of size 10 from a distribution with mean 10 and standard deviation 5 is 3.1623.  The associated p-value is 8e-04.  For a level 0.05 hypothesis test, a p-value of 8e-04 suggests that we should reject the null hypothesis that mu is at most 10."



calc_t_score(x, mu, sig, n = 10, p_val = TRUE, sig_level = 0.05, alternative = 3)


calc_quantile_from_prob(0.8413448, direction = "l", test = "t")


