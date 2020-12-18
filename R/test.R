

x <- 15
mu <- 10
se <- 10
test = "z"
calc_z_score(x = 15, mu = 10, sigma = 5, n = 10, p_val = TRUE, sig_level = 0.05, alternative = 1)
calc_z_score(x = 100, mu = 90, sigma = 5)

calc_t_score(x = 15, mu = 10, s = 10, n = 10, p_val = TRUE, sig_level = 0.05, alternative = 3)


calc_quantile_from_prob(0.8413448, direction = "l", test = "t")


start = -2
stop = NULL
prob = NULL
mu = 0
sd = 1
n = 1
direction = "both"
test = "t"
fill = "orange"
digits = 4

shade_area(start = 2, mu = 0, sd = 10, test = "t", direction = "greater")
shade_area(start = 2, mu = 0, sd = 1, test = "t", direction = "less")
shade_area(start = 2, stop = -2, mu = 0, sd = 1, test = "t", direction = "middle")
shade_area(start = -2, mu = 0, sd = 1, n = 1, test = "t", direction = "both")

shade_area(start = 2, mu = 0, sd = 1, test = "z", direction = "greater")
shade_area(start = 2, mu = 0, sd = 1, test = "z", direction = "less")
shade_area(start = 2, stop = -2, mu = 0, sd = 1, test = "z", direction = "middle")
shade_area(start = -2, mu = 0, sd = 1, test = "z", direction = "both")

shade_area(p = .5, mu = 0, sd = 1, test = "t", direction = "greater")
shade_area(p = .025, mu = 0, sd = 1, test = "t", direction = "less")
shade_area(p = .95, mu = 0, sd = 1, test = "t", direction = "middle")
shade_area(p = .05, mu = 0, sd = 1, n = 1000, test = "t", direction = "both")

shade_area(p = .025, mu = 0, sd = 1, n = 10, test = "z", direction = "greater")
shade_area(p = .025, mu = 0, sd = 1, test = "z", direction = "less")
shade_area(p = .95, mu = 0, sd = 1, test = "z", direction = "middle")
shade_area(p = .05, mu = 0, sd = 1, test = "z", direction = "both")







