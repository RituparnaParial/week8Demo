# examining two samples: with and without a phone
# adn we're comparing the rection time of both

install.packages("pwr")
library(pwr)
power_information <- pwr.t.test(d = 0.8,
                                sig.level = 0.01,
                                power = 0.95,
                                type = "two.sample",
                                alternative = "two.sided")


power_information
plot(power_information)

h = ES.h(p1 = 0.5, p2 = 0.75)

power_changes <- pwr.p.test(h =h,
                            sig.level = 0.05,
                            power = 0.90)
power_changes                            
plot(power_changes)


# Cohen describes the effect size as "the degree to which the null hypothesis is false"
# in the coin flip example, this is the difference between the 75% and the 50%

cohen.ES(test = "r", size = "small")