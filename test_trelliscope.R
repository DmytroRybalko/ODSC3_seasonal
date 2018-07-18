library(trelliscopejs)
library(ggplot2)
library(dplyr)

glimpse(mpg)

ggplot(aes(cty, hwy), data = mpg) +
  geom_abline(alpha = 0.5) +
  geom_point() +
  xlim(7, 37) + ylim(9, 47) + theme_bw() +
  facet_wrap(~ manufacturer + class, nrow = 4)

qplot(cty, hwy, data = mpg) +
  geom_abline(alpha = 0.5) +
  geom_point() +
  xlim(7, 37) + ylim(9, 47) + theme_bw() +
  facet_trelliscope(~ manufacturer + class, nrow = 2, ncol = 4)
