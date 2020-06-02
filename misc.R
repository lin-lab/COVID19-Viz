# misc exploration stuff
library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)

global_rt_long <- fread("raw_data/jhu_global_rt.tsv")

china_rts <- global_rt_long %>%
  filter(Country_Region == "China", !is.na(Province_State))

china_rts %>%
  filter(positiveIncrease >= 0) %>%
  ggplot(aes(x = positiveIncrease)) +
  geom_histogram(bins = 20) +
  scale_x_sqrt() +
  facet_wrap(~Province_State)

global_rt_long[, .N, by = .(positiveIncrease > 25)]
global_rt_long[is.na(Province_State) & positive > 50,
               .N, by = .(positiveIncrease > 10)]


global_rt_long[positiveIncrease >= 0 & positiveIncrease < 1000] %>%
  ggplot(aes(x = positiveIncrease)) +
  geom_histogram(bins = 20) +
  scale_x_sqrt()


global_rt_long[positiveIncrease < 0, .(positiveIncrease, Combined_Key)]


global_rt_long[, ci_width := ci_upper - ci_lower]

global_rt_long[positiveIncrease >= 0, ] %>%
  ggplot(aes(x = log10(1 + positiveIncrease), y = ci_width)) +
    geom_point()
