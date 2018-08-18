library(tidyverse)
library(mgcv)

vowels <- read_csv("./data/vowels.csv") %>%
  mutate(
    vowel = as.ordered(vowel),
    voicing = ordered(voicing, levels = c("voiceless", "voiced")),
    place = as.ordered(place),
    vow_voi = interaction(vowel, voicing),
    vow_voi = as.ordered(vow_voi),
    speaker = as.factor(speaker),
    word = as.factor(word)
  ) %>%
  na.omit()

contrasts(vowels$vowel) <- "contr.treatment"
contrasts(vowels$voicing) <- "contr.treatment"
contrasts(vowels$place) <- "contr.treatment"
contrasts(vowels$vow_voi) <- "contr.treatment"

f0_gam <- bam(
  f0 ~
    s(time_point, k = 6),
  data = vowels
)

f0_gam_3 <- bam(
  f0 ~
    vowel +
    voicing +
    s(time_point, k = 6) +
    s(time_point, k = 6, by = vowel) +
    s(time_point, k = 6, by = voicing) +
    s(time_point, speaker, bs = "fs", k = 6) +
    s(time_point, word, bs = "fs", k = 6),
  data = vowels
)

f0_gam_4 <- bam(
  f0 ~
    vow_voi +
    s(time_point, k = 6) +
    s(time_point, k = 6, by = vow_voi) +
    s(time_point, speaker, bs = "fs", k = 6) +
    s(time_point, word, bs = "fs", k = 6),
  data = vowels
)

save(vowels, f0_gam, f0_gam_3, f0_gam_4, file = "./practicals/practical-2/www/practical-2.Rdata")
