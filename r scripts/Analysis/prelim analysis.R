library(tidyverse)
library(mgcv)
library(dplyr)
library(FSSgam)
library(tidyr)

#coral PCA

PCA = comp2021_Aug2026 |> select(REEF_NAME, REPORT_YEAR, COMP_2021, COVER) |> 
  pivot_wider(values_from = "COVER", names_from = "COMP_2021", values_fn = mean) |> 
  select(-1:-2) |> 
  prcomp()

PCA$sdev/sum(PCA$sdev)*100

biplot(PCA,choices = c(4,5))


# fish PCA

sum_wide <- sum_wide %>%
  dplyr::mutate(cREPORT_YEAR = as.numeric(cREPORT_YEAR))

ihatethis <- sum_wide |>
  ungroup() |>
  select(where(is.numeric), 
         -cREPORT_YEAR,
         -c(SITE_LAT, SITE_LONG, A_SECTOR, OPENORCLOSED, OPENORCLOSED_AFTER2004)) |>
  mutate(across(everything(), ~ tidyr::replace_na(.x, 0))) |>
  select(where(~ sd(.x) > 0)) 

PCA <- prcomp(ihatethis, scale. = TRUE)
summary(PCA)

PCA$sdev/sum(PCA$sdev)*100

biplot(PCA,choices = c(1,2))


#model selection

habitat_wide <- comp2021_Aug2026 %>%
  group_by(A_SECTOR, AIMS_REEF_NAME, cREPORT_YEAR, COMP_2021) %>%
  summarise(COVER = sum(COVER, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = COMP_2021,
              values_from = COVER,
              values_fill = 0,
              names_prefix = "cover_")

fish_total   <- fish_total  %>% 
  dplyr::mutate(cREPORT_YEAR = as.numeric(as.character(cREPORT_YEAR)))

habitat_wide <- habitat_wide %>% 
  mutate(cREPORT_YEAR = as.numeric(as.character(cREPORT_YEAR)))

# joining fih and habitat

fishandhab <- fish_total %>%
  left_join(habitat_wide,
            by = c("AIMS_REEF_NAME", "cREPORT_YEAR"),
            relationship = "many-to-one") %>%
  ungroup() %>%
  as.data.frame()

# model skeleton - need to group habitat and fish types properly

data(fishandhab)
use.dat <- fishandhab %>%
  dplyr::filter(SCIENTIFIC_NAME == "Bolbometopon muricatum") %>%
  na.omit()
use.dat$AIMS_REEF_NAME <- as.factor(use.dat$AIMS_REEF_NAME)
use.dat$cREPORT_YEAR <- as.factor(use.dat$cREPORT_YEAR)
use.dat$OPENORCLOSED <- as.factor(use.dat$OPENORCLOSED)
test.fit <- gam(totalAbundance ~ s(cover_ACBX, k = 3, bs = "cr") + s(AIMS_REEF_NAME, bs = "re"),
                family = tw(), data = use.dat)
summary(test.fit)
model.set <- generate_model_set(
  use.dat = use.dat,
  test.fit = test.fit,
  pred.vars.cont = c("cover_ACBX", "cover_G_POR_M"),
  pred.vars.fact = "OPENORCLOSED",
  linear.vars = "cREPORT_YEAR",
  null.terms = "s(AIMS_REEF_NAME, bs='re')",
  max.predictors = 2,
  k = 3
)
fit.model.set(model.set, parallel = FALSE)

out.list <- fit.model.set(model.set,
                          max.models = 600,
                          parallel = T)

