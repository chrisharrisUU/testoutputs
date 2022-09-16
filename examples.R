### Provide examples of regular output compared to output by the testoutput-function
source("functions.R")

## T-Test----

# Standard output
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>%
  mutate(Species = factor(Species, levels = c("versicolor", "setosa"))) %$%
  t.test(Sepal.Length ~ Species)
# Independent two sample t-test
iris %>%
  filter(Species %in% c("setosa", "versicolor")) %>%
  mutate(Species = factor(Species, levels = c("versicolor", "setosa"))) %>%
  ttest(data = .,
      y = Sepal.Length,
      x = Species)
# Independent one sample t-test
ttest(data = iris,
      y = Sepal.Length,
      x = Species,
      mu = 4.9,
      sub = "setosa",
      dir = "greater")

## Bayesian T-Test----

# APAish output
iris %>%
  filter(Species == "setosa") %$%
  ttestBF(x = Sepal.Length,
          mu = 4.9,
          nullInterval = c(0, Inf)
  ) %>% printBFt()
ttestBF(
  subset(iris,
         Species == "setosa")$Sepal.Length,
  mu = 4.9,
  nullInterval = c(0, Inf)
) %>% printBFt()

# Intext version for RMarkdown
ttestBF(
  subset(iris,
         Species == "setosa")$Sepal.Length,
  mu = 4.9,
  nullInterval = c(0, Inf)
) %>% printBFt(print = TRUE)

# Using formula
iris %>%
  filter(Species %in% c("setosa", "virginica")) %>%
  as.data.frame() %>%
  ttestBF(data = .,
          formula = Sepal.Width ~ Species) %>%
  printBFt()

## Bayesian binominal test----
# Distribution
distr <- iris %>%
  filter(Species != "setosa") %>%
  mutate(bin_Spl_lgth = ifelse(Sepal.Length > median(Sepal.Length), 1, 0)) %$%
  table(Species, bin_Spl_lgth)
proportionBF(y = distr["versicolor", "0"],
             N = distr["versicolor", "1"] + distr["versicolor","0"],
             p = .5,
             rscale = "ultrawide",
             nullInterval = c(.5, 1)) %>%
  printBFb(print = F)
