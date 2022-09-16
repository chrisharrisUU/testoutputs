# Load dependencies
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(papaja)) {install.packages("papaja"); library(papaja)}
if (!require(magrittr)) {install.packages("magrittr"); library(magrittr)}
if (!require(BayesFactor)) {install.packages("BayesFactor"); library(BayesFactor)}
if (!require(rlang)) {install.packages("rlang"); library(rlang)}

# Frequentist t-tests
ttest <- function(data, x, y, mu = NA, sub = NA, dir = "two.sided", print = FALSE) {
  # Quotations
  x_en <- enexpr(x)
  y_en <- enexpr(y)
  sub_en <- enexpr(sub)
  
  
  # Tests
  if (is.na(mu)) {
    # Independent two sample t-test
    output <- t.test(eval(parse(text = y_en)) ~ eval(parse(text = x_en)), alternative = dir, data = data)
  } else {
    if (is.na(sub)) {
      # Independent one sample t-test
      output <- data %$%
        t.test(eval(parse(text = y_en)), mu = mu, alternative = dir)
    } else {
      # Independent one sample t-test: subgroup
      output <- data %>%
        filter(!!x_en == !!sub_en) %$%
        t.test(eval(parse(text = y_en)), mu = mu, alternative = dir)
    }
  }
  
  # Effect size
  if (is.na(mu)) {
    # Independent two sample t-test
    output2 <- data %>%
      group_by(!!x_en) %>%
      mutate(diff = !!y_en - mean(!!y_en)) %>%
      mutate(absdiff = diff ^ 2) %>%
      summarize(sum = sum(absdiff),
                n = n(),
                m = mean(!!y_en),
                .groups = "drop_last")
    sd_pooled <- sqrt((output2$sum[1] + output2$sum[2]) / (output2$n[1] + output2$n[2] - 2))
    d <- (output2$m[1] - output2$m[2]) / sd_pooled
    # variance = ((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2 - 2))) * ((n1 + n2) / (n1 + n2 - 2))
    # with n1, n2 being the group sizes and d being the effect size
    v <- ((output2$n[1] + output2$n[2]) / (output2$n[1] * output2$n[2]) + (d^2) / (2 * (output2$n[1] + output2$n[2] - 2))) * ((output2$n[1] + output2$n[2]) / (output2$n[1] + output2$n[2] - 2))
  } else {
    if (is.na(sub)) {
      # Independent one sample t-test
      output2 <- data %>%
        summarize(m = mean(!!y_en),
                  sd = sd(!!y_en),
                  n = n(),
                  .groups = "drop_last")
    } else {
      # Independent one sample t-test: subgroup
      output2 <- data %>%
        filter(!!x_en == !!sub_en) %>%
        summarize(m = mean(!!y_en),
                  sd = sd(!!y_en),
                  n = n(),
                  .groups = "drop_last")
    }
    d <- (output2$m - mu) / output2$sd
    v <- 1 / output2$n + (d^2) / (2 * output2$n)
  }
  
  # Confidence interval around effect size
  lowerCI <- d - sqrt(v) * 1.96
  upperCI <- d + sqrt(v) * 1.96
  
  # Gather and format parameters
  df <- output$parameter %>% round(., 2)
  t <- output$statistic %>% round(., 2) %>% format(., nsmall = 2)
  if (output$p.value < .001) {
    p <- .001
    sign <- " < "
  } else {
    p <- output$p.value %>% round(., 3) %>% format(., nsmall = 2)
    sign <- " = "
  }
  p %<>% as.character() %>% substr(., 2, nchar(.))
  conf <- output$conf.int %>% round(., 2) %>% format(., nsmall = 2)
  d <- d %>% round(., 2) %>% format(., nsmall = 2)
  lowerCI <- lowerCI %>% round(., 2) %>% format(., nsmall = 2)
  upperCI <- upperCI %>% round(., 2) %>% format(., nsmall = 2)
  # output$estimate
  
  # Output
  ifelse(print,
         paste0("*t*(", df, ") = ", t, ", *p*", sign, p, ", *d* = ", d, ", 95% CI [", lowerCI, ", ", upperCI, "]"),
         paste0("t(", df, ") = ", t, ", p", sign, p, ", d = ", d, ", 95% CI [", lowerCI, ", ", upperCI, "]"))
}

# BayesFactor T-Test
# Adapted from Tobias Heycke https://osf.io/q5eak/
printBFt <- function(BF, index = 1, postit = 100000, print = FALSE, dir = NA) {
  test_dir <- rownames(BF@bayesFactor)[1] %>%
    substr(., 15, nchar(.))
  b <- as.vector(BF[index])
  if (test_dir == "-Inf<d<0") {
    h <- "-0"
  } else if (test_dir == "0<d<Inf") {
    h <- "+0"
  } else {
    if (is.na(dir)) {
      if (as.vector(BF[index]) < 1) {
        b <- 1 / b
        h <- "01"
      } else {
        h <- "10"
      }
    } else {
      if (dir == "10") {
        h <- "10"
      } else {
        b <- 1 / b
        h <- "01"
      }
    }
  }
  
  s <- " = "
  if (b > 1000000) {
    b <- 1000000
    s <- " > "
  }
  
  if (as.character(class(BF@numerator[[names(BF@numerator)[index]]])) == "BFoneSample") { 
    rBF <- BayesFactor::ttestBF(BF@data[,1],
                                mu = BF@numerator[[names(BF@numerator)[index]]]@prior$mu,
                                rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale)
  }
  
  if (as.character(class(BF@numerator[[names(BF@numerator)[1]]])) == "BFindepSample") { 
    if (BF@numerator[[names(BF@numerator)[index]]]@identifier$formula == "y ~ group") {
      rBF <- BayesFactor::ttestBF(subset(BF@data,
                                         BF@data[,2] == "x")[,1] ,
                                  subset(BF@data,
                                         BF@data[,2] == "y")[,1],
                                  rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale,
                                  paired = FALSE)
    } else {
      form <- BF@numerator[[names(BF@numerator)[index]]]@identifier$formula
      rBF <- BayesFactor::ttestBF(data = BF@data,
                                  formula = eval(parse(text = form)),
                                  rscale = BF@numerator[[names(BF@numerator)[index]]]@prior$rscale,
                                  paired = FALSE)
    }
  }
  
  post <- BayesFactor::posterior(rBF, index = index, iterations = postit)
  d <- median(post[, "delta"])
  HDI <- coda::HPDinterval(post[, "delta"])
  ifelse(print,
         paste0('*BF~', h, '~*', s, printnum(b), ', ', '*d* = ', printnum(d), ', ', '*95% HDI* [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']'),
         paste0('BF', h, s, printnum(b), ', ', 'd = ', printnum(d), ', ', '95% HDI [', printnum(HDI[1]), ', ', printnum(HDI[2]), ']'))
}

# Bayesfactor Binominal test
printBFb <- function(BF, print = FALSE) {
  b <- as.vector(BF[1])
  
  test_dir <- rownames(BF@bayesFactor)[1] %>%
    substr(.,
           gregexpr("r=", .)[[1]][1] + 4,
           nchar(.))
  if (test_dir == "0<p<0.5") {
    h <- "-0"
  } else if (test_dir == "0.5<p<1") {
    h <- "+0"
  } else {
    if (b < 1) {
      b <- 1 / b
      h <- "01"
    } else {
      h <- "10"
    }
  }
  ifelse(print,
         paste0('*BF~', h, '~*', " = ", printnum(b)),
         paste0('BF', h, " = ", printnum(b)))
  
}
