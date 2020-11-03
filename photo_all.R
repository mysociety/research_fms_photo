#Calculate overall model controlling for other factors

require(multiwayvcov)
require(car)
require(MASS)
require(pscl)
require(broom)

memory.size(max = FALSE)
memory.limit(size = NA)
#rm(list=ls())

# adjust to odds ratio and minus 100
relative_odds <- function(v){
  return(exp(v) - 1)
}

random_values <- function(mean, se, n=10000000){
  return(mean + (rnorm(n) * se))
}

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

get_result_with_ci <- function(model) {
  se = sqrt(diag(vcov(model)))
  intercept_values = random_values(model$coefficients["(Intercept)"], se["(Intercept)"])
  year_values = random_values(model$coefficients["year"], se["year"])
  photo_values = random_values(model$coefficients["photo"], se["photo"])
  
  year_prob = year_values * (2019 - 2007)
  base_prob = intercept_values + year_prob
  photo_prob = base_prob + photo_values
  base_prob = logit2prob(base_prob)
  photo_prob = logit2prob(photo_prob)
  prob_difference = photo_prob - base_prob

  rr = quantile(prob_difference,probs = c(.025,.975))
  
  ndf <- data.frame()
  for (item in list(base_prob, photo_prob, prob_difference)) {
    rr = quantile(item,probs = c(.025,.975))
    ldf <- data.frame(mean = mean(item),
                      lb = rr["2.5%"],
                      ub = rr["97.5%"])
    ndf <- rbind(ndf, ldf)
  }
  rownames(ndf) <- c("base","with photo","difference")

  return(ndf)
}

ds <- read.csv("_data\\photo_experiment_lad_safe.csv",header = TRUE)

ds$fixed_not_council <- ds$fixed - ds$fixed_council

ds <- ds[(ds$council_valid == 1),] #only councils that have had fixes
ds <- ds[(ds$fixed_not_council == 0),] #discard user fixed reports
ds$year = ds$year - 2007 #reposition on 2017 as year 0
ds$month = as.factor(ds$month) #treat months as categorial variables

model <- glm(fixed_council ~ photo + cobrand + SHEF_B + year + month, data = ds, family = "binomial")

c = confint.default(model)


summary(model)

relative_odds(coef(model)["photo"])
lb = relative_odds(c[2])
ub = relative_odds(c[(length(c)/2) + 2])
lb
ub

a = get_result_with_ci(model)
a