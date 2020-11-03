# calculate for each category a model looking at the effectiveness of including photos

require(multiwayvcov)
require(car)
require(MASS)
require(pscl)
require(broom)

memory.size(max = FALSE)
memory.limit(size = NA)
rm(list = ls())


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
  
  ndf <- data.frame()
  for (item in list(base_prob, photo_prob, prob_difference)) {
    rr = quantile(item,probs = c(.025,.975), na.rm = TRUE)
    ldf <- data.frame(mean = mean(item),
                      lb = rr["2.5%"],
                      ub = rr["97.5%"])
    ndf <- rbind(ndf, ldf)
  }
  rownames(ndf) <- c("base","with photo","difference")
  
  return(ndf)
}

ds <- read.csv("_data//photo_experiment_lad_safe.csv",header = TRUE)

ds$fixed_not_council <- ds$fixed - ds$fixed_council

# remove non council reported fixes and reports before first council fix
ds <- ds[(ds$council_valid == 1),]
ds <- ds[(ds$fixed_not_council == 0),]

ds <- ds[!(ds$SHEF_B == "Dangerous Building/Structure"), ] # not enough for regression to work
ds$SHEF_B <- droplevels(ds$SHEF_B)
ds$year = ds$year - 2007
ds$month = as.factor(ds$month)
ds$cobrand <- as.character(ds$cobrand)
ds$cobrand[ds$cobrand == ""] <- "fixmystreet"
ds$cobrand <- as.factor(ds$cobrand)

ndf = data.frame()

options = levels(ds$SHEF_B)

#options = c("Road Surface Defects")

for (category in options) {
  category
  df <- ds[(ds$SHEF_B == category),] 
  model <- glm(fixed_council ~ photo + cobrand + year + month, data = df, family = "binomial")
  
  absolute_rate = get_result_with_ci(model)
  
  c = confint.default(model)
  photo = relative_odds(model$coefficients["photo"])
  
  lb = relative_odds(c[2])
  ub = relative_odds(c[(length(c)/2) + 2])
  p = summary(model)$coefficients[,4]["photo"]
  sig = p < 0.01
  r2 = pR2(model)["McFadden"]
  overall_fix_rate = mean(df$fixed_council)
  fix_rate = mean(df$fixed_council)
  photo_fix_rate = mean(df[(df$photo == 1),]$fixed_council)
  no_photo_fix_rate = mean(df[(df$photo == 0),]$fixed_council)
  levels(df$fixed_council)
  if (sum(df$fixed_council) > 0) {
    t_test = t.test(df$photo ~ df$fixed_council)
    t_p = t_test$p.value   
  } else {
    t_p = 1
  }
  mean_sig = t_p < 1
  ldf <- data.frame(category = category,
                    photo = photo,
                    lb = lb,
                    ub = ub,
                    p = p,
                    sig = sig,
                    r2 = r2,
                    cases = nrow(df),
                    mean_fix_rate = fix_rate,
                    mean_photo_fix_rate = photo_fix_rate,
                    mean_no_photo_fix_rate = no_photo_fix_rate,
                    #mean_diff = photo_fix_rate-no_photo_fix_rate,
                    #mean_diff_p=t_p,
                    #mean_sig=mean_sig,
                    base_prob = unname(absolute_rate["base",]["mean"]),
                    photo_prob = unname(absolute_rate["with photo",]["mean"]),
                    predicted_difference = unname(absolute_rate["difference",]["mean"]),
                    diff_lb = unname(absolute_rate["difference",]["lb"]),
                    diff_ub = unname(absolute_rate["difference",]["ub"])
                    )
  if (sig == TRUE) {
    ndf <- rbind(ndf, ldf)
  }
  
} 

ndf <- ndf[order(-ndf$predicted_difference),]

write.csv(ndf,"_results//categories_exp.csv", row.names = FALSE)
