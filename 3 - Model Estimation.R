########################################################################
########################### Load Packages ##############################
########################################################################

pacman::p_load(
  "MASS", # Ordered Logit Models
  "sandwich", # Clustered Robust Standard Errors
  "lmtest", # Calculate Models with Robust Standard Errors
  "broom", # Tidy Models
  "marginaleffects", # Marginal Effects
  install = FALSE
)

########################################################################
########################## Estimate Models #############################
########################################################################

# Define Model Parameters
outcomes <- c("conf.personal", "conf.county", "conf.state", "conf.nation")

treatments <- c("vsu.dc", "vsu.kg", "vsu.dva", "any.change")

adjustment <- c("percent.bachelor + med.age + percent.white + l.med.income + per.gop +
                birthyr + inv.race + gender + inv.educ + ideo5")

final <- final %>%
  # Remove "I Don't Know" Responses for Confidence Questions
  filter(conf.personal %in% 1:4 & conf.county %in% 1:4 & conf.state %in% 1:4 & conf.nation %in% 1:4) %>%
  # Re-Order Confidence Variables So That Higher = More Election Confidence
  mutate(conf.personal = case_when(
    conf.personal == 1 ~ 4,
    conf.personal == 2 ~ 3,
    conf.personal == 3 ~ 2,
    conf.personal == 4 ~ 1
  ),
  conf.county = case_when(
    conf.county == 1 ~ 4,
    conf.county == 2 ~ 3,
    conf.county == 3 ~ 2,
    conf.county == 4 ~ 1
  ),
  conf.state = case_when(
    conf.state == 1 ~ 4,
    conf.state == 2 ~ 3,
    conf.state == 3 ~ 2,
    conf.state == 4 ~ 1
  ),
  conf.nation = case_when(
    conf.nation == 1 ~ 4,
    conf.nation == 2 ~ 3,
    conf.nation == 3 ~ 2,
    conf.nation == 4 ~ 1
  )) %>%
  # Convert Outcomes to Factors
  mutate(
    conf.personal = as.factor(conf.personal),
    conf.county = as.factor(conf.county),
    conf.state = as.factor(conf.state),
    conf.nation = as.factor(conf.nation)
  )
  
# Pooled Regression Adjustment and CRSEs
model.list <- list()
ame.list <- list()

for (outcome in outcomes) {
  
  pooled.crse[[outcome]] <- list()
  
  for (treatment in treatments) {
    
    # Fit the Model
    model <- polr(as.formula(paste(outcome, "~", treatment, "+", adjustment)), data = final)
    
    # Compute Clustered Robust Standard Errors (df = n - k - 1 / 2412 - 11 - 1)
    model.robust <- coeftest(model, vcov = vcovCL, type = "HC1", df = 2400, cluster = ~FIPSCode)
    
    # Store Results
    model.name <- paste(outcome, treatment, sep = ".")
    model.list[[model.name]] <- model.robust
    
    # Calculate Average Marginal Effects
    ame.robust <- avg_comparisons(model,
                                  variables = treatment,
                                  vcov = ~FIPSCode)
    
    # Store Results
    ame.name <- paste(outcome, treatment, sep = ".")
    ame.list[[ame.name]] <- ame.robust
  }
  
}

########################################################################
###################### Store Results Tabularly #########################
########################################################################

########################################################################
################### Estimate Sensitivity Analysis ######################
########################################################################