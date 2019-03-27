# multilevel_helper_functions
#
# Author: Removed for peer review
###############################################################################

library(lme4)                  
library(piecewiseSEM)          
# Group means: LAs nested within regions. From this calculate Variance Partition Coefficient.
lmer_null <- function(data, y, level2) {
  model <- data %>% 
      select(y=!! y, level2= !! level2)  %>%
        lmer(y ~ (1| level2), data = ., REML = FALSE)
  return(model)
}
# Intercept-only model: assume that the relationship between Leave and explanatory variables is the same in each region.
lmer_intercept <- function(data, y, x, level2) {
  model <- data %>% 
    select(y= !!y, x= !!x, level2= !!level2)  %>%
      lmer(y ~ x + (1 | level2), data = ., REML = FALSE)
  return(model)
}
# Slope model: allow the relationships also to vary across each region. 
lmer_slope <- function(data, y, x, level2) {
  print(x)
  model <- data %>% 
    select(y= !!y, x= !!x, level2 = !!level2)  %>%
      lmer(y ~ x + (1 + x | level2), data = ., REML = FALSE)
  return(model)
}
# Fetches summary statistics for multi-level models.
get_randoms_fitted <- function(intercept, slope, data, y, x, level2){
  fits_intercept <- sem.model.fits(list(intercept), data) 
  fits_slope <- sem.model.fits(list(slope), data) 
  likelihood <- anova(intercept, slope)$`Pr(>Chisq)`[2]
  return(
    data_frame(
      var_name=x,
      conditional_intercept=fits_intercept$Conditional,
      marginal_intercept=fits_intercept$Marginal,
      conditional_slope=fits_slope$Conditional,
      marginal_slope=fits_slope$Marginal,
      likelihood=likelihood,
      corr=as.data.frame(VarCorr(slope)) %>% filter(grp=="level2", var2=="x")  %>% .$sdcor,
      fitted_slope=fitted(slope),
      fitted_intercept=fitted(intercept),
      outcome=data %>% pull(!! y),
      level2=data %>% pull(!! level2),
      var_values=data %>% pull(x)
    )
  )
}
get_randoms <- function(intercept, slope, data, x){
  fits_intercept <- sem.model.fits(list(intercept), data) 
  fits_slope <- sem.model.fits(list(slope), data) 
  likelihood <- anova(intercept, slope)$`Pr(>Chisq)`[2]
  summary <- inner_join(get_summary(intercept, "intercept", x), get_summary(slope, "slope", x)) %>%
    mutate(conditional_intercept=fits_intercept$Conditional,
           marginal_intercept=fits_intercept$Marginal,
           conditional_slope=fits_slope$Conditional,
           marginal_slope=fits_slope$Marginal,
           likelihood=likelihood,
           corr=as.data.frame(VarCorr(slope)) %>% filter(grp=="level2", var2=="x")  %>% .$sdcor
    )
  return(summary)
}
# Fetches level 2 residuals and CIs for random intecept model.
library(rlang)
get_summary <- function(model, type, x) {
  u0 <- ranef(model, condVar = TRUE)
  u0$level2$x <- NULL
  sqrt(c(attr(u0[[1]],"postVar")))
  if(type=="intercept") {
    u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])
  }
  else {
    u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])[1,]
  }
  id <- rownames(u0[[1]])
  tab <- cbind(id, u0[[1]], u0se)
  colnames(tab) <- c("id", "u0", "u0se")
  tab <- tab %>% mutate(upper = u0+(1.96*u0se),
                        lower = u0-(1.96*u0se),
                        x_var=x) %>%
    setNames(c(names(.)[1], paste0(names(.)[-1],paste0("_",type)))) 
}
# Null model vs random intercept model.
get_variances <- function(data, y, x, level2) {
  if(is.null(x)) {
    model <- lmer_null(data, y, level2)
  } else {
    model <- lmer_intercept(data, y, x, level2)
  }
  between <- as.data.frame(VarCorr(model)) %>% filter(grp=="level2") %>% .$vcov
  within <- as.data.frame(VarCorr(model)) %>% filter(grp=="Residual") %>% .$vcov
  p_region <- between/(between+within)
  return(
    data_frame(
      type=ifelse(is.null(x),"null", x),
      within=within,
      between=between,
      total=within+between,
      p_region=p_region)
  )
}