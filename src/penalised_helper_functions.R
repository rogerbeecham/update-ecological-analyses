# penalised_helper_functions
#
# Author: Removed for peer review
###############################################################################

get_region_neighbours <- function(data, region) {
  return(data %>% filter(region_abbr == region) %>% pull(row))
}

get_state_neighbours <- function(data, state) {
  return(data %>% filter(state_abbr == state) %>% pull(row))
}

add_variables <- function(boot_region, cats) {
  for(i in 1:nrow(cats)) {
    add_row <- TRUE
    for(j in 1:nrow(boot_region)) {
      if(cats$row[i]==boot_region$row[j]) {
        add_row <- FALSE
      }
    }
    if(add_row) {
      boot_region <- boot_region %>% 
        tibble::add_row(
          row=cats$row[i],
          observed=NA,
          upper=NA,
          lower=NA,
          region=boot_region$region[1]
        ) 
    }
  }
  return(boot_region)
}

adjusted_r2 <- function(r2, n, k){
  return(
    1-(((1-r2)*(n-1))/(n-k-1))  
    )
}

do_elastic_boot <- function(data, alpha) {
  explanatory <- as.matrix(data$data %>% select(!!x_1:!!x_n))
  outcome <- as.matrix(data$data %>% select(!!y))
  return(cv.glmnet(explanatory, outcome, alpha=alpha))
}


do_elastic_boot <- function(data, y, x_1, x_n, alpha) {
  explanatory <- as.matrix(data %>% select(!!x_1:!!x_n))
  outcome <- as.matrix(data %>% select(!!y))
  return(cv.glmnet(explanatory, outcome, alpha=alpha))
}

do_penalised_bootstrap <- function(data, region, cats, y, x_1, x_n, alpha) {
  models <- modelr::bootstrap(data, 100, id='boot_num') %>%
      group_by(boot_num) %>%
      mutate(
          model = purrr::map(strap,~do_elastic_boot(.$data, y, x_1, x_n, alpha)),
          coef = purrr::map(model, ~coef(., s="lambda.1se")),
          coef = coef %>% as.matrix %>% purrr::map(broom::tidy)) %>%
          unnest(coef) %>%
            group_by(row) %>%
              summarise(
                lower=quantile(value,0.05, na.rm = TRUE),
                upper=quantile(value,0.95, na.rm = TRUE)) %>%
                  filter(row!="(Intercept)")
    cv_fit_elastic <- glmnet::cv.glmnet(as.matrix(data %>% select(!!x_1:!!x_n)),  as.matrix(data %>% select(!!y)), alpha=alpha)
    observed <- as_tibble(broom::tidy(glmnet::coef.cv.glmnet(cv_fit_elastic, s = "lambda.1se"))) %>%
                  mutate(observed=value, 
                         observed=ifelse(observed==0,observed+0.00001,observed)) %>%
                    select(row, observed) %>%
                      filter(row!="(Intercept)")
    num_vars <- nrow(observed)
    if(num_vars>0)
    {  
      fit <- glmnet::glmnet(as.matrix(data %>% select(!!x_1:!!x_n)),as.matrix(data %>% select(!!y)), alpha=alpha)
      cv_fit <- glmnet::cv.glmnet(as.matrix(data %>% select(!!x_1:!!x_n)),as.matrix(data %>% select(!!y)), alpha=alpha)
      data$fitted <- as.numeric(glmnet::predict.glmnet(fit, newx=as.matrix(data %>% select(!!x_1:!!x_n)), s =  cv_fit$lambda.1se))
      data$obs <- as.numeric(as.matrix(data %>% select(!!y)))
      r2 <- adjusted_r2((data %>% select(obs,fitted) %>% boot::corr())^2, nrow(data), num_vars)
      models <- left_join(models,observed) %>%
                mutate(region = rep(region,nrow(models)),
                       r2 = rep(r2, nrow(models))) %>%
                add_variables(.,cats) %>%
                add_column(ridge_lasso=alpha) %>%
                add_column(outcome=quo_name(y)) 
    }
    else
    {
      models <-  tibble(
        row="degree_educated",
        lower=NA,
        upper=NA,
        observed=NA,
        region=region,
        r2=NA) %>%
      add_variables(.,cats) %>%
        add_column(ridge_lasso=alpha) %>%
        add_column(outcome=quo_name(y)) 
    }
     return(models)
  }
