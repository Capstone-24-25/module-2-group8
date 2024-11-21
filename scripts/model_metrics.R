load(here::here("data/full_clean_claims_bigrams.rda"))
load(here::here("data/clean_claims_tokens.rda"))
load(here::here("results/bi_final_log_fit.rda"))
load(here::here("results/final_log_fit.rda"))
load(here::here("results/m_bi_final_log_fit.rda"))
load(here::here("data/clean1.RData"))
load(here::here("data/clean_claims_tokens.rda"))
load(here::here("data/m_full_clean_claims_bigrams.rda"))

library(tidyverse)
library(tidymodels)


multi_metric <- metric_set(accuracy, sensitivity, specificity)

## Logistic w/ headers
set.seed(3435)

bc_split <- initial_split(claims_clean_tokens, prop = 0.8, strata = bclass)

claim_train <- training(bc_split)
claim_test <- testing(bc_split)


test_preds <- augment(final_log_fit, new_data = claim_test)


rbind(multi_metric(test_preds, truth = bclass, estimate = .pred_class),
      roc_auc(test_preds, truth = bclass, `.pred_N/A: No relevant content.`))

## Bigrams w/ log odds

set.seed(1027)
bi_partitions <- full_clean_claims_bigrams %>% initial_split(prop = 0.8, strata = bclass)

bi_claim_test <- testing(bi_partitions)

bi_preds <- augment(bi_final_log_fit, bi_claim_test)


rbind(multi_metric(bi_preds, truth = bclass, estimate = .pred_class),
      roc_auc(bi_preds, truth = bclass, `.pred_N/A: No relevant content.`))


## Bigrams with probabilities as predictors, multiclass

set.seed(1027)
m_bi_partitions <- m_full_clean_claims_bigrams %>% initial_split(prop = 0.8)

m_bi_claim_test <- testing(m_bi_partitions)

m_bi_preds <- augment(m_bi_final_log_fit, m_bi_claim_test)


multi_metric(m_bi_preds, truth = mclass, estimate = .pred_class)
