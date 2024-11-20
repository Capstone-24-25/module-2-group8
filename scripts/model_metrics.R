load(here::here("data/full_clean_claims_bigrams.rda"))
load(here::here("data/clean_claims_tokens.rda"))
load(here::here("results/bi_final_log_fit.rda"))

library(tidyverse)
library(tidymodels)

set.seed(1027)
bi_partitions <- full_clean_claims_bigrams %>% initial_split(prop = 0.8, strata = bclass)

bi_claim_test <- testing(bi_partitions)

bi_preds <- augment(bi_final_log_fit, bi_claim_test)

multi_metric <- metric_set(accuracy, sensitivity, specificity)

rbind(multi_metric(bi_preds, truth = bclass, estimate = .pred_class),
      roc_auc(bi_preds, truth = bclass, `.pred_N/A: No relevant content.`))
