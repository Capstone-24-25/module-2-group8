source(here::here("scripts/preprocessing.R"))
library(tidyverse)
library(tidymodels)
load(here::here("data/claims-test.RData"))
load(here::here("results/final_log_fit.rda"))
load(here::here("results/bi_final_log_fit.rda"))
load(here::here("results/m_final_log_fit.rda"))
load(here::here("results/m_bi_final_log_fit.rda"))
load(here::here("data/claims_clean_tokens.rda"))
load(here::here("data/full_clean_claims_bigrams.rda"))
load(here::here("data/m_full_clean_claims_bigrams.rda"))
load(here::here("data/clean_claims_bigrams.rda"))

cleaned_test <- parse_data(claims_test)
cleaned_test[,c('bclass', 'mclass')] <- NA
cleaned_test_tokens <- nlp_fn(cleaned_test)
cleaned_test_bigrams <- bi_nlp_fn(cleaned_test)

# Match cleaned_test_tokens to claims_clean_tokens columns
missing_tokens <- setdiff(colnames(claims_clean_tokens), colnames(cleaned_test_tokens))
cleaned_test_tokens[missing_tokens] <- 0
cleaned_test_tokens <- cleaned_test_tokens[, colnames(claims_clean_tokens), drop = FALSE]

# Match cleaned_test_bigrams to full_clean_claims_bigrams columns
binary_missing_bigrams <- setdiff(colnames(full_clean_claims_bigrams), colnames(cleaned_test_bigrams))
binary_cleaned_test_bigrams <- cleaned_test_bigrams
binary_cleaned_test_bigrams[binary_missing_bigrams] <- 0
binary_cleaned_test_bigrams <- binary_cleaned_test_bigrams[, colnames(full_clean_claims_bigrams), drop = FALSE]
binary_cleaned_test_bigrams <- binary_cleaned_test_bigrams %>% select(-log_odds_pred)

# Match cleaned_test_bigrams to m_full_clean_claims_bigrams columns
multi_missing_bigrams <- setdiff(colnames(m_full_clean_claims_bigrams), colnames(cleaned_test_bigrams))
multi_cleaned_test_bigrams <- cleaned_test_bigrams
multi_cleaned_test_bigrams[multi_missing_bigrams] <- 0
multi_cleaned_test_bigrams <- multi_cleaned_test_bigrams[, colnames(m_full_clean_claims_bigrams), drop = FALSE]
multi_cleaned_test_bigrams <- multi_cleaned_test_bigrams %>% select(-c(`.pred_Potentially unlawful activity`, 
                                                                       `.pred_Possible Fatality`, `.pred_Physical Activity`, 
                                                                       `.pred_N/A: No relevant content.`, mclass))

# Binary Predictions
binary_log_preds <- augment(final_log_fit, new_data = cleaned_test_tokens)

binary_log_preds <- binary_log_preds |> 
  mutate(log_odds_pred = log(`.pred_N/A: No relevant content.`  / `.pred_Relevant claim content` + .01)) |> 
  select(.id, log_odds_pred)

full_cleaned_test_bigrams <- inner_join(binary_cleaned_test_bigrams, binary_log_preds)

binary_bigram_preds <- augment(bi_final_log_fit, new_data = full_cleaned_test_bigrams)

binary_bigram_preds <- binary_bigram_preds %>% 
  mutate(binary_pred_class = .pred_class) %>% 
  select(.id, binary_pred_class, contains(".pred"), - .pred_class)

save(binary_bigram_preds, file = here::here("results/preds-group-binary.RData"))

# Multiclass Predictions
multiclass_log_preds <- augment(m_final_log_fit, new_data = cleaned_test_tokens)

multiclass_log_preds <- multiclass_log_preds %>% 
  select(.id, `.pred_Potentially unlawful activity`, 
         `.pred_Possible Fatality`, `.pred_Physical Activity`, 
         `.pred_N/A: No relevant content.`)

multiclass_full_cleaned_test_bigrams <- inner_join(multi_cleaned_test_bigrams, multiclass_log_preds)

multiclass_bigram_preds <- augment(m_bi_final_log_fit, new_data = multiclass_full_cleaned_test_bigrams)

multiclass_bigram_preds <- multiclass_bigram_preds %>% 
  mutate(multi_pred_class = .pred_class) %>% 
  select(.id, multi_pred_class, contains(".pred"), - .pred_class)

save(multiclass_bigram_preds, file = here::here("results/preds-group-multiclass.RData"))
