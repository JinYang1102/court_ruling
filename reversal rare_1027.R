library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

claim_construction <- read_csv("/Users/qiandu/Desktop/2025_summer/reversal_rate/claim_construction_with_cpc_1027.csv")

cpc_cols <- c("patent1_cpc", "patent2_cpc", "patent3_cpc", "patent4_cpc", "patent5_cpc", "patent6_cpc", "patent7_cpc")
reversal_cols <- c(
  "patent1_reverse",
  "patent2_reverse",
  "patent3_reverse",
  "patent4_reverse",
  "patent5_reverse",
  "patent6_reverse",
  "patent7_reverse"
)

cpc_long <- claim_construction %>%
  mutate(rowid = row_number()) %>%
  pivot_longer(
    cols = all_of(cpc_cols),
    names_to = "cpc_col",
    values_to = "cpc"
  )

reversal_long <- claim_construction %>%
  mutate(rowid = row_number()) %>%
  pivot_longer(
    cols = all_of(reversal_cols),
    names_to = "reversal_col",
    values_to = "reversal"
  )

cpc_long <- cpc_long %>%
  group_by(rowid) %>%
  mutate(idx = row_number()) %>%
  ungroup()

reversal_long <- reversal_long %>%
  group_by(rowid) %>%
  mutate(idx = row_number()) %>%
  ungroup()

long_df <- left_join(cpc_long, reversal_long, by = c("rowid", "idx"))

long_df <- long_df %>%
  filter(!is.na(cpc) & cpc != "") %>%
  mutate(
    cpc3 = str_sub(cpc, 1, 3),
    reversal = as.numeric(reversal)
  )

cpc_rate <- long_df %>%
  group_by(cpc3) %>%
  summarise(
    n = n(),
    reversal_count = sum(reversal == 1, na.rm = TRUE),
    reversal_rate = reversal_count / n
  ) %>%
  arrange(desc(reversal_rate))

print(cpc_rate, n = Inf)

write_xlsx(cpc_rate, "/Users/qiandu/Desktop/2025_summer/reversal_rate/reversal_rate_1027.xlsx")
