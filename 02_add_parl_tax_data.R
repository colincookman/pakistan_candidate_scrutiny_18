# AUTHORS: Colin Cookman + Luke Sonnet
# NOTES: Merges in released data on incumbent MPA and MNAs to check tax records
# and check for incumbents 

library(tabulizer)

# -----
# load data
# -----
dfw <- read.csv("pk_candidate_scrutiny_data_2018_wide.csv", stringsAsFactors = FALSE)

# -----
# Extract parliamentarian data
# -----

parl <- extract_tables("data/2017727974535733TaxDirectory-Parliamentarians2016.pdf")
parl_df <- map_dfr(parl, data.frame)
names(parl_df) <- c("constituency", "name", "CNIC", "parl_tax_paid", "parl_AOP_tax_paid")

parl_df <- parl_df %>%
  filter(!((grepl("SENATE", constituency) | 
              grepl("ASSEMBLY", constituency) | 
              grepl("Tax Paid by AOPs in", parl_AOP_tax_paid)))) %>%
  mutate(
    parl_tax_paid = as.numeric(gsub("-", "0", gsub("\\,", "", parl_tax_paid)))
  ) %>%
  rename(parl_constituency = constituency)

# merge with wide data (no double merges, note same n of rows in dfw and mdf)
mdf <- merge(
  dfw, 
  select(parl_df, CNIC, parl_tax_paid, parl_constituency), 
  by.x = "candidate_CNIC_ECP", 
  by.y = "CNIC",
  all.x = TRUE
)

mdf$incumbent <- !is.na(mdf$parl_constituency)
mdf$candidate_tax_paid_num_2016[is.na(mdf$candidate_tax_paid_num_2016)] <- 0
ggplot(mdf, aes(x = incumbent, y = log(candidate_tax_paid_num_2016+1))) +
  geom_violin()

write.csv(mdf, file = "data/scrutiny_w_incumbents.csv")

# Plot to show 2016 data is v close
ggplot(mdf, aes(x = log(parl_tax_paid + 1), y = log(candidate_tax_paid_num_2016+1))) + 
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

ggplot(mdf, aes(x = log(parl_tax_paid + 1), y = log(candidate_tax_paid_num_2016+1))) + 
  geom_hex(bins = 10) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

