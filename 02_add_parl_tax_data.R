# AUTHORS: Colin Cookman + Luke Sonnet
# NOTES: Merges in released data on incumbent MPA and MNAs to check tax records
# and check for incumbents 

library(tabulizer)
parl <- extract_tables("data/2017727974535733TaxDirectory-Parliamentarians2016.pdf")
parl_df <- map_dfr(parl, data.frame)
names(parl_df) <- c("constituency", "name", "CNIC", "parl_tax_paid", "parl_AOP_tax_paid")

parl_df <- parl_df %>%
  filter(!((grepl("SENATE", constituency) | grepl("ASSEMBLY", constituency) | grepl("Tax Paid by AOPs in", parl_AOP_tax_paid)))) %>%
  mutate(
    type = ifelse(grepl("NA\\-", constituency), "MNA",
                  ifelse(grepl("^P[BSPK]-", constituency), "MPA", "Senator")
    ),
    parl_tax_paid = as.numeric(gsub("-", "0", gsub("\\,", "", parl_tax_paid)))
  ) %>%
  rename(parl_constituency = constituency)

test2 <- merge(candidate_df, select(parl_df, CNIC, parl_tax_paid, parl_constituency), by.x = "candidate_CNIC_ECP", by.y = "CNIC", all.x = TRUE)


with(test2[test2$tax_year == 2016, ], plot(log(parl_tax_paid+1), log(candidate_tax_paid_num+1), col = rgb(0, 0, 0, 0.2)))
abline(a = 0, b  = 1)
