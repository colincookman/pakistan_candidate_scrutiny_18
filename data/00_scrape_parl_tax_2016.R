library(tabulizer)
library(tidyverse)

# -----
# Extract parliamentarian data
# -----

parl <- extract_tables("data/2017727974535733TaxDirectory-Parliamentarians2016.pdf")
parl_df <- map_dfr(parl, data.frame)
names(parl_df) <- c("constituency", "name", "CNIC", "parl_tax_paid", "parl_aop_tax_paid")

parl_df <- parl_df %>%
  filter(!((grepl("SENATE", constituency) | 
              grepl("ASSEMBLY", constituency) | 
              grepl("Tax Paid by AOPs in", parl_aop_tax_paid)))) %>%
  mutate(
    parl_tax_paid = as.numeric(gsub("-", "0", gsub("\\,", "", parl_tax_paid))),
    parl_aop_tax_paid = as.numeric(gsub("-", "0", gsub("\\,", "", parl_aop_tax_paid)))
  ) %>%
  rename(parl_incumbent_constituency_2016 = constituency,
         parl_tax_paid_2016 = parl_tax_paid,
         parl_aop_tax_paid_2016 = parl_aop_tax_paid,
         parl_incumbent_name = name)

parl_df$parl_inc_chamber <- NA
parl_df$parl_inc_chamber[!grepl("\\-", parl_df$parl_incumbent_constituency_2016)] <- "Senate"
parl_df$parl_inc_chamber[grepl("(BALOCHESTAN|KPK|PUNJAB|SINDH)\\-", parl_df$parl_incumbent_constituency_2016)] <- "NA-Women"
parl_df$parl_inc_chamber[grepl("NON\\-MUSLIM\\-", parl_df$parl_incumbent_constituency_2016)] <- "NA-Minority"
parl_df$parl_inc_chamber[grepl("NA\\-", parl_df$parl_incumbent_constituency_2016)] <- "NA"
parl_df$parl_inc_chamber[grepl("NM\\-6[45]", parl_df$parl_incumbent_constituency_2016)] <- "PB-Minority"
parl_df$parl_inc_chamber[grepl("PBW\\-", parl_df$parl_incumbent_constituency_2016)] <- "PB-Women"
parl_df$parl_inc_chamber[grepl("PB\\-", parl_df$parl_incumbent_constituency_2016)] <- "PB"
parl_df$parl_inc_chamber[grepl("PK\\-", parl_df$parl_incumbent_constituency_2016)] <- "PK"
parl_df$parl_inc_chamber[grepl("WR\\-", parl_df$parl_incumbent_constituency_2016)] <- "PK-Women"
parl_df$parl_inc_chamber[grepl("MR\\-", parl_df$parl_incumbent_constituency_2016)] <- "PK-Minority"
parl_df$parl_inc_chamber[grepl("PP\\-", parl_df$parl_incumbent_constituency_2016)] <- "PP"
parl_df$parl_inc_chamber[grepl("W\\-", parl_df$parl_incumbent_constituency_2016)] <- "PP-Women"
parl_df$parl_inc_chamber[grepl("NM\\-3[67][0-9]", parl_df$parl_incumbent_constituency_2016)] <- "PP-Minority"
parl_df$parl_inc_chamber[grepl("PS\\-", parl_df$parl_incumbent_constituency_2016)] <- "PS"
parl_df$parl_inc_chamber[grepl("RSW\\-", parl_df$parl_incumbent_constituency_2016)] <- "PS-Women"
parl_df$parl_inc_chamber[grepl("RSM\\-", parl_df$parl_incumbent_constituency_2016)] <- "PS-Minority"

parl_df$parl_inc_province <- NA
parl_df$parl_inc_province[parl_df$parl_inc_chamber == "Senate"] <- parl_df$parl_incumbent_constituency_2016[parl_df$parl_inc_chamber == "Senate"]
parl_df$parl_inc_province[substr(parl_df$parl_inc_chamber, 1, 2) == "PB"] <- "BALOCHISTAN"
parl_df$parl_inc_province[substr(parl_df$parl_inc_chamber, 1, 2) == "PP"] <- "PUNJAB"
parl_df$parl_inc_province[substr(parl_df$parl_inc_chamber, 1, 2) == "PK"] <- "KPK"
parl_df$parl_inc_province[substr(parl_df$parl_inc_chamber, 1, 2) == "PS"] <- "SINDH"

parl_df$parl_inc_type <- ifelse(grepl("Senate", parl_df$parl_inc_chamber), "Senator",
                                ifelse(grepl("NA", parl_df$parl_inc_chamber), "MNA", "MPA"))

parl_df$parl_incumbent_tax_2016 <- 1

# -----
# Write parliamentarian data
# -----

#head(parl_df)
write.csv(parl_df, file = "data/cleaned_parliamentary_2016.csv", row.names = FALSE)
