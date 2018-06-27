# AUTHORS: Colin Cookman + Luke Sonnet
# NOTES: check INVESTIGATE lines for things to check against raw pdfs, TODO for unfinished sections
# ----------

library(tidyverse)

# CC: Why do you have na.strings = ""?
candidate_import <- read.csv("data/candidate_scrutiny_forms_2018.csv", na.strings = "", stringsAsFactors = FALSE)
candidate_df <- candidate_import

# ----------
# Clean candidate metadata
# ----------

# years as factors ------------------------------------------------------------

candidate_df$tax_year <- as.factor(candidate_df$tax_year)

# clean up constituency numbers -----------------------------------------------
# clean up stray parentheses in some constituency numbers due to multiple folders per constituency

candidate_df$constituency_number <- gsub("\\s{1}\\(\\d*\\)", "", candidate_df$constituency_number)

# clean up reserved seats lists -----------------------------------------------
candidate_df$assembly <- as.character(candidate_df$assembly)
candidate_df$constituency_number <- as.character(candidate_df$constituency_number)
candidate_df$constituency_number[candidate_df$assembly == "NAM"] <- "Minority List"
candidate_df$assembly <- gsub("NAM", "National Assembly", candidate_df$assembly)
candidate_df$constituency_number <- gsub("\\w*W$", "Womens List", candidate_df$constituency_number)
candidate_df$constituency_number <- gsub("\\w*M$", "Minority List", candidate_df$constituency_number)
# at least in my current local version, NASW is spread out across a couple folders, corrected here
candidate_df$constituency_number[grepl("NASW", candidate_df$constituency_number)] <- "Womens List"
# at least in available ECP data national assembly minority candidates don't have provincial affiliations
candidate_df$province <- gsub("Minority", "National", candidate_df$province)

# reclaim ECP candidate numbers for minority / women's candidates, just for listing purposes
candidate_df$candidate_number <- as.character(candidate_df$candidate_number)

candidate_df$candidate_number[candidate_df$constituency_number == "Womens List"] <- 
  str_match(str_split(candidate_df$target[candidate_df$constituency_number == "Womens List"], "/", simplify = TRUE)[,5], "(?<=\\-)(.*?)(?=\\_)")[,1]

candidate_df$candidate_number[candidate_df$constituency_number == "Minority List"] <- 
  str_match(str_split(candidate_df$target[candidate_df$constituency_number == "Minority List"], "/", simplify = TRUE)[,5], "(?<=\\-)(.*?)(?=\\_)")[,1]

candidate_df$candidate_number[candidate_df$constituency_number == "Minority List" & candidate_df$province == "National"] <- 
  str_match(str_split(candidate_df$target[candidate_df$constituency_number == "Minority List" & candidate_df$province == "National"], "/", simplify = TRUE)[,4], "(?<=\\-)(.*?)(?=\\_)")[,1]

# concatenate a candidate-constituency code
candidate_df <- candidate_df %>% mutate(
  candidate_code = paste(candidate_df$constituency_number, 
      as.numeric(as.character(candidate_df$candidate_number)), sep = "-")
)

# from this abbreviate the reserved list codes
candidate_df$candidate_code[candidate_df$province == "National" & candidate_df$constituency_number == "Minority List"] <- 
  gsub("Minority List-", "NA-ML-", candidate_df$candidate_code[candidate_df$province == "National" & candidate_df$constituency_number == "Minority List"])

candidate_df$candidate_code[candidate_df$province == "Balochistan" & candidate_df$constituency_number == "Minority List"] <- 
  gsub("Minority List", "PB-ML", candidate_df$candidate_code[candidate_df$province == "Balochistan" & candidate_df$constituency_number == "Minority List"])

candidate_df$candidate_code[candidate_df$province == "KPK" & candidate_df$constituency_number == "Minority List"] <- 
  gsub("Minority List", "PK-ML", candidate_df$candidate_code[candidate_df$province == "KPK" & candidate_df$constituency_number == "Minority List"])

candidate_df$candidate_code[candidate_df$province == "Punjab" & candidate_df$constituency_number == "Minority List"] <- 
  gsub("Minority List", "PP-ML", candidate_df$candidate_code[candidate_df$province == "Punjab" & candidate_df$constituency_number == "Minority List"])

candidate_df$candidate_code[candidate_df$province == "Sindh" & candidate_df$constituency_number == "Minority List"] <- 
  gsub("Minority List", "PS-ML", candidate_df$candidate_code[candidate_df$province == "Sindh" & candidate_df$constituency_number == "Minority List"])

candidate_df$candidate_code[candidate_df$province == "Balochistan" & 
                              candidate_df$assembly == "National Assembly" & 
                              candidate_df$constituency_number == "Womens List"] <- 
  gsub("Womens List", "NA-BW", candidate_df$candidate_code[candidate_df$province == "Balochistan" & 
                                                             candidate_df$assembly == "National Assembly" & 
                                                             candidate_df$constituency_number == "Womens List"]
       )

candidate_df$candidate_code[candidate_df$province == "Balochistan" & 
                              candidate_df$assembly == "Provincial Assembly" & 
                              candidate_df$constituency_number == "Womens List"] <- 
  gsub("Womens List", "PB-WL", candidate_df$candidate_code[candidate_df$province == "Balochistan" & 
                                                             candidate_df$assembly == "Provincial Assembly" & 
                                                             candidate_df$constituency_number == "Womens List"]
  )

candidate_df$candidate_code[candidate_df$province == "KPK" & 
                              candidate_df$assembly == "National Assembly" & 
                              candidate_df$constituency_number == "Womens List"] <- 
  gsub("Womens List", "NA-KW", candidate_df$candidate_code[candidate_df$province == "KPK" & 
                                                             candidate_df$assembly == "National Assembly" & 
                                                             candidate_df$constituency_number == "Womens List"]
  )

candidate_df$candidate_code[candidate_df$province == "KPK" & 
                              candidate_df$assembly == "Provincial Assembly" & 
                              candidate_df$constituency_number == "Womens List"] <- 
  gsub("Womens List", "PK-WL", candidate_df$candidate_code[candidate_df$province == "KPK" & 
                                                             candidate_df$assembly == "Provincial Assembly" & 
                                                             candidate_df$constituency_number == "Womens List"]
  )

candidate_df$candidate_code[candidate_df$province == "Punjab" & 
                              candidate_df$assembly == "National Assembly" & 
                              candidate_df$constituency_number == "Womens List"] <- 
  gsub("Womens List", "NA-PW", candidate_df$candidate_code[candidate_df$province == "Punjab" & 
                                                             candidate_df$assembly == "National Assembly" & 
                                                             candidate_df$constituency_number == "Womens List"]
  )

candidate_df$candidate_code[candidate_df$province == "Punjab" & 
                              candidate_df$assembly == "Provincial Assembly" & 
                              candidate_df$constituency_number == "Womens List"] <- 
  gsub("Womens List", "PP-WL", candidate_df$candidate_code[candidate_df$province == "Punjab" & 
                                                             candidate_df$assembly == "Provincial Assembly" & 
                                                             candidate_df$constituency_number == "Womens List"]
  )

candidate_df$candidate_code[candidate_df$province == "Sindh" & 
                              candidate_df$assembly == "National Assembly" & 
                              candidate_df$constituency_number == "Womens List"] <- 
  gsub("Womens List", "NA-SW", candidate_df$candidate_code[candidate_df$province == "Sindh" & 
                                                             candidate_df$assembly == "National Assembly" & 
                                                             candidate_df$constituency_number == "Womens List"]
  )

candidate_df$candidate_code[candidate_df$province == "Sindh" & 
                              candidate_df$assembly == "Provincial Assembly" & 
                              candidate_df$constituency_number == "Womens List"] <- 
  gsub("Womens List", "PS-WL", candidate_df$candidate_code[candidate_df$province == "Sindh" & 
                                                             candidate_df$assembly == "Provincial Assembly" & 
                                                             candidate_df$constituency_number == "Womens List"]
  )

# strip dashes from CNIC / MNIC numbers ---------------------------------------
# for reference SBP CNIC pattern is 12345-1234567-1, MNIC pattern 123-12-123456, latter may also have alphumerics

candidate_df$candidate_CNIC_SBP <- as.factor(gsub("-", "", candidate_df$candidate_CNIC_SBP))
candidate_df$candidate_MNIC_NAB <- as.factor(gsub("NULL", NA, candidate_df$candidate_MNIC_NAB))
candidate_df$candidate_MNIC_SBP <- as.factor(gsub("-", "", candidate_df$candidate_MNIC_SBP))
candidate_df$candidate_MNIC_SBP <- as.factor(gsub("NULL", NA, candidate_df$candidate_MNIC_SBP))

# setting NA values for missing data ------------------------------------------
# we want to keep '0' observations as distinct from NA/missing data, although conversion to numeric for any calculations will turn zeros into NAs
candidate_df$candidate_name_SBP <- gsub("NA", NA, candidate_df$candidate_name_SBP)
candidate_df$candidate_NTN_issue <- gsub("NA", NA, candidate_df$candidate_NTN_issue)
candidate_df$candidate_tax_remarks <- gsub("NA", NA, candidate_df$candidate_tax_remarks)
candidate_df$candidate_tax_paid <- as.factor(gsub("NA", NA, candidate_df$candidate_tax_paid))

# ----------
# Clean up tax data
# ----------

# INVESTIGATE - ok to set '-' as NA?
candidate_df$candidate_tax_receipts <- as.factor(gsub("-", NA, candidate_df$candidate_tax_receipts))
candidate_df$candidate_tax_receipts <- as.factor(gsub("NA", NA, candidate_df$candidate_tax_receipts))
candidate_df$candidate_tax_income <- as.factor(gsub("-", NA, candidate_df$candidate_tax_income))
candidate_df$candidate_tax_income <- as.factor(gsub("NA", NA, candidate_df$candidate_tax_income))

# setting all unregistered tax filers w/o NTNs to tax type "unregistered" -------
candidate_df$candidate_tax_type <- gsub("missing", "Unregistered", candidate_df$candidate_tax_type)

# note -- following NTNs are NA values (not specifically listed as unregistered but no data)
# 2018 Candidate Scrutiny Forms/KPK/Provincial Assembly/PK-38/PK-38-0017_1310144915515
# 2018 Candidate Scrutiny Forms/Punjab/National Assembly/NA-70/NA-70-0012_3420282747187
# 2018 Candidate Scrutiny Forms/Punjab/Provincial Assembly/PP-174/PP-174-0007_3510290157271

# standardize RTO names -------------------------------------------------------
candidate_df$candidate_RTO <- as.character(candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("Regional Tax Office, Islamabad", "RTO ISLAMABAD", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO Islamabad", "RTO ISLAMABAD", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO Faisalabad", "RTO FAISALABAD", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO Multan", "RTO MULTAN", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO, Multan", "RTO MULTAN", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("LTU-II KARACHI", "LTU KARACHI II", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO-II KARACHI", "RTO KARACHI II", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO-III KARACHI", "RTO KARACHI III", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO-II LAHORE", "RTO LAHORE II", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO \\(Corporate\\) Lahore", "RTO (CORPORATE) LAHORE", candidate_df$candidate_RTO)
# assuming these two are abbreviations?
candidate_df$candidate_RTO <- gsub("RTO FSD", "RTO FAISALABAD", candidate_df$candidate_RTO)
candidate_df$candidate_RTO <- gsub("RTO, Mn", "RTO MULTAN", candidate_df$candidate_RTO)

# clean up tax remarks -------------------------------------------

tax_vars <- c("candidate_tax_income", "candidate_tax_receipts", "candidate_tax_paid")
candidate_df <- candidate_df %>%
  mutate_at(tax_vars,
            funs(gsub("\\,|\\s+", "", .))) %>%
  mutate_at(tax_vars,
            funs(num = as.numeric(.)))


na_if_no_match <- function(.x) {
  if (length(.x) == 0) NA else sum(as.numeric(.x))
}
for (tax_var in tax_vars) {
  na_num <- is.na(candidate_df[[paste0(tax_var, "_num")]])
  candidate_df[[tax_var]][na_num] <-
    map_dbl(
      str_extract_all(candidate_df[[tax_var]][na_num], 
                      "\\d+(\\.\\d{2})?"), 
      na_if_no_match
    )
}

# ----------
# Clean up NAB comments
# ----------

# table(candidate_df$candidate_NAB_status)
candidate_df <- candidate_df %>%
  mutate(
    candidate_NAB_guilty = ifelse(
      grepl(
        "It is certified that no information about( any)? conviction or plea bargain",
        candidate_NAB_status
      ),
      "No Conviction or Plea Bargain",
      "Conviction or Plea Bargain"
    ),
    candidate_NAB_conviction = ifelse(
      grepl(
        "It is certified that information about conviction",
        candidate_NAB_status
      ),
      "Conviction",
      "No conviction"
    ),
    candidate_NAB_plea = ifelse(
      grepl(
        "It is certified that information about [pP]lea [bB]argain",
        candidate_NAB_status
      ),
      "Plea bargain",
      "No plea bargain"
    ),
    candidate_NAB_remarks = str_extract(
      candidate_NAB_status,
      "(?<=Remarks\\: ).*?(?=\\sShakeel)"
    ),
    # Following isn't perfect
    candidate_NAB_accused = ifelse(
      grepl("is( an)? accused|pending", candidate_NAB_remarks),
      "Accused or pending",
      "Not accused or pending"
    )
  )

# with(candidate_df, table(candidate_NAB_conviction, candidate_NAB_guilty, candidate_NAB_plea))
# table(candidate_df$candidate_NAB_remarks)
# with(candidate_df, table(candidate_NAB_accused, candidate_NAB_conviction))
# with(candidate_df, table(candidate_NAB_accused, candidate_NAB_guilty))
# candidate_df$candidate_NAB_status[candidate_df$candidate_NAB_guilty == "Conviction or Plea Bargain" & candidate_df$candidate_NAB_conviction == "No conviction"]
# candidate_df$candidate_NAB_remarks[candidate_df$candidate_NAB_guilty == "Conviction or Plea Bargain" & candidate_df$candidate_NAB_accused == "Not accused"]
# unique(candidate_df$candidate_NAB_remarks[candidate_df$candidate_NAB_guilty == "No Conviction or Plea Bargain" & candidate_df$candidate_NAB_accused == "Not accused"])
# candidate_df$candidate_NAB_remarks[candidate_df$candidate_NAB_guilty == "Conviction or Plea Bargain" & candidate_df$candidate_NAB_accused == "Not accused"]
# unique(candidate_df$candidate_NAB_status[candidate_df$candidate_NAB_guilty == "Conviction or Plea Bargain"])

# ----------
# SBP cleaning
# ----------

# table(table(candidate_df$candidate_loan_info))
# table(candidate_df$candidate_loan_info)[table(candidate_df$candidate_loan_info) > 20000] # nada
# table(candidate_df$candidate_loan_info)[table(candidate_df$candidate_loan_info) == 8616] # nada
# table(candidate_df$candidate_loan_info)[table(candidate_df$candidate_loan_info) > 3 & table(candidate_df$candidate_loan_info) < 8000] # nada
# table(table(candidate_df$candidate_loan_info))

temp_loan <-
  gsub(
    "\\(Rs\\. in Million\\)\\s+CNIC\\/Name\\s+Relation( with)?\\s+(FI )?Name( of Company\\s+FI Name)?\\s+Overdue\\s+Writeoff (Candidate|with)?",
    "",
    candidate_df$candidate_loan_info
  ) %>%
  gsub(
    "Further\\, candidate/spouse/dependent is also director\\/owner of following companies having overdue \\/write off amounting to Rs 2 million and above for last one year",
    "",
    .
  ) %>%
  str_split("\\:\\-") %>%
  map_dfr(~ data.frame(candidate_personal_loan = trimws(.x[1]), candidate_business_loan = trimws(.x[2])))
candidate_df <- bind_cols(candidate_df, temp_loan)
# TODO actually extract loan data

# check if FBR-NAB names match ------------------------------------------------

candidate_df <- candidate_df %>% mutate(
  urdu_name_match = ifelse(as.character(candidate_name_FBR) != as.character(candidate_name_NAB), "Urdu Name Mismatch", "Name Match")
)

# many apparent name mismatches - on quick visual scan source appears to mostly be typos or nonstandardized name spelling

# check if FBR-NAB-SBP CNICs match --------------------------------------------
# INVESTIGATE -- there are pdf encoding issues with these CNIC strings - string reading / copying from original pdfs is not correct

candidate_df <- candidate_df %>% mutate(
  CNIC_FBR_NAB_match = ifelse(as.character(candidate_CNIC_FBR) != as.character(candidate_CNIC_NAB), "CNIC Mismatch", "CNIC Match"),
  CNIC_FBR_SBP_match = ifelse(as.character(candidate_CNIC_FBR) != as.character(candidate_CNIC_SBP), "CNIC Mismatch", "CNIC Match"),
  CNIC_NAB_SBP_match = ifelse(as.character(candidate_CNIC_NAB) != as.character(candidate_CNIC_SBP), "CNIC Mismatch", "CNIC Match")
)

# in almost all cases issue appears to be FBR not matching with NAB/SBP due to encoding errors - 
# only two unique cases of NAB-SBP CNICs not matching (PP-32-1 and PP-32-2, visual check of FBR + NAB forms shows FBR-SBP versions match, NAB the outlier)

# check that NAB-SBP MNICs match ----------------------------------------------

candidate_df <- candidate_df %>% mutate(
  MNIC_match = ifelse(as.character(candidate_MNIC_NAB) != as.character(candidate_MNIC_SBP), "MNIC Mismatch", "MNIC Match")
)

# TODO INVESTIGATE mismatches (just 9 unique cases)
# CC: btw here is a nice way to open some pdfs quickly
map(candidate_df$target[candidate_df$MNIC_match == "MNIC Mismatch"], open)

# establish candidate UIDs to identify multi-constituency contestants ------------

uniques <- candidate_df[!duplicated(candidate_df$candidate_code), 1:30]
duplicate_SBP_CNIC <- uniques[(duplicated(uniques$candidate_CNIC_SBP) | duplicated(uniques$candidate_CNIC_SBP, fromLast = TRUE)), 1:30]

# identify candidates that contested multiple constituencies, or otherwise candidates whose names do not match despite sharing identical CNICs
duplicate_SBP_CNIC$candidate_name_FBR <- as.character(duplicate_SBP_CNIC$candidate_name_FBR)
duplicate_SBP_CNIC$candidate_name_NAB <- as.character(duplicate_SBP_CNIC$candidate_name_NAB)
duplicate_SBP_CNIC$candidate_CNIC_SBP <- as.character(duplicate_SBP_CNIC$candidate_CNIC_SBP)

duplicate_SBP_CNIC <- dplyr::arrange(duplicate_SBP_CNIC, desc(candidate_CNIC_SBP))

duplicate_SBP_CNIC <- duplicate_SBP_CNIC %>% group_by(candidate_CNIC_SBP) %>% 
  mutate(
    multi_candidate_FBR = ifelse(candidate_name_FBR == lag(candidate_name_FBR), "Multi-Candidate", "Mismatch"),
    multi_candidate_NAB = ifelse(candidate_name_NAB == lag(candidate_name_NAB), "Multi-Candidate", "Mismatch")
    )

# multiple typos in FBR names are throwing mismatches but only NAB name mismatch (candidate_code PS-35-3) is due to first name / last name reversal
# otherwise all repeat SBP CNICs in dataset are the same candidate contesting multiple races
# unnecessary to create new UIDs since CNICs are in fact unique

multi_candidate <- dplyr::select(duplicate_SBP_CNIC, candidate_CNIC_SBP)
multi_candidate <- unique(multi_candidate)
multi_candidate$multi_candidate <- "YES"

candidate_df <- left_join(candidate_df, multi_candidate, by = "candidate_CNIC_SBP")

# TODO -- confirm to see if this joined properly - 2751 unique multi-candidate CNICs but seeing 2752 uniques, 1 NA introduced somehow?
summary(unique(as.factor(duplicate_SBP_CNIC$candidate_CNIC_SBP)))
summary(unique(as.factor(candidate_df$candidate_CNIC_SBP[candidate_df$multi_candidate == "YES"])))

# reorder columns -------------------------------------------------------------
candidate_df <- dplyr::select(candidate_df,
  candidate_code, province, assembly, constituency_number, candidate_number,
  candidate_CNIC_FBR, candidate_CNIC_NAB, candidate_CNIC_SBP,
  candidate_MNIC_NAB, candidate_MNIC_SBP, candidate_NTN,
  candidate_name_FBR, candidate_name_NAB, candidate_name_SBP,
  candidate_tax_type, candidate_NTN_issue, candidate_RTO,
  candidate_tax_paid, candidate_tax_paid_num, 
  candidate_tax_receipts, candidate_tax_receipts_num,
  candidate_tax_income, candidate_tax_income_num,
  candidate_tax_remarks,
  candidate_NAB_guilty, candidate_NAB_conviction, candidate_NAB_plea, candidate_NAB_accused,
  candidate_NAB_remarks,
  candidate_personal_loan, candidate_business_loan,
  multi_candidate, 
  urdu_name_match, 
  CNIC_FBR_NAB_match, CNIC_FBR_SBP_match, CNIC_NAB_SBP_match,
  MNIC_match,
  target
)

candidate_df <- dplyr::arrange(candidate_df, candidate_code)

# TODO -- possible variables to drop from final:
# FBR and NAB CNICs if we trust SBP to be most accurate
# CNIC_ and MNIC_ match checks
# FBR Urdu name (multiple apparent match errors with NAB)
# urdu_name_match if we drop FBR
# SBP name depending on transliteration results of NAB name


# re-write csv for final output -----------------------------------------------

write.csv(candidate_df, file = "pk_candidate_scrutiny_data_2018.csv", row.names = FALSE)
