# AUTHOR: Colin Cookman
# CONTACT: ccookman at gmail dot com
#
library(XML)
library(pdftools)
library(readr)
library(tidyverse)
library(stringr)
library(readtext)
library(lubridate)
# set wd to location of all candidate forms
wd <- getwd()
#
file_list <- list.dirs()
# just select directories with candidate forms in them
candidate_dirs <- file_list[grepl("_", file_list)]
# -- need to check and confirm this isn't missing anything due to unexpected folder naming convention breaks

# exclusion and duplication checks
# excluded <- file_list[!file_list %in% candidate_dirs]
# duplicated <- temp %>% filter(duplicated(data.frame(candidate_dirs)))
#
# -----------------------------------------------------------------------------
for(i in 1:length(candidate_dirs)) {

target <- candidate_dirs[i]
# can set target to candidate_dirs[10000] for a practice import
target <- sub(".", "", target)
wd <- paste0(wd, target)
setwd(wd)

province <- str_split(target, "/", simplify = TRUE)[1,2]
assembly <- str_split(target, "/", simplify = TRUE)[1,3]
constituency_number <- str_split(target, "/", simplify = TRUE)[1,4]
candidate_number <- str_split(target, "/", simplify = TRUE)[1,5]
candidate_number <- str_match(candidate_number, "(?<=\\d\\-)(.*?)(?=\\_)")[1]

FBR <- list.files(
  pattern = "_FBR.pdf$", 
  full.names = TRUE)

NAB <- list.files(
  pattern = "_NAB.pdf$", 
  full.names = TRUE)

SBP <- list.files(
  pattern = "_SBP.pdf$", 
  full.names = TRUE)

# FBR form import -------------------------------------------------------------

FBR_import <- pdf_text(FBR)
FBR_text <- toString(FBR_import)
FBR_text <- read_lines(FBR_text)

CNIC_row <- grep("CNIC\\s*:", FBR_text)
candidate_CNIC_FBR <- trimws(str_split(FBR_text[CNIC_row], ":", simplify = TRUE)[1,2])

Name_row <- grep("Name\\s*:", FBR_text)
candidate_name_FBR <- trimws(str_split(FBR_text[Name_row], ":", simplify = TRUE)[1,2])
  
NTN_row <- grep("NTN\\s*:", FBR_text)
candidate_NTN <- trimws(str_split(FBR_text[NTN_row], ":", simplify = TRUE)[1,2])

NTN_date_row <- grep("NTN Issuance\\s*:", FBR_text)
candidate_NTN_issue <- dmy(trimws(str_split(FBR_text[NTN_date_row], ":", simplify = TRUE)[1,2]))

RTO_row <- grep("Tax Office\\s*:", FBR_text)
candidate_RTO <- trimws(str_split(FBR_text[RTO_row], ":", simplify = TRUE)[1,2])

Tax_info_start <- grep("Summary of Income", FBR_text)
Tax_info_end <- grep("Receipts under FTR", FBR_text)
Tax_info <- trimws(FBR_text[Tax_info_start:Tax_info_end])

tax_year <- c(2015, 2016, 2017)

# THIS PART DOESN'T REPLICATE ACROSS ENTRIES - NEED TO FIX --------------------
# positions aren't consistent across forms in cases where candidates are listed as "Non-Filer" or data is blank
# 
# this code block is a rough fix for the blank data issue but doesn't address Non-Filer, haven't seen yet whether pattern holds otherwise
#
if(length(str_split(trimws(Tax_info)[5], "\\s{2,}", simplify = TRUE)) == 2) {
  candidate_tax_income <- c(NA, NA, NA)
  candidate_tax_receipts <- c(NA, NA, NA)
  candidate_tax_paid <- c(NA, NA, NA)
} else {

# 
candidate_tax_income <- c(str_split(trimws(Tax_info)[5], "\\s{2,}", simplify = TRUE)[1,1],
                          str_split(trimws(Tax_info)[7], "\\s{2,}", simplify = TRUE)[1,1],
                          str_split(trimws(Tax_info)[9], "\\s{2,}", simplify = TRUE)[1,3])

candidate_tax_receipts <- c(str_split(trimws(Tax_info)[5], "\\s{2,}", simplify = TRUE)[1,2],
                          str_split(trimws(Tax_info)[7], "\\s{2,}", simplify = TRUE)[1,2],
                          str_split(trimws(Tax_info)[9], "\\s{2,}", simplify = TRUE)[1,4])

candidate_tax_paid <- c(str_split(trimws(Tax_info)[5], "\\s{2,}", simplify = TRUE)[1,3],
                          str_split(trimws(Tax_info)[7], "\\s{2,}", simplify = TRUE)[1,3],
                          str_split(trimws(Tax_info)[9], "\\s{2,}", simplify = TRUE)[1,5])
}

# NAB form import -------------------------------------------------------------

NAB_import <- pdf_text(NAB)
NAB_text <- toString(NAB_import)
NAB_text <- read_lines(NAB_text)

CNIC_row <- grep("CNIC", NAB_text)
CNIC_MNIC <- trimws(str_split(NAB_text[CNIC_row], ":", simplify = TRUE)[1,2])
candidate_CNIC_NAB <- trimws(str_split(CNIC_MNIC, "/", simplify = TRUE)[1,1]) 
# in theory this should automatically match with the CNIC listed on the FBR form but in at least one case I've found, it does not, so better check
candidate_MNIC_NAB <- trimws(str_split(CNIC_MNIC, "/", simplify = TRUE)[1,2])

Name_row <- grep("Name", NAB_text)
candidate_name_NAB <- trimws(str_split(NAB_text[Name_row], ":", simplify = TRUE)[1,2])

NAB_status_row <- as.integer(CNIC_row + 1):length(NAB_text)
candidate_NAB_status <- trimws(NAB_text[NAB_status_row])
candidate_NAB_status <- paste(candidate_NAB_status, collapse=" ")
# just taking the raw text for now until I can find an example where there may be differing results here (so far all samples I've checked were cleared)

# SBP form import -------------------------------------------------------------

SBP_import <- pdf_text(SBP)
SBP_text <- toString(SBP_import)
SBP_text <- read_lines(SBP_text)

CNIC_row <- grep("CNIC of Candidate:", SBP_text)
candidate_CNIC_SBP <- trimws(str_split(SBP_text[CNIC_row], ":", simplify = TRUE)[1,2])

CNIC_row <- grep("MNIC of Candidate:", SBP_text)
candidate_MNIC_SBP <- trimws(str_split(SBP_text[CNIC_row], ":", simplify = TRUE)[1,2])

Name_row <- grep("Name of Candidate:", SBP_text)

# candidate names missing in some SBP forms
if(length(Name_row) == 0) {
candidate_name_SBP <- NA }
 else {
candidate_name_SBP <- trimws(str_split(SBP_text[Name_row], ":", simplify = TRUE)[1,2])
}

Loan_info_rows <- grep("below:", SBP_text)
Loan_info_rows <- as.integer(Loan_info_rows + 1):length(SBP_text)
candidate_loan_info <- trimws(SBP_text[Loan_info_rows])
candidate_loan_info <- paste(candidate_loan_info, collapse = " ")
# again just taking raw text for now until can work out the possible outputs and patterns in this section

# Combine all form data -------------------------------------------------------

temp <- data.frame(province, assembly, constituency_number, candidate_number, 
         candidate_CNIC_FBR, candidate_CNIC_NAB, candidate_CNIC_SBP,
         candidate_MNIC_NAB, candidate_MNIC_SBP,
         candidate_name_FBR, candidate_name_NAB, candidate_name_SBP,
         candidate_NTN, candidate_NTN_issue, candidate_RTO,
         tax_year, candidate_tax_income, candidate_tax_receipts, candidate_tax_paid,
         candidate_NAB_status,
         candidate_loan_info
         )

# at some point will want to clean up the differing CNIC numbering conventions between SPB (includes dash) and FBR/NAB

# Add to master output and keep a log
if(i == 1) {
  out <- temp
  log <- target
} else {
  out <- rbind(out, temp)
  log <- rbind(log, target)
}

# reset to original wd for next pass
wd <- getwd()

}
