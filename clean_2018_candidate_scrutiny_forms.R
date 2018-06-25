# AUTHORS: Colin Cookman + Luke Sonnet
# CONTACT: ccookman at gmail dot com

# ----------
# Loading packages
# ----------
library(XML)
library(pdftools)
library(tidyverse)
library(readtext)
library(lubridate)

clear_logs <- TRUE
if (clear_logs) {
  cat("", file = "err.log", append = FALSE)
  cat("", file = "missing.log", append = FALSE)
  cat("", file = "success.log", append = FALSE)
}

# ---------
# Build candidate directories
# ----------

# Use project instead of setting directory (CC: create R project at root of both raw data and analysis)
file_list <- list.dirs("2018 Candidate Scrutiny Forms")
# just select directories with candidate forms in them
candidate_dirs <- file_list[grepl("_", file_list)]
# -- need to check and confirm this isn't missing anything due to unexpected folder naming convention breaks

# exclusion and duplication checks
# excluded <- file_list[!file_list %in% candidate_dirs]
# duplicated <- temp %>% filter(duplicated(data.frame(candidate_dirs)))

# ----------
# Helper functions
# ----------

# reads pdf_text and saves any errors or warnings
read_pdf_text <- function(file, target) {
  pdf_text <- tryCatch({
    o <- capture.output(file_import <- pdf_text(file), type = "message")
    if (any(grepl("PDF error", o))) {
      cat(paste0(target, "\t", deparse(substitute(file)), " PDF error\n"),
          file = "err.log",
          append = TRUE)
    }
    return(read_lines(toString(file_import)))
  },
  error = function(e) {
    cat(paste0(target, "\t", deparse(substitute(file)), " read error ", as.character(e), "\n"),
        file = "err.log",
        append = TRUE)
    return(NULL)
  })
}

# ----------
# Function to read individual candidate's data
# ----------
read_candidate_folder <- function(target) {
  #target <- sub(".", "", target)  
  # CC: I think you were just removing the leading slash, maybe?
  # But this just breaks my code as all it does it removes the first character from the target
  
  # Get metadata about candidate -------------------------------------------------------------
  cand_meta <- str_split(target, "/", simplify = TRUE)
  
  meta_dat <- data.frame(
    province = cand_meta[1, 2],
    assembly = cand_meta[1, 3]
  )
  
  if (meta_dat$province == "Minority") {
    meta_dat$constituency_number <- NA
    meta_dat$candidate_number <- str_match(cand_meta[1, 4], "(?<=\\d\\-)(.*?)(?=\\_)")[1]
  } else {
    meta_dat$assembly <- cand_meta[1, 3]
    meta_dat$constituency_number <- cand_meta[1, 4]
    meta_dat$candidate_number <- str_match(cand_meta[1, 5], "(?<=\\d\\-)(.*?)(?=\\_)")[1]
  }
  
  # Get files from each admin. body
  FBR <- list.files(
    path = target, # added this instead of changing directory
    pattern = "_FBR.pdf$", 
    full.names = TRUE)
  
  NAB <- list.files(
    path = target,
    pattern = "_NAB.pdf$", 
    full.names = TRUE)
  
  SBP <- list.files(
    path = target,
    pattern = "_SBP.pdf$", 
    full.names = TRUE)
  
  # CC: Skip for now if empty while it syncs
  if (length(FBR) == 0 | length(NAB) == 0 | length(SBP) == 0) {
    cat(paste0(target, "\tmissing a file\n"), file = "missing.log", append = TRUE)
    return(data.frame())
  }
  
  # FBR form import -------------------------------------------------------------
  FBR_text <- read_pdf_text(FBR, target)
  # Means there was read error
  if (!is.character(FBR_text)) return(data.frame())

  FBR_dat <- data.frame(
    tax_year = 2015:2017
  )
  
  CNIC_row <- grep("CNIC\\s*:", FBR_text)
  FBR_dat$candidate_CNIC_FBR <- trimws(str_split(FBR_text[CNIC_row], ":", simplify = TRUE)[1,2])
  
  Name_row <- grep("Name\\s*:", FBR_text)
  FBR_dat$candidate_name_FBR <- trimws(str_split(FBR_text[Name_row], ":", simplify = TRUE)[1,2])
  
  NTN_row <- grep("NTN\\s*:", FBR_text)
  FBR_dat$candidate_NTN <- trimws(str_split(FBR_text[NTN_row], ":", simplify = TRUE)[1,2])
  
  NTN_date_row <- grep("NTN Issuance\\s*:", FBR_text)
  FBR_dat$candidate_NTN_issue <- dmy(trimws(str_split(FBR_text[NTN_date_row], ":", simplify = TRUE)[1,2]))
  
  RTO_row <- grep("Tax Office\\s*:", FBR_text)
  FBR_dat$candidate_RTO <- trimws(str_split(FBR_text[RTO_row], ":", simplify = TRUE)[1,2])
  
  # Always load with NAs to start
  FBR_dat$candidate_tax_income <-
    FBR_dat$candidate_tax_receipts <-
    FBR_dat$candidate_tax_paid <- 
    FBR_dat$candidate_tax_type <- 
    FBR_dat$candidate_tax_remarks <- rep(NA, 3)
  
  Tax_info_start <- grep("Tax Regime", FBR_text)
  Tax_info_end <- grep("Receipts under FTR", FBR_text)
  Tax_info <- FBR_text[(Tax_info_start+1):(Tax_info_end-1)] # CC: trimming a little closer
  # remove the year and numbering, also remove "remarks" row if it exists, we can deal with later?
  Tax_remarks <- any(grepl("Remarks", Tax_info))
  Tax_info <- gsub("([123]\\s+201[567]|Remarks)", "", Tax_info)
  #Leading_ws <- map_dbl(Tax_info, ~ str_locate(.x, "\\w")[1,1] - 1)
  Tax_info <- trimws(Tax_info)
  #Leading_ws <- Leading_ws[Tax_info != ""]
  Tax_info <- Tax_info[Tax_info != ""]
  # NB: this means that "missing" FRB forms now are just empty vectors
  
  if (length(Tax_info) == 0) {
    FBR_dat$candidate_tax_type <- "missing"
  } else {
    
    # CC: Use purrr to get list of years so that its easy to loop over and apply functions to them
    years <- map(Tax_info, str_split, "\\s{2,}", simplify = TRUE)
    

    if (length(years) > 3) {
      
      # If they have tax remarks, assume they are all rows beyond the first three for now
      if (Tax_remarks) {
        FBR_dat$candidate_tax_remarks <- paste(Tax_info[4:length(Tax_info)], collapse = "\n")
        years <- years[1:3]
      } else {
        # When there are too many rows, the first row to have 3 cols should take everything below
        # that isn't a 0 or "Non-Filer" until you get to another row with three years
        # for (i in length(years):2) {
        #   if (!any(years[[i]] %in% c("0", "Non-Filer")) & ncol(years[[i]]) != 3) {
        #     years[[i-1]][1, 1] <- paste0(years[[i-1]][1, 1], "\n", years[[i]][1, 1])
        #   }
        # }
        # years <- years[1:3]
        cat(paste0(target, "\t", "too many rows in tax\n"), file = "err.log", append = TRUE)
        return(data.frame())
      }

    } else if (length(years) < 3) {
      # All examples showed a missing second year, adding NA, could check trailing WS to be 
      # more programmatic
      years <- c(years[[1]], matrix(rep(NA, 3), 1), years[[2]])
      cat(paste0(target, "\t", "adding row of NA for presumed missing second year\n"), file = "warn.log", append = TRUE)
    }
    
    # check for filers for each year and bind the results together!
    for (i in seq_along(years)) {
      
      year <- years[[i]]

      # Catch "Non-Filer"
      if (any(grepl("Non-Filer", year))) {
        tax_row <- c("Non-Filer", rep(NA, 3))
      } else if (ncol(year) < 3) { 
        # Some rows have too few data columns
        # Some of these are just 0s, so we can safely just fill those as 0
        if (all(year == "0")) {
          tax_row <- c("Filer", rep("0", 3))
        } else {
          
          if (ncol(year) == 2) {
            # If the length is 2, count sequences of characters
            space_seqs <- rle(strsplit(Tax_info[i], "")[[1]])
            
            # If the sequence of spaces is longer than 30, than its cols 1 + 3, with col 2 missing
            if (any(space_seqs$lengths[space_seqs$values == " "] > 40)) {
              tax_row <- c("Filer", year[1], NA, year[2])
              cat(paste0(target, "\t", "guessing some col 2 missing\n"), file = "warn.log", append = TRUE)
            } else {
              # If space in between is X length, then its 1 and 3, if its
              # not, then it could be either 1 and 2 or 2 and 3, because of the
              # trimws above
              
              # two have 1 and 2 and no 3, and one just has a really long third column
              if (any(space_seqs$lengths[space_seqs$values == " "] == 34)) {
                tax_row <- c("Filer", year[1], NA, year[2])
                cat(paste0(target, "\t", "guessing some col 2 missing\n"), file = "warn.log", append = TRUE)
              } else {
                tax_row <- c("Filer", year[1], year[2], NA)
                cat(paste0(target, "\t", "guessing col 3 missing\n"), file = "warn.log", append = TRUE)
              }

            }
          } else {
            # So far all examples are only first cell filled in, if so, can do simple rule where all go to first position, otherwise have to count leading spaces and infer
            tax_row <- c("Filer", year[1], NA, NA)
            cat(paste0(target, "\t", "only 1 col\n"), file = "warn.log", append = TRUE)
          }
        }
      } else {
        tax_row <- c("Filer", year[1, ])
      }
      
      FBR_dat[i, c("candidate_tax_type", "candidate_tax_income", "candidate_tax_receipts", "candidate_tax_paid")] <- tax_row
    }
  }
  
  # NAB form import -------------------------------------------------------------
  # Skip "description" error files for now
  NAB_text <- read_pdf_text(NAB, target)
  if (!is.character(NAB_text)) return(data.frame())
  
  CNIC_row <- grep("CNIC", NAB_text)
  CNIC_MNIC <- trimws(str_split(NAB_text[CNIC_row], ":", simplify = TRUE)[1,2])
  
  NAB_dat <- data.frame(
    candidate_CNIC_NAB = trimws(str_split(CNIC_MNIC, "/", simplify = TRUE)[1,1]),
    # in theory this should automatically match with the CNIC listed on the FBR form but in at least one case I've found, it does not, so better check
    candidate_MNIC_NAB = trimws(str_split(CNIC_MNIC, "/", simplify = TRUE)[1,2])
  )

  Name_row <- grep("Name", NAB_text)
  NAB_dat$candidate_name_NAB <- trimws(str_split(NAB_text[Name_row], ":", simplify = TRUE)[1,2])
  
  NAB_status_row <- as.integer(CNIC_row + 1):length(NAB_text)
  candidate_NAB_status <- trimws(NAB_text[NAB_status_row])
  NAB_dat$candidate_NAB_status <- paste(candidate_NAB_status, collapse=" ")
  # just taking the raw text for now until I can find an example where there may be differing results here (so far all samples I've checked were cleared)
  
  # SBP form import -------------------------------------------------------------
  
  SBP_text <- read_pdf_text(SBP, target)
  if (!is.character(SBP_text)) return(data.frame())
  
  
  CNIC_row <- grep("CNIC of Candidate:", SBP_text)
  MNIC_row <- grep("MNIC of Candidate:", SBP_text)
  
  SBP_dat <- data.frame(
    candidate_CNIC_SBP = trimws(str_split(SBP_text[CNIC_row], ":", simplify = TRUE)[1,2]),
    candidate_MNIC_SBP = trimws(str_split(SBP_text[MNIC_row], ":", simplify = TRUE)[1,2])
  )

  Name_row <- grep("Name of Candidate:", SBP_text)
  
  # candidate names missing in some SBP forms
  if (length(Name_row) == 0) {
    SBP_dat$candidate_name_SBP <- NA 
  } else {
    SBP_dat$candidate_name_SBP <- trimws(str_split(SBP_text[Name_row], ":", simplify = TRUE)[1,2])
  }
  
  Loan_info_rows <- grep("below:", SBP_text)
  Loan_info_rows <- as.integer(Loan_info_rows + 1):length(SBP_text)
  candidate_loan_info <- trimws(SBP_text[Loan_info_rows])
  SBP_dat$candidate_loan_info <- paste(candidate_loan_info, collapse = " ")
  # again just taking raw text for now until can work out the possible outputs and patterns in this section
  
  # Combine all form data -------------------------------------------------------
  
  ret <- data.frame(
    meta_dat,
    FBR_dat,
    NAB_dat,
    SBP_dat,
    # For debugging
    target = target
  )
  # at some point will want to clean up the differing CNIC numbering conventions between SPB (includes dash) and FBR/NAB
  
  cat(paste0(target, "\t", "success\n"), file = "success.log", append = TRUE)
  
  # Reorders data, note `everything()` which will just get the rest
  select(ret,
    names(meta_dat), tax_year, contains("CNIC"), contains("MNIC"),
    contains("name"), everything()
  )
}

# ----------
# Run on all data
# ----------

# Final execution ----------

# Can use for loop like above, but growing data.frames recursively is _very_ slow
# R wants you to use lapply, but let's use purrr, which is the "tidy" way to do this
system.time({dat <- map_dfr(candidate_dirs, read_candidate_folder)})
# 894 seconds
write.csv(dat, file = "candidate_scrutiny_forms_2018.csv", row.names = FALSE)

debugging <- FALSE
if (debugging) {
  # For debugging ----------

  # Lazy debugging
  for (target in candidate_dirs[2000:3000]) {
    print(target)
    dat <- read_candidate_folder(target)
  }

  # Invalid SBP PDF
  target <- "2018 Candidate Scrutiny Forms/KPK/Provincial Assembly/PAKM/PAKM-0022_1720184712651"
  # Forgot what bottom one's case was
  target <- "2018 Candidate Scrutiny Forms/Balochistan/National Assembly/NA-265/NA-265-0019_5440005597555"
  # Multiple rows of text in some boxes
  target <- "2018 Candidate Scrutiny Forms/Balochistan/Provincial Assembly/PB-15/PB-15-0016_5440028686619"
  target <- "2018 Candidate Scrutiny Forms/Punjab/National Assembly/NA-98/NA-98-0006_3810106314423" # first row
  target <- "2018 Candidate Scrutiny Forms/Sindh/Provincial Assembly/PS-49/PS-49-0006_4230189906189" # third row
  target <- "2018 Candidate Scrutiny Forms/Sindh/Provincial Assembly/PS-19/PS-19-0014_4510136091983" # first and second
  target <- "2018 Candidate Scrutiny Forms/Punjab/Provincial Assembly/PP-15/PP-15-0011_3740552959443" # all three rows
  target <- "2018 Candidate Scrutiny Forms/Punjab/Provincial Assembly/PP-104/PP-104-0015_3310690883097" # long
  
  # Missing middle cells, etc...
  target <- "2018 Candidate Scrutiny Forms/Balochistan/Provincial Assembly/PB-13/PB-13-0027_5340345710071"
  target <- "2018 Candidate Scrutiny Forms/KPK/National Assembly/NA-32/NA-32-0002_4200072543509"
  # Both of the above problems
  target <- "2018 Candidate Scrutiny Forms/Balochistan/National Assembly/NA-261/NA-261-0007_5340382191767"
  # Only has first cell, non-zero, non-"Non-filer"
  target <- "2018 Candidate Scrutiny Forms/Punjab/National Assembly/NA-83/NA-83-0010_3410245994991"
  target <- "2018 Candidate Scrutiny Forms/Sindh/National Assembly/NA-200/NA-200-0024_4320364410531"
  target <- "2018 Candidate Scrutiny Forms/Sindh/National Assembly/NA-211/NA-211-0021_4530278910227"
  target <- "2018 Candidate Scrutiny Forms/Sindh/National Assembly/NA-236/NA-236-0013_1550108773695"
  target <- "2018 Candidate Scrutiny Forms/Sindh/Provincial Assembly/PS-12/PS-12-0020_4320173014423"
  # "missing" example
  target <- "2018 Candidate Scrutiny Forms/Balochistan/National Assembly/NABW/NABW-0031_5440021680144"
  # "remarks" empty example
  target <- "2018 Candidate Scrutiny Forms/Balochistan/Provincial Assembly/PABM/PABM-0010_5130121369039"
  # Non-filer with 0s
  target <- "2018 Candidate Scrutiny Forms/Balochistan/National Assembly/NA-272/NA-272-0023_5140169339383"
  # Plain Non-filer
  target <- "2018 Candidate Scrutiny Forms/Balochistan/Provincial Assembly/PB-9/PB-9-0019_5520276709583"
  # too much data in tax form
  target <- "2018 Candidate Scrutiny Forms/Balochistan/Provincial Assembly/PB-15/PB-15-0016_5440028686619"
  # Example 0
  target <- "2018 Candidate Scrutiny Forms/Balochistan/Provincial Assembly/PB-3/PB-3-0016_5620117350297"
  # Has remarks that are meaningful
  target <- "2018 Candidate Scrutiny Forms/KPK/Provincial Assembly/PK-54/PK-54-0009_1610271708743"
  target <- "2018 Candidate Scrutiny Forms/KPK/Provincial Assembly/PK-86/PK-86-0026_6110171350283"
  target <- "2018 Candidate Scrutiny Forms/KPK/Provincial Assembly/PK-91/PK-91-0019_1120104067713"
  target <- "2018 Candidate Scrutiny Forms/Balochistan/Provincial Assembly/PB-5/PB-5-0010_5440074738019"
  # Too long of col 3 for whitespace check to work
  target <- "2018 Candidate Scrutiny Forms/Punjab/National Assembly/NA-138/NA-138-0002_3510237042907"

  # Smarter debugging
  debugonce(read_candidate_folder)
  read_candidate_folder(target)
}
