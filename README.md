# Pakistan 2018 General Elections Candidate Scrutiny Forms

This repository hosts open source R code used to scan, consolidate, tidy, and clean candidate scrutiny forms released by the Election Commission of Pakistan for prospective candidates for Pakistan's 2018 national and provincial assembly elections. The scrutiny forms consist of data released on tax payments from the Federal Board of Revenue (FBR), corruption cases from the the National Accountability Bureau (NAB), and oustanding loans from the State Bank of Pakistan (SBP).

The final output after cleaning is also included as [pk_candidate_scrutiny_data_2018.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/pk_candidate_scrutiny_data_2018.csv), or in wide format (collapsing three reported tax-year rows per candidate into a single row) as [pk_candidate_scrutiny_data_2018_wide.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/pk_candidate_scrutiny_data_2018_wide.csv).

Please note that this code is still a work in progress and data outputs hosted here may be incomplete. For questions, suggestions, or to contribute, please leave an issue here or contact the contributors, Luke Sonnet and Colin Cookman.

## Data
The raw files used as a data source are approximately 10.3 GB in size and too large to host in this Github repository; the [data subfolder](https://github.com/colincookman/pakistan_2018_candidates/tree/master/data) provides information on alternative mirror hosts that can be used to obtain these original raw pdfs. We also draw on data posted by the FBR about the tax records of parliamentarians in 2016, [available on the FBR's page here](http://download1.fbr.gov.pk/Docs/2017727974535733TaxDirectory-Parliamentarians2016.pdf) and in the [data subfolder](https://github.com/colincookman/pakistan_2018_candidates/tree/master/data) of this repository.

## Code replication

Once the data are downloaded to `data/2018 Candidate Scrutiny Forms` following the instructions in the [data subfolder](https://github.com/colincookman/pakistan_2018_candidates/tree/master/data), then you can:

* Run `00_scrape_scrutiny_forms.R` to read from PDFs and create intermediary data
* Run `01_clean_scrutiny_data.R` to clean up and standardize the scraped data

You can also regenerate the cleaned incumbent parliamentarian data, used in `01_clean_scrutiny_data.R`, by running `data/00_scrape_parl_tax_2016.R`

## Scope and possible gaps in data
Pakistani election law does not impose residency requirements for candidacy filings and allows individual candidates to contest multiple seats simultaneously, within and across assemblies and provinces. As of the initial data release the ECP provided information for 19397 candidacy filings and 15907 unique candidates (as identified by accompanying Computerized National ID Card records). Note that in some cases individual records reported by the FBR, NAB, and SBP may not be consistent across multiple constituency filings.

### Candidacy filing counts based on available data
| province    | assembly            | direct_seats | womens_seats | minority_seats | 
|-------------|---------------------|--------------|--------------|----------------| 
| Balochistan | National Assembly   | 374          | 40           | NA             | 
| Balochistan | Provincial Assembly | 1302         | 13           | 24             | 
| KPK         | National Assembly   | 1076         | 90           | NA             | 
| KPK         | Provincial Assembly | 1959         | 177          | 59             | 
| National    | National Assembly   | NA           | NA           | 215            | 
| Punjab      | National Assembly   | 2428         | 172          | NA             | 
| Punjab      | Provincial Assembly | 6234         | 668          | 141            | 
| Sindh       | National Assembly   | 1181         | 178          | NA             | 
| Sindh       | Provincial Assembly | 2916         | 94           | 56             | 

(A constituency-level summary count of candidacy filing reports can be found [here](https://github.com/colincookman/pakistan_2018_candidates/blob/master/data/constituency_filing_count.csv) in the data folder.)

The ECP had [previously reported](https://www.ecp.gov.pk/PrintDocument.aspx?PressId=55295&type=Image) on June 18 2018 ([Wayback Ref](https://web.archive.org/web/20180627164802/https://www.ecp.gov.pk/PrintDocument.aspx?PressId=55295&type=Image)) that 21482 nomination papers had been filed. While we cannot account for the discrepancy or identify missing candidates at this stage, based on the ECP's earlier aggregate figures the following number of candidacy filings may be missing from the current dataset (or not accounted for in the earlier ECP statement in cases where more records were available):

### ECP reported filings (Difference with available data)
| province    | assembly            | direct_seats | womens_seats | minority_seats | 
|-------------|---------------------|--------------|--------------|----------------| 
| Balochistan | National Assembly   | 435 (-61)    | 36 (+4)      | NA             | 
| Balochistan | Provincial Assembly | 1400 (-98)   | 116 (-103)   | 56 (-32)       | 
| KPK         | National Assembly   | 992 (+84)    | 88 (+2)      | NA             | 
| KPK         | Provincial Assembly | 1920 (+39)   | 262 (-85)    | 73 (-14)       | 
| National    | National Assembly   | NA           | NA           | 154 (+61)      | 
| Punjab      | National Assembly   | 2700 (-272)  | 236 (-64)    | NA             | 
| Punjab      | Provincial Assembly | 6747 (-513)  | 664 (+4)     | 232 (-91)      | 
| Sindh       | National Assembly   | 1346 (-165)  | 76 (+102)    | NA             | 
| Sindh       | Provincial Assembly | 3626 (-710)  | 213 (-119)   | 110 (-54)      | 

A final list of candidates following the process of scrutiny, disqualification, appeal, and withdrawal is currently [scheduled for release](https://www.ecp.gov.pk/PrintDocument.aspx?PressId=55262&type=Image) by the ECP on June 30 2018 which may allow for some retroactive analysis to identify missing filing data. 

As a possible focus for future investigation, the [filing_sequence_gaps.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/data/filing_sequence_gaps.csv) file in the data subfolder identifies candidate numbers missing in the available candidate number sequence as released by the ECP.

## Caveats
This dataset is being presented to encourage broader open data sharing among the community of analysts on Pakistan. We make no guarantees as to and cannot verify the accuracy of, or account for any discrepancies in, the underlying data.

## pk_candidate_scrutiny_data_2018.csv variable key

There are currently three rows in this dataset for every candidate-constituency. Each row is then a candidate-constituency-tax_year.

**candidate_code:** Candidacy filing code generated from constituency and candidate number (not unique to single individuals)

**province:** Province location (note that former FATA constituencies are included in KPK, and Islamabad constituencies in Punjab)

**assembly:** National or provincial assembly

**constituency_number:** Constituency number for directly elected seats or womens / minorities reserved list

**candidate_number:** ECP-assigned candidacy filing number

**candidate_CNICP_ECP:** ECP-reported Computerized National ID Card, unique to single individuals

**multi_candidate:** Flags individuals that are contesting multiple constituencies

**candidate_NTN:** Candidate National Tax Number as reported by Federal Board of Revenue

**candidate_NTN_issue:** Date of NTN issue as reported by Federal Board of Revenue

**candidate_RTO:** Location of NTN-issuing Regional Tax Office as reported by Federal Board of Revenue

**candidate_MNIC_NAB:** Candidate Manual National ID Card as reported by the NAB

**candidate_MNIC_SBP:** Candidate Manual National ID Card as reported by the SBP

**tax_year:** Tax-year observation (2015, 2016, or 2017)

**candidate_tax_type:** Candidate filed, did not file, or was unregistered

**candidate_tax-paid:** Tax paid by candidate as reported by FBR

**candidate_tax_paid_num:** Tax paid converted to numeric values for calculation

**candidate_tax_receipts:** "Receipts under final tax regime" as reported by FBR

**candidate_tax_receipts_num:** Tax receipts converted to numeric values for calculation

**candidate_tax_income:** Taxable income as reported by FBR

**candidate_tax_income_num:** Taxable income converted to numeric values for calculation

**candidate_tax_remarks:** Additional remarks as reported by FBR

**candidate_NAB_guilty:** Binary variable if NAB reported any conviction, plea bargain, or other pending case against candidate

**candidate_NAB_conviction:** Binary variable if NAB reported any conviction against candidate

**candidate_NAB_plea:** Binary variable if NAB reported any plea bargain on the part of candidate

**candidate_NAB_accused:** Binary variable if NAB reported candidate accused or otherwise facing pending cases

**candidate_NAB_remarks:** NAB remarks on any 

**candidate_personal_loan:** SBP remarks on candidate personal loans, if any reported

**candidate_business_loan:** SBP remarks on candidate business loans, if any reported

**parl_inc_tax_2016:** An indicator for whether this CNIC is linked to a parliamentarian in 2016 when the FBR released incumbents tax payment amounts in the 2016 Incumbent Parliamentarian report

**parl_inc_name:** Candidate name when a parliamentarian as reported by the FBR in the 2016 Incumbent Parliamentarian report

**parl_inc_chamber:** Candidate chamber when a parliamentarian as reported by the FBR in the 2016 Incumbent Parliamentarian report

**parl_inc_province:** Candidate province when a parliamentarian as reported by the FBR in the 2016 Incumbent Parliamentarian report, incomplete for MNAs

**parl_inc_type:** Candidate seat type when a parliamentarian as reported by the FBR in the 2016 Incumbent Parliamentarian report, incomplete for MNAs

**parl_tax_paid_2016:** Amount paid by candidate in 2016 when they were a parliamentarian, as reported by the FBR in the 2016 Incumbent Parliamentarian report

**parl_aop_tax_paid_2016:** Amount paid by AOPs the candidate was a part of in 2016 when they were a parliamentarian, as reported by the FBR in the 2016 Incumbent Parliamentarian report

**candidate_name_FBR:** Candidate name as reported by FBR in Urdu unicode

**candidate_name_NAB:** Candidate name as reported by NAB in Urdu unicode

**candidate_name_SBP:** Candidate name as reported by SBP in Roman Urdu (note: SBP forms did not report name information consistently)

**urdu_name_match:** Check on whether FBR and NAB Urdu names match (several cases indicate mismatch, apparently due to typos or nonstandardized name spellings)

**MNIC_match:** Check on whether NAB and SBP MNIC identifications match

**target:** Directory path for folder with candidate data

### wide version of the data

We also release a wide version of the data as `pk_candidate_scrutiny_data_2018_wide.csv`. All that changes is the data is no longer candidate-constituency-tax_year, but is rather just candidate-constituency level. All of the `candidate_tax_*` variables scraped from the FBR PDFs become `candidate_tax_*_YYYY` variables.
