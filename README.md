# Pakistan 2018 General Elections Candidate Scrutiny Forms

This repository hosts open source R code used to scan, consolidate, tidy, and clean candidate scrutiny forms released by the Election Commission of Pakistan for prospective candidates for Pakistan's 2018 national and provincial assembly elections. The final output after cleaning is also included as [pk_candidate_scrutiny_data_2018.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/pk_candidate_scrutiny_data_2018.csv).

For questions, suggestions, or to contribute, please leave an issue here or contact the contributors, Luke Sonnet and Colin Cookman.

## Data
The raw files used as a data source are approximately 10.3 GB in size and too large to host in this Github repository; the [data subfolder](https://github.com/colincookman/pakistan_2018_candidates/tree/master/data) provides information on alternative mirror hosts that can be used to obtain these original raw pdfs.

## Code replication

Once the data are downloaded to `data/2018 Candidate Scrutiny Forms` following the instructions in the [data subfolder](https://github.com/colincookman/pakistan_2018_candidates/tree/master/data), then you can:

* Run `00_scrape_scrutiny_forms.R` to read from PDFs and create intermediary data
* Run `01_clean_scrutiny_data.R` to clean up and standardize the scraped data

## Scope and possible gaps in data
Pakistani election law does not impose residency requirements for candidacy filings and allows individual candidates to contest multiple seats simultaneously, within and across assemblies and provinces. As of the initial data release the ECP provided information for 19397 candidacy filings and 15904 unique candidates (as identified by accompanying Computerized National ID Card records). 

### Candidacy filing counts
| province    | assembly            | direct_seats | womens_seats | minority_seats | 
|-------------|---------------------|--------------|--------------|----------------| 
| Balochistan | National Assembly   | 373          | 40           | NA             | 
| Balochistan | Provincial Assembly | 1302         | 13           | 24             | 
| KPK         | National Assembly   | 1080         | 90           | NA             | 
| KPK         | Provincial Assembly | 1959         | 177          | 59             | 
| National    | National Assembly   | NA           | NA           | 215            | 
| Punjab      | National Assembly   | 2426         | 172          | NA             | 
| Punjab      | Provincial Assembly | 6234         | 668          | 141            | 
| Sindh       | National Assembly   | 1181         | 178          | NA             | 
| Sindh       | Provincial Assembly | 2915         | 94           | 56             | 

(A constituency-level summary of candidacy filing reports can be found [here]([pk_candidate_scrutiny_data_2018.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/constituency_filing_count.csv) in the data folder.)

The ECP had [previously reported](https://www.ecp.gov.pk/PrintDocument.aspx?PressId=55295&type=Image) on June 18 2018 ([Wayback Ref](https://web.archive.org/web/20180627164802/https://www.ecp.gov.pk/PrintDocument.aspx?PressId=55295&type=Image)) that 21482 nomination papers had been filed. While we cannot account for the discrepancy or identify missing candidates at this stage, based on the ECP's earlier aggregate figures the following number of candidacy filings may be missing from the current dataset (or not accounted for in the earlier ECP statement in cases where more records were available):

### ECP reported filings (Difference with available data)
| province    | assembly            | direct_seats | womens_seats | minority_seats | 
|-------------|---------------------|--------------|--------------|----------------| 
| Balochistan | National Assembly   | 435 (-62)    | 36 (+4)      | NA             | 
| Balochistan | Provincial Assembly | 1400 (-98)   | 116 (-103)   | 56 (-32)       | 
| KPK         | National Assembly   | 992 (+88)    | 88 (+2)      | NA             | 
| KPK         | Provincial Assembly | 1920 (+39)   | 262 (-85)    | 73 (-14)       | 
| National    | National Assembly   | NA           | NA           | 154 (+61)      | 
| Punjab      | National Assembly   | 2700 (-274)  | 236 (-64)    | NA             | 
| Punjab      | Provincial Assembly | 6747 (-513)  | 664 (+4)     | 232 (-91)      | 
| Sindh       | National Assembly   | 1346 (-165)  | 76 (+102)    | NA             | 
| Sindh       | Provincial Assembly | 3626 (-711)  | 213 (-119)   | 110 (-54)      | 


A final list of candidates following the process of scrutiny, disqualification, appeal, and withdrawal is currently [scheduled for release](https://www.ecp.gov.pk/PrintDocument.aspx?PressId=55262&type=Image) by the ECP on June 30 2018 which may allow for some retroactive analysis to identify missing filing data.


## Output variable key
TODO


## Caveats
This dataset is being presented to encourage broader open data sharing among the community of analysts on Pakistan. We make no guarantees as to and cannot verify the accuracy of, or account for any discrepancies in, the underlying data.