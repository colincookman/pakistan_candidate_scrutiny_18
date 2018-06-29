## Description of data

As part of the filing and scrutiny process for registered candidates for the 2018 Pakistani national and provincial assembly, in early June 2018 the Election Commission of Pakistan collected reports from Pakistan's Federal Board of Revenue, National Accountability Bureau, and State Bank of Pakistan, each providing statements on prospective candidates' tax history, corruption cases, and loan default records.

## Raw candidate scrutiny files

The ECP released these files hosted through a publicly viewable Google Drive folder on its website on June 21, 2018. As of writing on June 26, 2018, these original files are accessible on the ECP webpage at this link: 
https://www.ecp.gov.pk/frmGenericPage.aspx?PageID=3152 ([Wayback Machine Reference](https://web.archive.org/web/20180627011544/https://www.ecp.gov.pk/frmGenericPage.aspx?PageID=3152))

As a backup mirror, the files were downloaded directly from the ECP on June 24 2016. The unaltered zip files are stored and are available for download through this link: 
https://drive.google.com/drive/folders/1Lin-DGGo7n4Eg5H7fXEloCIqRfBBGJwA?usp=sharing

Due to the size of the files, the Punjab Provincial Assembly folders was automatically split by the Google download function into two separate zip files, which should be merged (option-drag on a Mac) to ensure a single consolidated set of folders.

An unzipped version of the files — merged as above but otherwise unchanged — is also available for download or viewing through this link:
https://drive.google.com/drive/folders/1uTCZ5MkMdjT6YWen3GADLjpe7J8JmrZF?usp=sharing

To replicate our analysis, the files should be placed in this folder, the `data` folder, in a folder named `"2018 Candidate Scrutiny Forms"`.

## Data cleaning and processing

For R code used to clean, consolidate, and tidy the pdf reports in this dataset and transform it into a consolidated .csv file output, see [00_scrape_scrutiny_forms.R](https://github.com/colincookman/pakistan_2018_candidates/blob/master/00_scrape_scrutiny_forms.R) in the master directory. The output from that, [scraped_scrutiny_forms.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/data/scraped_scrutiny_forms.csv), is hosted in this folder and used in the code [01_clean_scrutiny_data.R](https://github.com/colincookman/pakistan_2018_candidates/blob/master/01_clean_scrutiny_data.R) to produce our final outputs, [pk_candidate_scrutiny_data_2018.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/pk_candidate_scrutiny_data_2018.csv) and  [pk_candidate_scrutiny_data_2018_wide.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/pk_candidate_scrutiny_data_2018_wide.csv). For additional comments see the [main repository README file](https://github.com/colincookman/pakistan_2018_candidates/blob/master/README.md).

[constituency_filing_count.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/data/constituency_filing_count.csv) is a summary count of the number of reported candidate filings per constituency based on this data. 
[filing_sequence_gaps.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/data/filing_sequence_gaps.csv) identifies candidate numbers missing in the available candidate number sequence as released by the ECP.

## Additional data

We also draw on data posted by the FBR about the [tax records](https://github.com/colincookman/pakistan_2018_candidates/blob/master/data/2017727974535733TaxDirectory-Parliamentarians2016.pdf) of parliamentarians in 2016 [also available on the FBR's page here](http://download1.fbr.gov.pk/Docs/2017727974535733TaxDirectory-Parliamentarians2016.pdf). 

The code [00_scrape_parl_tax_2016.R](https://github.com/colincookman/pakistan_2018_candidates/blob/master/data/00_scrape_parl_tax_2016.R) scrapes this to produce [cleaned_parliamentary_2016.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/data/cleaned_parliamentary_2016.csv), which is used in the [01_clean_scrutiny_data.R](https://github.com/colincookman/pakistan_2018_candidates/blob/master/01_clean_scrutiny_data.R) process to add additional incumbency variables.

