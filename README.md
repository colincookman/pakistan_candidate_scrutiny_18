# Pakistan 2018 General Elections Candidate Scrutiny Forms

This repository hosts open source R code used to scan, consolidate, tidy, and clean candidate scrutiny forms released by the Election Commission of Pakistan for prospective candidates for Pakistan's 2018 national and provincial assembly elections. The final output after cleaning is also included as [pk_candidate_scrutiny_data_2018.csv](https://github.com/colincookman/pakistan_2018_candidates/blob/master/pk_candidate_scrutiny_data_2018.csv).

Please note that as of current writing this code is still a work in progress and data outputs hosted here are still incomplete. For questions, suggestions, or to contribute, please leave an issue here or contact the contributors, Luke Sonnet and Colin Cookman.

## Data
The raw files used as a data source are approximately 10.3 GB in size and too large to host in this Github repository; the [data subfolder](https://github.com/colincookman/pakistan_2018_candidates/tree/master/data) provides information on alternative mirror hosts that can be used to obtain these original raw pdfs.

## Variable key
TODO

## Analysis

Once the data are downloaded to `data/2018 Candidate Scrutiny Forms` following the instructions in the [data subfolder](https://github.com/colincookman/pakistan_2018_candidates/tree/master/data), then you can:

* Run `00_scrape_scrutiny_forms.R` to read from PDFs and create intermediary data
* Run `01_clean_scrutiny_data.R` to clean up and standardize the scraped data