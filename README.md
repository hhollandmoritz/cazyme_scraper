# cazyme_scraper

A custom R-script to scrape the list of experimentally characterized cazymes from the cazymes website.

## Create conda environment

``` bash
git clone git@github.com:hhollandmoritz/cazyme_scraper.git
cd cazyme_scraper
./install_dependencies.sh
conda activate cazyme_scraper_vX
```

If you require packages not installed in the above environment, add them to temporal_paper.yml and update the version number.

## Using this script

1.  Modify the example to match your needs. First modify the list of families you're searching for [line 126](https://github.com/hhollandmoritz/cazyme_scraper/blob/f49741f419afd7fdc23ae93cca21ff7ef96d9ee2/cazyme_scraper.R#L126). Then modify the search pattern you're looking for in the protein name [line 149](https://github.com/hhollandmoritz/cazyme_scraper/blob/f49741f419afd7fdc23ae93cca21ff7ef96d9ee2/cazyme_scraper.R#L149).
2.  Optional, modify the output directory and the write_output option to determine if the script will create a table or just an R-object.
3.  Activate the conda environment as above and run the script.

Either in R:

``` r
source("cazyme_scraper.R")
```

Or from the command line:

``` bash
Rscript cazyme_scraper.R
```
