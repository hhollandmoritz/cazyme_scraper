#!/bin/bash

# Install the conda environment:
conda env create -f cazyme_scraper.yml --force

source $(conda info --base)/etc/profile.d/conda.sh
# Since the name changes with each version, make sure to pull the version
# name out of the yml file
ENV_NAME=$(awk '/name: cazyme_scraper_v/{print $NF}' cazyme_scraper.yml)
conda activate $ENV_NAME

# Install packages not in conda with R (we use the cloud-0 mirror so that the download
# will be fast in any part of the world)
Rscript -e 'install.packages("unheadr", repos="https://cloud.r-project.org")'

# Deactivate the conda environment
conda deactivate
