here::i_am("cazyme_scraper.R")
library(here)
library(rvest)
library(janitor)
library(unheadr)
library(tidyverse)


# Cazyme scraper.R to scrape characterized chazymes from the website
get_char_cazy <- function(webpage = "http://www.cazy.org/GH18_characterized.html",
                          FamilyName = "GH18") {
  
  webpg <- read_html(webpage)
  
  # Count the number of pages
  havepages <- webpg %>%
    html_elements("span.pages") %>%
    html_text() %>%
    str_extract("([0-9]+)$") %>%
    as.numeric()
  
  if(length(havepages) == 0) { # No pages
    # Extract table from webpage
    tbl <- webpg %>%
      html_elements("table") %>%
      .[[2]] %>%
      html_table()
    
    # Clean the data; remove subheadings, correct rownames, break genbank ids into multiple lines
    tbl_clean <- tbl %>% 
      select(-X8) %>% # random extra column
      janitor::row_to_names(row_number = 3, remove_row = FALSE, remove_rows_above = FALSE) %>% 
      filter(`Protein Name` != "Protein Name") %>%  # remove extra header columns
      filter(!grepl("suivante", `Protein Name`)) %>% # reomve pagination if present
      filter(!grepl("Top", `Protein Name`)) %>% # Remove "top" row if present
      unheadr::untangle2(regex = "(Archaea|Bacteria|Eukaryota|Viruses)", orig = `Protein Name`, new = "Taxonomy") %>% 
      mutate(GenBank_mod = gsub("(\\.[0-9])", "\\1;", GenBank)) %>% # might break if more than 9 versions of a gene are present
      mutate(Nrow_before_sepGenbank = n()) %>%
      separate_rows(GenBank_mod, sep = ";") %>%
      filter(GenBank_mod != "") %>%
      mutate(GenBank = GenBank_mod) %>%
      select(-GenBank_mod)
    
    
    tbl_list <- list(tbl_clean)
    
    writeLines(paste0("Extracted table from page 1 of 1"))
  } else {
    # Extract table from webpage
    tbl <- webpg %>%
      html_elements("table") %>%
      .[[2]] %>%
      html_table()
    
    # Clean the data; remove subheadings, correct rownames, break genbank ids into multiple lines
    tbl_clean <- tbl  %>% 
      select(-X8) %>% # random extra column
      janitor::row_to_names(row_number = 3, remove_row = FALSE, remove_rows_above = FALSE) %>%
      filter(`Protein Name` != "Protein Name") %>%  # remove extra header columns
      filter(!grepl("suivante", `Protein Name`)) %>% # reomve pagination if present
      filter(!grepl("Top", `Protein Name`)) %>% # Remove "top" row if present
      unheadr::untangle2(regex = "(Archaea|Bacteria|Eukaryota|Viruses)", orig = `Protein Name`, new = "Taxonomy") %>%
      mutate(GenBank_mod = gsub("(\\.[0-9])", "\\1;", GenBank)) %>% # might break if more than 9 versions of a gene are present
      mutate(Nrow_before_sepGenbank = n()) %>%
      separate_rows(GenBank_mod, sep = ";") %>%
      filter(GenBank_mod != "") %>%
      mutate(GenBank = GenBank_mod) %>%
      select(-GenBank_mod)
    
    tbl_list <- list(tbl_clean)
    writeLines(paste0("Extracted table from page 1 of ", havepages))
    
    # Start on page 2 and iterate to final page
    for(i in 1:(havepages-1)) {
      nresults = 100 # number of results per page; static - page 2 starts at 100
      # rewrite webpage to the correct page number
      tmp_webpage <- paste0(webpage, "?debut_FUNC=",nresults*i, "#pagination_FUNC")
      tmp_webpg <- read_html(tmp_webpage)
      
      # Extract table from webpage
      tmp_tbl <- tmp_webpg %>% 
        html_elements("table#pos_onglet.listing") %>% 
        .[[1]] %>% 
        html_table()
      
      
      # Clean the data; remove subheadings, correct rownames, break genbank ids into multiple lines
      tmp_tbl_clean <- tmp_tbl %>% 
        select(-X8) %>% # random extra column
        janitor::row_to_names(row_number = 3, remove_row = FALSE, remove_rows_above = FALSE) %>%
        filter(`Protein Name` != "Protein Name") %>%  # remove extra header columns
        filter(!grepl("suivante", `Protein Name`)) %>% # reomve pagination if present
        filter(!grepl("Top", `Protein Name`)) %>% # Remove "top" row if present
        unheadr::untangle2(regex = "(Archaea|Bacteria|Eukaryota|Viruses)", orig = `Protein Name`, new = "Taxonomy") %>%
        mutate(GenBank_mod = gsub("(\\.[0-9])", "\\1;", GenBank)) %>% # might break if more than 9 versions of a gene are present
        mutate(Nrow_before_sepGenbank = n()) %>%
        separate_rows(GenBank_mod, sep = ";") %>%
        filter(GenBank_mod != "") %>%
        mutate(GenBank = GenBank_mod) %>%
        select(-GenBank_mod)
      
      tbl_list <- rlist::list.append(tbl_list, tmp_tbl_clean)
      writeLines(paste0("Extracted table from page ", i + 1, " of ", havepages))
    }
  }
  
  tbl_return <- purrr::reduce(tbl_list, bind_rows)
  
  if(!is.null(FamilyName)) {
    tbl_return <- tbl_return %>% 
      mutate(FamilyName = FamilyName)
  } else {
    tbl_return <- tbl_return %>% 
      mutate(FamilyName = NA)
  }
  
  return(tbl_return)
  
}


#### ====================================================================== ####

# Example for Chitin
#### ====================================================================== ####
Families <- c("GH18","GH19","GH23","GH73","GH20","GH84","CE4") # chitin families of interest 

outdir <- "" # output directory, if blank, will create output in location of script

scrape_char_chitin_cazy <- function(FamilyNames = Families,
                                    write_output = TRUE,
                                    output_dir = outdir) {
  
  # FamilyNames = list of gene families to pull data from
  # write_output = true or false; should output be written to new table?
  # output_dir = what directory should output be written to? If blank, output will be written in location of script
  
  # Scrape the lists from the website
  cazy_char_list <- lapply(FamilyNames, function(x) {
    writeLines(paste0("Processing ", x))
    web <- paste0("http://www.cazy.org/", x, "_characterized.html")
    get_char_cazy(webpage = web, FamilyName = x)
  })
  
  
  # Reduce the list to one dataframe
  chitin_char <- purrr::reduce(cazy_char_list, bind_rows) %>%
    filter(Taxonomy %in% c("Bacteria", "Archaea", "Viruses")) %>%
    filter(grepl("[Cc]hi", `Protein Name`)) # searching for 'chi' pattern in names
  
  if(write_output) {
    # Write a table
    write_tsv(chitin_char, file = here(outdir,"characterized_chitinases.txt"))
  }
  
  return(chitin_char)
}

scrape_char_chitin_cazy()
