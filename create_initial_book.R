# following https://github.com/ropensci-review-tools/babelquarto

# library(remotes)
# install_github("https://github.com/ropensci-review-tools/babelquarto")
library(babelquarto)

parent_dir <- "/Users/DMontecino/OneDrive - Wildlife Conservation Society/DICTIONARIES"
project_dir <- "/Users/DMontecino/OneDrive - Wildlife Conservation Society/DICTIONARIES/SMART_for_Heath_Rangers_Dictionary/"
# babelquarto::quarto_multilingual_book(parent_dir = parent_dir, project_dir = project_dir)

#fs::dir_tree(file.path(project_dir))

# babelquarto::render_book(file.path(project_dir))

#usethis::use_github()

#Create a directory docs in the parent directory of blop 
#and then copy all the contents of _book in the docs directory.

#dir.create(path = "/Users/DMontecino/OneDrive - Wildlife Conservation Society/DICTIONARIES/SMART_for_Heath_Rangers_Dictionary/docs")


# functions from https://github.com/shafayetShafee/babelquarto1?tab=readme-ov-file#user-content-fn-1-93b118cd6837299bf4cbf2089e2340f7
library(rvest)
library(fs)

add_base_url <- function(file_path, base_link) {
  html_content <- rvest::read_html(file_path)
  lang_links <- html_content %>% 
    rvest::html_elements('a[id*="language-link"]') %>% 
    rvest::html_attr("href")
  lang_link_with_baseurl <- paste0(base_link, lang_links)
  raw_html <- paste(readLines(file_path), collapse="\n")
  for (i in seq_along(lang_links)) {
    raw_html <- sub(lang_links[i], lang_link_with_baseurl[i], raw_html)
  }
  return(raw_html)
}

create_abs_lang_link <- function(path, base_link) {
  html_files_path <- fs::dir_ls(path, recurse = TRUE, glob="*.html")
  for (file_path in html_files_path) {
    edited_raw_html <- add_base_url(file_path, base_link)
    writeLines(edited_raw_html, file_path)
  }
}


babelquarto::render_book(file.path(project_dir))

#move the files in _book to docs

#run the function

create_abs_lang_link(path = "docs", base_link = "")


# then just create the github page following these instructions 
# https://www.youtube.com/watch?v=uimdXPZc40I&ab_channel=JoshuaFrench

# Then, just create the website again in github


# Export the flextable objects as xlsx. 

#1. Create each "out" object using the chapters' qmd files. 

# library(devtools)

# Then install and load the YesSiR package
# devtools::install_github("Sebastien-Le/YesSiR")
# library(YesSiR)




# The chapters in english
names_chapter_english<-c("site_description", "wildlife", "livestock_dom", "animals_samples")



# Execute the R code within each .qmd file

for(i in names_chapter_english){
rmd <- parsermd::parse_rmd(paste0(project_dir, i, ".qmd"))
rmd<-parsermd::rmd_select(rmd, i) %>% parsermd::as_document()
chunk <- rmd[-grep("```", rmd)]
chunk
#> [1] "library(tidyr)"   "library(stringr)" ""                

eval(parse(text = chunk))

YesSiR::exportxlsx(table=out, 
                   # filename ="site_description", 
                   path = paste0(project_dir, "Partial_dictionaries_lao_khmer/Reference_english/", i, ".xlsx"))

}



# The chapters in lao
names_chapter_lao<-c("site_description", "wildlife", "livestock_dom", "animals_samples")



# Execute the R code within each .qmd file

for(i in names_chapter_lao){
  rmd <- parsermd::parse_rmd(paste0(project_dir, i, ".lo.qmd"))
  rmd<-parsermd::rmd_select(rmd, i) %>% parsermd::as_document()
  chunk <- rmd[-grep("```", rmd)]
  chunk
  #> [1] "library(tidyr)"   "library(stringr)" ""                
  
  eval(parse(text = chunk))
  
  YesSiR::exportxlsx(table=out, 
                     # filename ="site_description", 
                     path = paste0(project_dir, "Partial_dictionaries_lao_khmer/Lao/", i, "_lao.xlsx"))
  
}



# The chapters in kh
names_chapter_kh<-c("site_description", "wildlife", "livestock_dom", "animals_samples")



# Execute the R code within each .qmd file

for(i in names_chapter_kh){
  rmd <- parsermd::parse_rmd(paste0(project_dir, i, ".km.qmd"))
  rmd<-parsermd::rmd_select(rmd, i) %>% parsermd::as_document()
  chunk <- rmd[-grep("```", rmd)]
  chunk
  #> [1] "library(tidyr)"   "library(stringr)" ""                
  
  eval(parse(text = chunk))
  
  YesSiR::exportxlsx(table=out, 
                     # filename ="site_description", 
                     path = paste0(project_dir, "Partial_dictionaries_lao_khmer/Cambodia/", i, "_khmer.xlsx"))
  
}
