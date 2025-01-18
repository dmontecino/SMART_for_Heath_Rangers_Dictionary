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


#> to add a new language copy the index.qmd, dictionary.qmd, manual.qmd and save them as a 
#> new version as index.'2letterlanguagecode".qmd. Translate each of these files.
#> Then add the components in the quarto.yaml file
#> Then run the following line:

babelquarto::render_book(file.path(project_dir))

#> get the index.2languagecode.html created (also the dictionary and manual files) and open them in the
#> editor here. for khmer, laotian, and pitentially other languages, edit the "Table of contents" 
#> in english with the corresponding language. Save and close


#> Run these lines adding a new line in each chunk with the corresponding folder 
#> and source file


#save the manual as a pdf. 

renderthis::to_pdf("_book/Manual.html")
renderthis::to_pdf("_book/es/Manual.es.html")
renderthis::to_pdf("_book/fr/Manual.fr.html")
renderthis::to_pdf("_book/km/Manual.km.html")
# renderthis::to_pdf("_book/lo/Manual.lo.html")


#save the data dictionary as a pdf. 
renderthis::to_pdf("_book/Dictionary.html")
renderthis::to_pdf("_book/es/Dictionary.es.html")
renderthis::to_pdf("_book/fr/Dictionary.fr.html")
renderthis::to_pdf("_book/km/Dictionary.km.html")
# renderthis::to_pdf("_book/lo/Dictionary.lo.html")





# Edit the file path to download the correct manual pdf 
library(stringr)

languages<-c('en', 'es', 'fr', 'km')
nlanguages<-length(languages)

# i=2

for(i in languages){
if(i =="en"){
  # Define the input and output file paths
  input_file <- "_book/Manual.html"
  output_file <- "_book/Manual.html"}else{
    
    input_file <- paste0("_book/", i, "/Manual.",  i, ".html")
    output_file <- paste0("_book/", i, "/Manual.",  i, ".html")}


# Read the original HTML content from the file
original_html <- readLines(input_file, warn = FALSE)

# Combine the read lines into a single string
original_html_string <- paste(original_html, collapse = "\n")

if(i=="en"){
new_href <- paste0("/Manual.","pdf")}else{
new_href <- paste0("/Manual.", i, ".pdf")}

# # Define the input HTML content
# original_html <- '<a href="./Manual-y-Diccionario-SMART-para-Salud---Guardaparques.pdf" rel="" title="Download PDF" class="quarto-navigation-tool px-1" aria-label="Download PDF"><i class="bi bi-file-pdf"></i></a>'

# Define the regular expression pattern to extract the desired part
pattern <- 'href="./([^"]+).pdf"'

# Extract the desired substring using str_match
matches <- paste0(str_match(original_html_string, pattern)[1,2], ".pdf")

# Replace the href value using stringr's str_replace function
modified_html_string <- gsub(pattern = matches,
                             replacement = new_href,
                             x = original_html_string)

writeLines(modified_html_string, output_file)
}




# Edit the file path to download the correct dictionary pdf 

for(i in languages){
  if(i =="en"){
    # Define the input and output file paths
    input_file <- "_book/Dictionary.html"
    output_file <- "_book/Dictionary.html"}else{
      
      input_file <- paste0("_book/", i, "/Dictionary.",  i, ".html")
      output_file <- paste0("_book/", i, "/Dictionary.",  i, ".html")}
  
  
  # Read the original HTML content from the file
  original_html <- readLines(input_file, warn = FALSE)
  
  # Combine the read lines into a single string
  original_html_string <- paste(original_html, collapse = "\n")
  
  if(i=="en"){
    new_href <- paste0("/Dictionary.","pdf")}else{
      new_href <- paste0("/Dictionary.", i, ".pdf")}
  
  # # Define the input HTML content
  # original_html <- '<a href="./Manual-y-Diccionario-SMART-para-Salud---Guardaparques.pdf" rel="" title="Download PDF" class="quarto-navigation-tool px-1" aria-label="Download PDF"><i class="bi bi-file-pdf"></i></a>'
  
  # Define the regular expression pattern to extract the desired part
  pattern <- 'href="./([^"]+).pdf"'
  
  # Extract the desired substring using str_match
  matches <- paste0(str_match(original_html_string, pattern)[1,2], ".pdf")
  
  # Replace the href value using stringr's str_replace function
  modified_html_string <- gsub(pattern = matches,
                               replacement = new_href,
                               x = original_html_string)
  
  writeLines(modified_html_string, output_file)
}




#move the files in _book to docs

source_dir <- "_book/"
dest_dir <- "docs/"


# remove files in the destination folder
unlink(file.path(dest_dir, "*"), recursive = TRUE)# list.files(dest_dir, full.names = TRUE, recursive = T)

# Get a list of all files and folders in the source directory
items_to_copy <- list.files(source_dir, full.names = TRUE)

# Copy each item (file or folder) to the destination directory
sapply(items_to_copy, function(item) {
  file.copy(item, dest_dir, recursive = TRUE, overwrite = TRUE)
})





#run the function

# create_abs_lang_link(path = "docs", base_link = "")

# commit in git and push the changes to github


# then just create the github page following these instructions 
# https://www.youtube.com/watch?v=uimdXPZc40I&ab_channel=JoshuaFrench

# Then, just create the website again in github





