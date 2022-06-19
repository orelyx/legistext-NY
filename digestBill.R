################################################################################
# digestBill.R:  From a bill introduced in the New York legislature, in pdf format
#                as posted at https://www.nysenate.gov/legislation/bills/, 
#                extract the section numbering and text of the bill, processing 
#                only the text with line numbers. 
#
#     Copyright (c) 2022 Orebed Analytics LLC under MIT License; see LICENSE.txt. 
# 
#     Data files produced by this software are released under Creative Commons 
#     license (see https://creativecommons.org/licenses/by/4.0/legalcode) to the 
#     extent that they are not already in the public domain. 

library(tidyverse)
library(stringi)
library(pdftools)
library(tidytext)
library(tokenizers)
library(cleanNLP)
library(quanteda)
library(glue)

outlineTagRegex <- 
  str_c("(?:^[ ]*Section[ ]+[0-9-]{1,9}([a-z]{0,1})\\1{0,4}[.]{0,1}[ ]{0,1})|",
        "(?:^[ ]*TITLE[ ]+(?:[0-9]{1,9}|[IXV]{1,12})(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*ARTICLE[ ]+(?:[0-9]{1,9}|[IXV]{1,12})(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*SUBTITLE[ ]+(?:[0-9]{1,9}|[IXV]{1,12})(?:-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)|",
        "(?:^[ ]*§[ ]{0,1}[0-9-]{1,9}([a-z]{0,1})\\2{0,4}[\\.]{0,1}[ ]*)|",
        "(?:^[ ]*[0-9]{1,5}[\\.]{0,1}[ \t]{0,8}[(]{1}[a-zA-Z]{1,2}[)]{1}[ ]*)|",
        "(?:^[ ]*[0-9]{1,5}[\\.]{1}[^0-9]{1}[ ]*)|",
        "(?:^[ ]*[0-9]{1,5}-[0-9]{0,6}([a-z]{0,1})\\3{0,4}[\\.]{1}[ ]*)|",
        "(?:^[ ]*[0-9]{1,5}[-]{0,1}([a-z]{0,1})\\4{0,4}[\\.]{1}[ ]*)|",
        "(?:^[ ]*[a-z]{1}[\\.]{1}[ ]*)|",                         # 2020-04-20 only lower-case
        "(?:^[ ]*[(]{1}[a-zA-Z]{1,2}[)]{1}[^,]{1}[ ]*)|",
        "(?:^[ ]*([ivxIVX]+[\\.]|[(][ivxIVX]+[)])[^,][ ]*)")

digestBill <- function(path, filename, name) {
  bill <- file.path(path, filename)
  pages <- pdf_text(bill)
  lines <- unlist(sapply(1:length(pages), 
                         function(x) str_extract_all(pages[x], "[^\n]+")))
  
  # This won't necessarily be useful
  titleInfo <- tibble(Senate_Bill = str_extract(lines[3], "S. [0-9]{1,4}"),
                      Assembly_Bill = str_extract(lines[3], "A. [0-9]{1,4}"),
                      Session = str_extract(lines[4], "[0-9]{4}[ ]*-[ ]*[0-9]{4} [A-Za-z ]+"),
                      Date = str_extract(lines[6], "[A_Za-z]+ [0-9]+, [0-9]+"))
  
  text <- sapply(lines, function(x) str_extract(x, "^[ ]{0,10}([0-9]+)[ ]+(.*)$"))
  names(text) <- NULL
  textMatrix <- str_match(text[which(!is.na(text))], "^[ ]{0,10}([0-9]+)[ ]+(.*)$")
  textTibble <- tibble(Line_Numbers = textMatrix[,2], Text = textMatrix[,3]) %>%
    mutate(Page = cumsum(Line_Numbers == "1")) %>%
    select(Page, everything()) %>%
    mutate(Outline_Tag = str_extract(Text, outlineTagRegex)) %>%
    mutate_at("Text", function(x) str_remove(x, outlineTagRegex)) %>%
    mutate_at("Text", function(x) str_replace(x, "$", " ")) %>%
    mutate_at("Text", function(x) str_replace(x, "- $", "")) %>%
    mutate_at("Text", function(x) str_replace(x, "^[ ]*", "")) %>%
    mutate_at("Text", function(x) str_replace(x, "[ ]{1}[ ]*", " ")) %>%
    mutate_at("Outline_Tag", function(x) (str_replace(x, "[ ]*$", ""))) %>%
    mutate(Item = cumsum(!is.na(Outline_Tag))) %>%
    mutate(Page_Line = str_c(as.character(Page), ".", as.character(Line_Numbers))) %>%
    select(Item, Page_Line, Outline_Tag, everything())
  textTibble
}
# thisBill.df <- digestBill(".", "a9856.pdf", "A9856")
