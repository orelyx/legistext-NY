################################################################################
# billOutline.R: Parse the data frame representation of a bill prepared by 
#                digestBill.R to extract a full outline tag for each paragraph 
#                by which the paragraph can be referenced. The algorithm used 
#                attempts through cleverness to get around the lack of a formal 
#                specification of bill formats; it succeeds on S4264A (2021) and 
#                several other bills but gets fooled in a couple of places so there's 
#                room for improvement as well as testing over a wider variety 
#                of legislation. 
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

outlineTags <- 
  c("(^[ \t]*TITLE[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",      # 1
    "(^[ \t]*ARTICLE[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",    # 1
    "(^[ \t]*SUBTITLE[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",   # 1
    "(^[ \t]*CHAPTER[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",    # 1
    "(^[ \t]*SUBCHAPTER[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)", # 1
    "(^[ \t]*PART[ ]+([0-9]{1,9}|[IXV]{1,12})(-[A-Za-z]{1}){0,1}[.]{0,1}[ ]*)",       # 1
    "(^[ \t]*Section[ ]+[0-9-]{1,9}[.]{0,1}[ ]{0,1})",                              # 2
    "(^[ \t]*§[ ]{0,1}[0-9-]{1,9}[a-z]{0,1}[.]{0,1}[ ]*)",                          # 2
    "(^[ \t]*[0-9]{1,3}[.]{0,1}[ \t]{0,8}[(]{1}[a-zA-Z]{1,2}[)]{1}[ ]*)",           # 3
    "(^[ \t]*[0-9]{1,3}[.]{1}[^0-9]{1}[ ]*)",                                       # 3
    "(^[ \t]*[0-9]{1,3}-[0-9]{0,6}[.]{1}[ ]*)",                                     # 3
    "(^[ \t]*[0-9]{1,3}[-]{0,1}[a-z]{0,1}[.]{1}[ ]*)",                              # 3
    "(^[ \t]*[a-z]{1}[.]{1}[ ]*)",                                                  # 4
    "(^[ \t]*[(]{1}[a-z]{1}[)]{1}([ivxIVX]+[.]|[(][ivxIVX]+[)])[ ]*)",              # 4
    "(^[ \t]*[(]{1}[a-z]{1}[)]{1}[ ]*)",                                            # 4
    "(^[ \t]*([ivx]+[.]|[(][ivx]+[)])[ ]*)",                                        # 5
    "(^[ \t]*[(]{1}[A-Z]{1,2}[)]{1}[ ]*)",                                          # 6
    "(^[ \t]*[IVX]+[.][ ]*)"                                                        # 7
  )

matchIndices <- c("TITLE", "ARTICLE", "SUBTITLE", "CHAPTER", "SUBCHAPTER", "PART", 
                  "Section09", "§09", "09.az", "09", "09-09", "09-az", "az", "(az)(ivx)",
                  "(az)", "(ivx)", "(AZ)", "IVX")
# Will have to refer to these using backquotes ``
for (m in matchIndices) {
  assign(m, first(which(matchIndices == m)))
}

tagIndices <- c("BIG_PART", "SECTION", "p_09", "p_az", "p_ivx", "p_AZ", "p_IVX")
for (n in tagIndices) {
  assign(n, first(which(tagIndices == n)))
}
tagLevels <- c(BIG_PART, BIG_PART, BIG_PART, BIG_PART, BIG_PART, BIG_PART, 
               SECTION, SECTION, p_09, p_09, p_09, p_09, 
               p_az, p_az, p_az, p_ivx, p_AZ, p_IVX)

tagNames <- c("section", "subsection", "paragraph", "subparagraph")

dropWords <- read_delim("first1000.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(is.na(Keep)) %>%
  select(Word) %>%
  unlist()

consecutiveSuffixes <- function(s1, s2) {
  n1 <- last(str_extract_all(s1, "[0-9]+", simplify = TRUE))
  n2 <- last(str_extract_all(s2, "[0-9]+", simplify = TRUE))
  if (!is.na(n1) & !is.na(n2)) {
    i1 <- as.integer(n1)
    i2 <- as.integer(n2)
    if (!is.na(i1) & !is.na(i2)) {
      if (str_remove(s1, n1) == str_remove(s2, n2)) {
        return(i2 == (i1 + 1))
      }
    }
  }
  FALSE
}

processOutline <- function(bill) {
  processedBill <- bill %>%
    mutate(Tokens = tokenize_words(Text)) %>%
    mutate(Keywords = mapply(function(x, y) {
      x[which(sapply(y, function(z) {
        !((z %in% dropWords) | (str_remove(z, "[0-9.]+") == ""))}))]},
      Tokens, Tokens)) %>%
    mutate_at("Keywords", function(x) sapply(x, unique)) %>%
    mutate(Tag = "") %>%
    mutate(Number_of_Keywords = sapply(Keywords, length)) 

    processedBill <- processedBill %>%
    mutate(Matches = mapply(
      function (x, y) { 
        list(stri_extract_first_regex(x, y)[TITLE:IVX]) },
      Outline, 
      MoreArgs = list(outlineTags))) 
  
  processedBill <- processedBill %>%
    mutate(First_Match = sapply(Matches, function(x) first(which(!is.na(x))))) %>%
    mutate(Level = sapply(Matches, function(x) tagLevels[first(which(!is.na(x)))])) 
  
  letters <- lastLetters <- ""
  carriedTags <- c(rep("", p_IVX))
  sectionInterpolated <- FALSE
  
  currentSection <- ""
  
  outlineTag <- function() { 
    if ((carriedTags[BIG_PART] != "") | sectionInterpolated) {
      trimws(str_c("(§", as.character(currentSection), ") ",
                   str_c(carriedTags, collapse = "")))
    } else {
      trimws(str_c(carriedTags, collapse = ""))
    }
  }
  
  massageFormat <- function(depth, tag) {
    if (depth > SECTION) {
      tag
    } else if (depth == SECTION) {
      myTag <- str_replace(tag, "Section ", "§ ")
      if (!str_detect(myTag, "§")) {
        myTag <- str_c("§ ", myTag)
      }
      pos <- stri_locate(myTag, regex = "[.]", mode = "last")
      if(is.na(pos[1])) {
        myTag <- str_c(myTag, ".")
      } else if (pos[2] != str_length(myTag)) {
        myTag <- str_c(myTag, ".")
      }
      myTag
    } else {
      str_c(tag, " ")
    }
  }
  
  nextLetter<- function(letters) {
    len <- str_length(letters)
    c <- substr(letters, len, len)
    if (c != "") {
      code <- utf8ToInt(c)
      if ((code >= utf8ToInt("a")) & (code <= utf8ToInt("z"))) {
        next1 <- intToUtf8(utf8ToInt("a") + 
                             (((code - utf8ToInt("a")) + 1) %% 26))
        return(next1)
      } else if ((code >= utf8ToInt("A")) & (code <= utf8ToInt("Z"))) {
        next1 <- intToUtf8(utf8ToInt("A") + 
                           (((code - utf8ToInt("A")) + 1) %% 26))
        return(next1)
      }
    }
    errorCondition("Invalid string parameter to nextLetter()")
  }
  
  finalWord <- function (textLine) {
    return(str_match(textLine, "([a-z]+)[ \\t]{0,2}$")[,2])
  }
  
  # Sometimes processedBill contains a spurious initial row; get rid of it
  while (is.na(processedBill$Level[1])) {
    processedBill <- processedBill %>%
      slice(-1)
  }
  # browser() ####################################################################
  depth <- processedBill$Level[1]
  if (depth == SECTION) { # Section number
    currentSection <- str_extract(processedBill$Outline[1], "[0-9-.]+")
    carriedTags[SECTION] <- str_c("§ ", currentSection)
  } else {
    message("Expected first section number not found.")
    currentSection <- NA
    carriedTags[depth] <- processedBill$Outline[1]
  }
  processedBill$Tag[1] <- outlineTag()
  
  for (i in 2:length(processedBill$Outline)) {
    # if (i == 286) { browser() }  #############################################
    newDepth <- processedBill$Level[i]
    # print(as.character(i)) #
    if (is.na(depth) | is.na(newDepth)) {
      # browser()
      # Sometimes an interpolated ARTICLE, TITLE, or SUBTITLE has an initial 
      # 'table-of-contents' section containing section numbers not preceded with 
      # "Section" or "§". In these cases code around 83 will not recognize the 
      # section number as a tag or assign a level. This is harmless; allowing it 
      # to be recognized creates ambiguity with paragraph numbers within 
      # sections and I'm not sure I can think of a safe way to deal with it. 
      # I formerly issued a warning with both depth and newDepth were NA but I 
      # now don't think I want to. 
      if (!is.na(depth)) {    # band-aid for "1900." at the start of a line
        newDepth <- depth
      } else {
        depth <- newDepth
      }
    }
    if (newDepth < depth) {
      if (newDepth == BIG_PART) { # Start of an interpolated law amendment
        
      } else if (newDepth == SECTION) { 
        thisSection <- str_extract(processedBill$Outline[i], "[0-9-.]+")
        if (!consecutiveSuffixes(currentSection, thisSection)) { # we're at a section of a law interpolation
          if (!carriedTags[BIG_PART] != "") { 
            sectionInterpolated <- TRUE
          }
          currentSubsection <- thisSection
        } else { # we're at a section of the main document following a law interpolation
          currentSection <- thisSection
          sectionInterpolated <- FALSE
          carriedTags[BIG_PART] <- ""
        }
        # 2020-07-29
        extraNumText <- str_match(processedBill$Text[i], "[A-Za-z ]+[.]{1}[ ]+(1[.]{1})")
        if (!is.na(extraNumText[2])) {
          carriedTags[SECTION] <- massageFormat(newDepth, processedBill$Outline[i])
          newDepth <- p_09
          processedBill$Outline[i] <- extraNumText[2]
          carriedTags[p_09] <- extraNumText[2]
          processedBill$Level[i] <- p_09
        } # end 2020-07-09
      } else if (newDepth == p_AZ) {
        letters <- str_extract(processedBill$Outline[i], "[A-Z]{1,5}")
        l1 <- str_length(letters)
        l2 <- str_length(lastLetters)
        if ((l1 > 0) & (l2 > 0)) {
          if (length((substr(letters, l1, l1) != nextLetter(lastLetters))) != 1) { browser() } #####################################
          if (substr(letters, l1, l1) != nextLetter(lastLetters)) {
            if (!is.na(str_match(letters, "[IVX]+")[,1])) {  # is it big-roman?
              if (str_length(str_match(letters, "[IVX]+")[,1]) == str_length(letters))
              {  # it's big-roman
                newDepth <- p_IVX
                processedBill$Level[i] <- p_IVX
                letters <- lastLetters
              } else {
                # it's not a p_AZ in correct sequence and not a big-roman
                message(str_c("Unexpected break in outline tag succession at item ", 
                              as.character(processedBill$Item[i]), ", 207"))
              }
            } else if (finalWord(processedBill$Text[i - 1]) %in% tagNames) {
              # It's not a real subsection id
              processedBill$Text[i - 1] <- str_c(processedBill$Text[i - 1], 
                                                 processedBill$Outline[i], 
                                                 processedBill$Text[i])
              processedBill$Text[i] <- ""
              processedBill$Outline[i] <- NA
              processedBill$Page_Lines[i - 1] <- 
                str_c(str_split(processedBill$Page_Lines[i - 1], " - ", 2)[[1]][1], 
                      " - ", 
                      str_split(processedBill$Page_Lines[i], " - ", 2)[[1]][2])
              letters <- lastLetters
            } else {
              message(str_c("Unexpected break in outline tag succession at item ", 
                            as.character(processedBill$Item[i]), ", 223"))
            }
          }
        } else if (letters != "A") { # maybe there is no lastLetters
          if (!is.na(str_match(letters, "[IVX]+")[,1])) {
            if (str_length(str_match(letters, "[IVX]+")[,1]) == str_length(letters)) {
              newDepth <- p_IVX
              processedBill$Level[i] <- p_IVX
              letters <- lastLetters
            } else {
              message(str_c("Unexpected break in outline tag succession at item ", 
                            as.character(processedBill$Item[i]), ", 234"))
            }
          } else {
            message(str_c("Unexpected break in outline tag succession at item ", 
                          as.character(processedBill$Item[i]), ", 238"))
          }
        }
      } else if (newDepth == p_az) {
        letters <- str_extract(processedBill$Outline[i], "[a-z]{1,5}")
        l1 <- str_length(letters)
        l2 <- str_length(lastLetters)
        if ((l1 > 0) & (l2 > 0)) {
          if (length((substr(letters, l1, l1) != nextLetter(lastLetters))) != 1) { browser() } #####################################
          if (substr(letters, l1, l1) != nextLetter(lastLetters)) {
            if (!is.na(str_match(letters, "[ivx]+")[,1])) {  # is it little-roman?
              if (str_length(str_match(letters, "[ivx]+")[,1]) == str_length(letters))
              {  # it's little-roman
                newDepth <- p_ivx
                processedBill$Level[i] <- p_ivx
                letters <- lastLetters
              } else {
                # it's not a p_az in correct sequence and not a little-roman
                message(str_c("Unexpected break in outline tag succession at item ", 
                              as.character(processedBill$Item[i]), ", 256"))
              }
            } else if (finalWord(processedBill$Text[i - 1]) %in% tagNames) {
              # It's not a real subsection id
              processedBill$Text[i - 1] <- str_c(processedBill$Text[i - 1], 
                                                 processedBill$Outline[i], 
                                                 processedBill$Text[i])
              processedBill$Text[i] <- ""
              processedBill$Outline[i] <- NA
              processedBill$Page_Lines[i - 1] <- 
                str_c(str_split(processedBill$Page_Lines[i - 1], " - ", 2)[[1]][1], 
                      " - ", 
                      str_split(processedBill$Page_Lines[i], " - ", 2)[[1]][2])
              letters <- lastLetters
              newDepth <- depth
            } else {
              message(str_c("Unexpected break in outline tag succession at item ", 
                            as.character(processedBill$Item[i]), ", 273"))
            }
          }
        } else if ((letters != "a") &  # maybe there is no lastLetters
                   !is.na(str_match(letters, "[ivx]+")[,1])) {
          if (str_length(str_match(letters, "[ivx]+")[,1]) == str_length(letters)) {
            newDepth <- p_ivx
            processedBill$Level[i] <- p_ivx
            letters <- lastLetters
          } else {
            message(str_c("Unexpected break in outline tag succession at item ", 
                          as.character(processedBill$Item[i]), ", 284"))
          }
        } else if (finalWord(processedBill$Text[i - 1]) %in% tagNames) {
          # It's not a real subsection id
          processedBill$Text[i - 1] <- str_c(processedBill$Text[i - 1], 
                                             processedBill$Outline[i], 
                                             processedBill$Text[i])
          processedBill$Text[i] <- ""
          processedBill$Outline[i] <- NA
          processedBill$Page_Lines[i - 1] <- 
            str_c(str_split(processedBill$Page_Lines[i - 1], " - ", 2)[[1]][1], 
                  " - ", 
                  str_split(processedBill$Page_Lines[i], " - ", 2)[[1]][2])
          letters <- lastLetters
          newDepth <- depth
        } else {
          # Looks as if we're OK
        }
      } 
      lastLetters <- letters
      carriedTags[newDepth] <- massageFormat(newDepth, processedBill$Outline[i])
      carriedTags[(newDepth + 1):p_IVX] <- ""
      processedBill$Tag[i] <- outlineTag()
    } else if (newDepth == depth) {
      if (newDepth == p_09) {
        extra <- str_extract(processedBill$Outline[i], "[(]{1}[a-z]{1,2}[)]{1}")
        if (!is.na(extra)) {
          carriedTags[p_09] <- trimws(str_remove(processedBill$Outline[i], "[(]{1}[a-z]{1,2}[)]{1}"))
          carriedTags[p_az] <- extra
          lastLetters <- letters <- str_extract(extra, "[a-z]{1,2}")
          newDepth <- p_az
          processedBill$Level[i] <- p_az
        } else {
          carriedTags[p_09] <- processedBill$Outline[i]
        }
        processedBill$Tag[i] <- outlineTag()
      } else if (newDepth == p_az) {
        letters <- str_extract(processedBill$Outline[i], "[a-z]{1,2}")
        l1 <- str_length(letters)
        l2 <- str_length(lastLetters)
        if (length((substr(letters, l1, l1) != nextLetter(lastLetters))) != 1) { browser() } #####################################
        if ((l1 > 0) & (l2 > 0) & (substr(letters, l1, l1) != nextLetter(lastLetters))) {
          if (!is.na(str_match(letters, "([ivx]+)")[,1]) & 
              (str_length(str_match(letters, "([ivx]+)")[,1]) == str_length(letters))) {
            newDepth <- p_ivx
            processedBill$Level[i] <- p_ivx
            carriedTags[p_ivx] <- processedBill$Outline[i]
          } else if (finalWord(processedBill$Text[i - 1]) %in% tagNames) {
            # It's not a real subsection id
            processedBill$Text[i - 1] <- str_c(processedBill$Text[i - 1], 
                                               processedBill$Outline[i], 
                                               processedBill$Text[i])
            processedBill$Text[i] <- ""
            processedBill$Outline[i] <- NA
            processedBill$Page_Lines[i - 1] <- 
              str_c(str_split(processedBill$Page_Lines[i - 1], " - ", 2)[[1]][1], 
                    " - ", 
                    str_split(processedBill$Page_Lines[i], " - ", 2)[[1]][2])
            letters <- lastLetters
            newDepth <- depth
          } else {
            message("Unexpected break in outline tag succession at Item ", 
                    as.character(processedBill$Item[i]), ", 345")
            carriedTags[p_az] <- processedBill$Outline[i]
            lastLetters <- letters
          }
        } else {
          carriedTags[p_az] <- processedBill$Outline[i]
          lastLetters <- letters
        }
        processedBill$Tag[i] <- outlineTag()
      } else if (newDepth == SECTION) {
        thisSection <- str_extract(processedBill$Outline[i], "[0-9-.]+")
        if (carriedTags[BIG_PART] != "") {
          currentSubsection <- thisSection
        } else if (!consecutiveSuffixes(currentSection, thisSection)) {
          currentSubsection <- thisSection
          sectionInterpolated <- TRUE
        } else {
          currentSection <- thisSection
          sectionInterpolated <- FALSE
        }
        carriedTags[SECTION] <- massageFormat(newDepth, processedBill$Outline[i])
        # 2020-07-29
        extraNumText <- str_match(processedBill$Text[i], "[A-Za-z ]+[.]{1}[ ]+(1[.]{1})")
        if (!is.na(extraNumText[2])) {
          # browser()
          # message(str_c("matched: ", extraNumText[1], "; ", extraNumText[2]))
          carriedTags[SECTION] <- massageFormat(newDepth, processedBill$Outline[i])
          newDepth <- p_09
          processedBill$Outline[i] <- extraNumText[2]
          carriedTags[p_09] <- extraNumText[2]
          processedBill$Level[i] <- p_09
        } # end 2020-07-09
        processedBill$Tag[i] <- outlineTag()
      } else {
        carriedTags[newDepth] <- massageFormat(newDepth, processedBill$Outline[i])
        processedBill$Tag[i] <- outlineTag()
      }
    } else { # newDepth > depth 
      if ((newDepth > (depth + 1)) & 
          (finalWord(processedBill$Text[i - 1]) %in% tagNames)) {
        # It's not a real subsection/subparagraph id
        processedBill$Text[i - 1] <- str_c(processedBill$Text[i - 1], 
                                           processedBill$Outline[i], 
                                           " ",
                                           processedBill$Text[i])
        processedBill$Text[i] <- ""
        processedBill$Outline[i] <- NA
        processedBill$Page_Lines[i - 1] <- 
          str_c(str_split(processedBill$Page_Lines[i - 1], " - ", 2)[[1]][1], 
                " - ", 
                str_split(processedBill$Page_Lines[i], " - ", 2)[[1]][2])
        letters <- lastLetters
        newDepth <- depth
      } else if (newDepth == p_09) {
        extra <- str_extract(processedBill$Outline[i], "[(]{1}[a-z]{1,2}[)]{1}")
        if (!is.na(extra)) {
          carriedTags[p_09] <- str_remove(processedBill$Outline[i], "[(]{1}[a-z]{1,2}[)]{1}")
          carriedTags[p_az] <- extra
          lastLetters <- letters <- str_extract(extra, "[a-z]{1,2}")
          newDepth <- p_az
          processedBill$Level[i] <- p_az
        } else {
          carriedTags[p_09] <- processedBill$Outline[i]
        }
        processedBill$Tag[i] <- outlineTag()
      } else if (newDepth == p_az) {
        letters <- str_extract(processedBill$Outline[i], "[a-z]{1,2}")
        lastLetters <- letters
        carriedTags[newDepth] <- processedBill$Outline[i]
        processedBill$Tag[i] <- outlineTag()
      } else if (newDepth == SECTION) {
        thisSection <- str_extract(processedBill$Outline[i], "[0-9-.]+")
        if (consecutiveSuffixes(currentSection, thisSection)) {
          currentSection <- thisSection
        } else {
          currentSubsection <- thisSection
        } 
        carriedTags[newDepth] <- massageFormat(newDepth, processedBill$Outline[i])
        processedBill$Tag[i] <- outlineTag()
        # 2020-07-29
        extraNumText <- str_match(processedBill$Text[i], "[A-Za-z ]+[.]{1}[ ]+(1[.]{1})")
        if (!is.na(extraNumText[2])) {
          # browser()
          # message(str_c("matched: ", extraNumText[1], "; ", extraNumText[2]))
          carriedTags[SECTION] <- massageFormat(newDepth, processedBill$Outline[i])
          newDepth <- p_09
          processedBill$Outline[i] <- extraNumText[2]
          carriedTags[p_09] <- extraNumText[2]
          processedBill$Level[i] <- p_09
        } # end 2020-07-09
      } else {
        carriedTags[newDepth] <- massageFormat(newDepth, processedBill$Outline[i])
        processedBill$Tag[i] <- outlineTag()
      }
    }
    depth <- newDepth
  }
  select(processedBill, -Matches, -First_Match, -Level) %>%
    filter(!is.na(Outline))
}




