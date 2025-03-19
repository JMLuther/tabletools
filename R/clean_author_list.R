#' Reformat list of authors into Bibliography format
#'
#' Reformats from typical list of full names (First middle Last,...) to standard
#' bibliographic format (Last FM, ...). Strips out numbers. Will not work for
#' some double and/or hyphenated names or other edge cases which will still have
#' to be cleaned manually.
#'
#' @param string_authors A text list of authors
#'
#' @return String of reformatted names
#' @export
#'
#' @examples
#' authors <- "Desmaré van Rooyen1,2, Sascha Bandulik3, Chandan Kumar-Sinha4,5, 
#'   Aaron M. Udager4,5,6, Chaelin Lee2, Heather Wachtel7, Debbie L. Cohen8, 
#'   James M. Luther9, Thomas Giordano6, Adina Turcu2, Richard Warth3, 
#'   William E. Rainey1,2 and Juilee Rege*1"
#' clean_author_list(authors)
#' auth <- "B. Ilkin Safa1, Jared Oakes1, Leslie Kirk1, Eric Olson1, 
#' Christian M. Warren1, Erin M. Wilfong1, Matthew T. Stier1, Samuel S. Bailin1,
#'  Curtis Gabriel1, Jeffrey C. Rathmell1, C. Robb Flynn1, J. Matthew Luther1, 
#'  John R. Koethe1,2, Spyros A. Kalams1, Celestine N. Wanjalla1, Mona Mashayekhi1"
#' clean_author_list(auth) 

clean_author_list <- function(string_authors){
  # remove numbers, ., line breaks and whitespace
  au1 <- gsub("[0-9\\*\\.\r\n]+", "", trimws(string_authors))
  au1 <- gsub(",[0-9]+", "", au1)
  
  # # remove titles
  au1 <- gsub("MD|MPH|Phd|MS+", "", trimws(au1))
  # au1 <- gsub("MD", "", trimws(au1))
  # remove "and" and whitespace
  au1 <- gsub("and[ ]*", "", au1)

  # extract names from list separated by commas
  au1 <- strsplit(au1, ",")[[1]]
  # remove leading and trailing whitespace
  au1 <- trimws(au1)
  # handle common last name prefix; will remove later
  au1 <- gsub(" (del|van) ", " \\1:", au1, ignore.case = T)
  # first initial; assumes Firstname ... Lastname format
  ini_first <-   gsub("\\<([A-Za-z]).*", "\\1", au1)
  
  # get count of words in each element
  word_n <- lengths(strsplit(au1, " "))
  # if more than 2 words, get first letter of second word
  ini_middle <- gsub(".* \\<([A-Za-z]).* .*", "\\1", au1)
  
  # ini_middle <- gsub(".* \\<(\p{L}).*", "\\1", au1)
  ini_middle[word_n <=2] <- ""
  
  # last name; assumes Firstname ... Lastname format
  name_last <- gsub(".* \\<(.*)$", "\\1", au1)
  # remove placeholder :
  name_last <- gsub("(:)", " ", name_last)
  # paste together
  name_inits <- paste0(name_last, " ", ini_first, ini_middle)
  
  # data.frame(au1, name_last, ini_first, ini_middle, name_inits) # checking
  paste0(name_inits[name_inits != " "], collapse = ", ") # return value
}
