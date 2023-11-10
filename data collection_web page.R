# load the necessary libraries
libs <- c('tidyverse','janitor','rvest','stringr')
lapply(libs,library,character.only=T)

# give the URL to the page 
link <- 'https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&sort=user_rating,desc' 
page <- read_html(link)

name <- page %>% 
  rvest::html_nodes('.lister-item-header a') %>% 
  rvest::html_text()
# package::function. the part preceding the two colons is not necessary
# it only clears the package a certain function comes from

imdb_links <- page %>% 
  html_nodes('.lister-item-header a') %>% 
  html_attr('href') %>% 
  stringr::str_replace(pattern = fixed("?ref_=adv_li_tt"), replacement = fixed("fullcredits?ref_=tt_cl_sm")) %>%
  paste0("http://www.imdb.com",  .) 
# ^ paste0 skips the space by default. If paste(), then gotta use sep = '' argument
paste('https://www.imdb.com', ., sep = '')

year <- page %>% 
  html_nodes('.text-muted.unbold') %>% 
  html_text()

rating <- page %>% 
  html_nodes('.ratings-imdb-rating strong') %>% 
  html_text()

get_cast <- function(movie_link){
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes('.primary_photo+ td a , .primary_photo+ td .wiseone-analysis-result-entity') %>% 
    html_text() %>% 
    paste(collapse = ',') %>% 
    return(movie_cast)
}

# this will take time (progress indicated by red button in console upper-right corner)
cast = sapply(imdb_links, FUN = get_cast, USE.NAMES = FALSE)

view(cast)

# practicing some data hygiene 
cast <- gsub('\n','',cast)


# bring all variables into forming a data frame 
imdb <- data.frame(name,year,rating, cast, stringsAsFactors = FALSE)

view(imdb)