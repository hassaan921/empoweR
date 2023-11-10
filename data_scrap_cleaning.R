# load the necessary libraries
libs <- c('tidyverse','janitor','rvest','stringr')
lapply(libs,library,character.only=T)

# to scrape multiple pages for multiple variables

get_cast <- function(movie_link){
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes('.primary_photo+ td a , .primary_photo+ td .wiseone-analysis-result-entity') %>% 
    html_text() %>% 
    paste(collapse = ',') %>% 
    return(movie_cast)
}
# ^ this does not need to run each time so we keep it outside the loop

movies <- data.frame()
# ^ this, if put inside the loop, will only show the last page, so we put one out here

for (page_result in seq(from = 1, to = 51, by = 50)){
  link <- paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                 page_result, "&ref_=adv_nxt") # link if multiple pages to scrape
  page <- read_html(link)
  
  name <- page %>% 
    html_nodes('.lister-item-header a') %>% 
    html_text()
  
  imdb_links <- page %>% 
    html_nodes('.lister-item-header a') %>% 
    html_attr('href') %>% 
    str_replace(pattern = fixed("?ref_=adv_li_tt"), replacement = fixed("fullcredits?ref_=tt_cl_sm")) %>%
    paste("https://www.imdb.com", ., sep="")
  
  year <- page %>% 
    html_nodes('.text-muted.unbold') %>% 
    html_text()
  
  rating <- page %>% 
    html_nodes('.ratings-imdb-rating strong') %>% 
    html_text()
  
  cast = sapply(imdb_links, FUN = get_cast, USE.NAMES = FALSE)
  cast <- gsub('\n','',cast)
} #change to = according to need and how URL changes


imdb <- rbind(movies, data.frame(name,year,rating, cast, stringsAsFactors = FALSE))

print(paste0('Page:', page_result)) # it will print page results as each page finishes running

view(imdb)
# ^ this did not work. Check for issues from code below

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

movies = data.frame()

for (page_result in seq(from = 1, to = 51, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, cast, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

view(movies)
