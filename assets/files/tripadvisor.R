# tripadvisor scraping

# we use rselenium
# https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html

require(pacman)

p_load(RSelenium,rvest,tidyverse,sys)

url <- "https://www.tripadvisor.com/Attraction_Review-g187323-d617423-Reviews-The_Holocaust_Memorial_Memorial_to_the_Murdered_Jews_of_Europe-Berlin.html"

rD <- rsDriver(browser="firefox", port=45354L, verbose=T)
remDr <- rD[["client"]]
remDr$navigate(url)


# First wait for the DOM to load by pausing for a couple seconds.
Sys.sleep(2) 

### LET us have a container for our data

tripadvisor_df <- tibble()

### LET US Scrape the first page
page_num <-  1
cat("We are scraping: ",page_num,"\n")
# now scroll down your window to bottom to load all page content
remDr$executeScript("window.scrollTo(0,document.body.scrollHeight)")
html <- read_html(remDr$findElement("id","REVIEWS")$getPageSource()[[1]])

reviews <- html %>% # Name a variable reviews and take a look at the html
  html_elements(".dHjBB") %>% # In particular, look at the div class named dHjBB
  html_children() %>% # Look at the child of the named class
  html_text() %>% # Grab the text contained with this class
  as_tibble() %>% # And save it as a tibble to reviews
  filter(!str_detect(value,"Showing results"))

print(reviews$value)

## append to our container

tripadvisor_df <- bind_rows(tripadvisor_df,reviews)

# navigate to next page

remDr$findElements("class", "eRhUG")[[1]]$clickElement()

# redo everything
# you have 17,826 reviews, each page has 10 reviews, so it would be 1783 pages

#for(page_num in seq(2,1783,1)){
for(page_num in seq(2,10,1)){
  cat("We are scraping: ",page_num,"\n")
  # now scroll down your window to bottom to load all page content
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight)")
  Sys.sleep(1)
  html <- read_html(remDr$findElement("id","REVIEWS")$getPageSource()[[1]])
  reviews <- html %>% # Name a variable reviews and take a look at the html
    html_elements(".dHjBB") %>% # In particular, look at the div class named dHjBB
    html_children() %>% # Look at the child of the named class
    html_text() %>% # Grab the text contained with this class
    as_tibble() %>% # And save it as a tibble to reviews
    filter(!str_detect(value,"Showing results"))
  
  print(reviews$value)
  ## append to our container
  tripadvisor_df <- bind_rows(tripadvisor_df,reviews)
  page_num <- page_num+1
  # move to next page
  remDr$findElements("class", "eRhUG")[[1]]$clickElement()
  Sys.sleep(3)
}

write_csv(
  tripadvisor_df,
  "./reviews.csv",
  na = "NA",
  col_names = FALSE)





