
# load packages
library(pacman)
p_load(tidyverse,glue,quanteda,haven)

# load nyt articles, input is doca_nyt
load("./lab6/doca_nyt.rdata")

# load doca data, the link is 
# https://web.stanford.edu/group/collectiveaction/cgi-bin/drupal/node/7?email=yongjun.zhang%40stonybrook.edu&inst=SBU

list.files(path="./lab6",pattern = ".dta")

doca_meta <- read_dta("./lab6/final_data_v10.dta",
                      encoding = "latin1")

doca <- doca_meta %>% 
  left_join(doca_nyt %>% 
              mutate(title_doca=toupper(title_doca)),
            by=c("title"="title_doca")) %>% 
  select(text,title,everything())


doca_nyt$text[1]


doca_nyt_df <- doca_nyt[1:50,] %>% 
  mutate(title_created=str_extract(text,"^.*?\\n"),
         text=str_replace_all(text,"Reproduced with permission.*?without permission.","") %>% 
           str_replace_all("\\n|[0-9]|[:punct:]",""))

# tokenize our texts

library(tidytext)

nyt_df <- doca_nyt_df %>% 
  mutate(uid=row_number()) %>% 
  unnest_tokens(word, text,token = "ngrams", n = 1) %>% 
  group_by(uid,word) %>% 
  summarise(term_fr=n()) %>% 
  pivot_wider(names_from = "word",values_from="term_fr",values_fill=0)



