# Print hello world
library(tidyverse)

cat("Let us start with your first R function")

print("Hello World! This is R")

a <- 1
b <- 1 + 1

install.packages("tidyverse")
library(tidyverse)

setwd("~/Documents/intro2css/assets/files")
getwd()
dir()

1:10
seq(1, 10, 1)
rep(1, 10)
rep("a", 10)
rep(T, 10)

for (i in 1:10) {
  a <- print(i ** 2)
}


library(tidyverse)

squared <- function(x){
  return(x ** 2)
}

map(1:10, squared)
map_dbl(1:10, ~ . ** 2)
map_chr(1:10, ~ as.character(.))
a <- map(1:10, ~ . ** 2)
a <- map_dbl(1:10, ~ . ** 2)
a <- map_chr(1:10, ~ as.character(.))
class(a)

map_dbl(1:10, squared)
1:10 %>% map_dbl(squared)
# a(b(c(d(e(f(x))))))
# x %>% 
#   f() %>% 
#   e() %>% 
#   d() %>% 
#   c() %>% 
#   b() %>% 
#   a()

for (i in 1:2) {
  for (j in 1:5) {
    print(i * j)
  }
}

1:2
1:5
rep(1:2, 5)
rep(1:5, 2)
map2_dbl(rep(1:2, 5), 
     rep(1:5, 2), 
     ~ .x * .y)

fruits <- c("apple", "banana", "lemon")
str_detect(fruits, "le")
str_detect(fruits, "^le")
str_detect(fruits, "le$")
str_extract_all(fruits, "na")
str_extract_all(fruits, "[bn]a")
str_extract_all(fruits, "[a-z]a")
str_extract_all(fruits, "(b|n)a")
str_extract_all(fruits, "(apple|banana)")

telephone <- c("+852 12345678",
  "+852 12349378",
  "+852 12345298",
  "+86 12345678",
  "+852 1234567")

str_detect(telephone, "^[+]852 \\d{8}")
str_extract_all(telephone, "\\d{8}")
str_extract_all(telephone, "\\d+")

str_extract_all(telephone, "(?<=852\\s)\\d{8}")
