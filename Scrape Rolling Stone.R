install.packages('RSelenium')

library(tidyverse)
library(rvest)
library(RSelenium)
library(googlesheets4)

rD <- RSelenium::rsDriver()
remDr <- rD[["client"]]
remDr$open(silent = T)

df <- data.frame(endpoints = c('arcade-fire-%ef%bb%bffuneral-1062733/', 'linda-mccartney-and-paul-ram-1062783/', 'the-go-gos-beauty-and-the-beat-1062833/',
                               'stevie-wonder-music-of-my-mind-2-1062883/', 'shania-twain-come-on-over-1062933/', 'buzzcocks-singles-going-steady-2-1062983/',
                               'sade-diamond-life-1063033/', 'bruce-springsteen-nebraska-3-1063083/', 'the-band-music-from-big-pink-2-1063133/',
                               'jay-z-the-blueprint-3-1063183/'),
               numbers = c(0,1,2,3,4,5,6,7,8,9),
               order = c(1,2,3,4,5,6,7,8,9,10),
               div = c(1,2,2,2,2,2,2,2,2,2))

namelist <- list()

for (j in 1:10){
  url <- paste0('https://www.rollingstone.com/music/music-lists/best-albums-of-all-time-1062063/',df$endpoints[which(df$order == j)])
  remDr$navigate(url)
  remDr$maxWindowSize()
  path1 <- paste0('//*[@id="pmc-gallery-vertical"]/div[',df$div[which(df$order==j)],']/div/div[')
  
  for (i in 1:50) {
    k <- (j-1)*50 + i
    path2 <- paste0(path1,i,']/article/h2')
    x <- remDr$findElement(using = 'xpath',
                         value = path2)
    
    namelist[[k]] <- x$getElementText()
  }
  
}

set <- data.frame(part = str_split_fixed(unlist(do.call('rbind',namelist)),", '",2)) %>%
  mutate(part.2 = recode(part.2, "<U+FEFF>Funeral" = "Funeral")) %>%
  mutate(part.2 = substr(part.2,1,nchar(part.2)-1)) %>%
  rename(artist = part.1,
         album = part.2)

write_sheet(set, ss = '1NaYuJGSfPWnkKpcpeQ94-BmJCHM9G9UpKrei6edmDSE', sheet = 'raw')
