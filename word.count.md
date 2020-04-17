---
  
  
title: "Basic Word Cound Analysis of Three Arabic Classics"
subtitle: "Hilya al-Awliya, Mafatih al-Ghayb and Ihya al-Ulum"
author: "Abdullah Oztop (@abdullahoztop)"
date: 18/4/2020
fig_width: 6
fig_height: 4
output:
  html_document:
    keep_md: true
    
---
  
  
  First and foremost, upload the necessary libraries, then import the texts via Kitab Project's github repo.
I choose Hilya al-Awliya, Mafatih al-Ghayb and Ihya al-Ulum.


```{r message=FALSE, warning=FALSE}


library(tidyverse)
library(RCurl)
library(arabicStemR)
library(gt)
library(tidytext)
library(data.table)
library(stringi)
library(lubridate)
library(readxl)
library(readOffice)
library(plotly)


stop <- read_xlsx("stop.xlsx")


hilya <- getURL("https://raw.githubusercontent.com/OpenITI/0450AH/master/data/0430AbuNucaymIsbahani/0430AbuNucaymIsbahani.HilyaAwliya/0430AbuNucaymIsbahani.HilyaAwliya.JK000022-ara1.completed")
mafatih <- getURL("https://raw.githubusercontent.com/OpenITI/0625AH/master/data/0606FakhrDinRazi/0606FakhrDinRazi.MafatihGhayb/0606FakhrDinRazi.MafatihGhayb.Shamela0023635-ara1")
ihya <- getURL("https://raw.githubusercontent.com/OpenITI/0525AH/master/data/0505Ghazali/0505Ghazali.IhyaCulumDin/0505Ghazali.IhyaCulumDin.JK000001-ara1")








```


Then I define my function to clean and tidy the texts.
```{r message=FALSE, warning=FALSE}




clean <-  function(txt) {
  
    text.c <- txt %>%
      cleanChars() %>%
      fixAlifs() %>%
      removeDiacritics() %>%
      removePunctuation() %>%
      removeNumbers() %>%
      removeNewlineChars()
    
    text.d <- data.frame(word = unlist(strsplit(as.character(text.c), " ")))
    
    text.s <- anti_join(text.d, stop)
    
    text.e <- text.s %>% count(word, sort=T)
    
    print(text.e)
}




```


Then apply the function to the three texts.


```{r message=FALSE, warning=FALSE}
hil <- clean(hilya)
maf <- clean(mafatih)
ih <- clean(ihya)


# Now merge the texts data into one dataset


merged.half <- full_join(ih, hil, by="word")


merged <- full_join(merged.half, maf, by="word")


```


```{r message=FALSE, warning=FALSE}
# Modifiy the names


names(merged) <- c("Word", "Ihya", "Hilya", "Mafatih")


merged$total <- merged$Ihya+merged$Hilya+merged$Mafatih


merged.full <- gather(merged, "Work","Count", 2:4)


```




Now I arrange the dataset to visualize it.


```{r message=FALSE, warning=FALSE}
to.be.plotted <- merged.full %>% arrange(desc(total)) %>% head(n=60)


#See the dataset before visualization


head(to.be.plotted) %>% gt()
```


To obtain a shiny graph compatible with Arabic, I use plotly, not ggplot.




```{r message=FALSE, warning=FALSE}
plotly <- to.be.plotted %>%
  mutate(Work=as.factor(Work),
         Word=fct_reorder(Word, Count, .fun = "sum")) %>%
  ggplot(aes(Word,Count, fill=Work)) +
  geom_col() +
  coord_flip() +
   theme_minimal() +
  theme(axis.text.y = element_text(size=18))
  
ggplotly(plotly)


```




