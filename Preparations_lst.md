---
title: "doha"
subtitle: ""
author: ""
date: '2020-04-11'
fig_width: 6 
fig_height: 4 
output: 
  html_document:
    keep_md: true
---


```r
library(tidyverse)
library(readOffice)
library(tidytext)
library(readxl)
library(arabicStemR)
library(plotly)


stop <- read_excel("stops_.xlsx")
fenar.raw <- read_docx("fenar.docx")
husam.raw <- read_docx("husam.docx")

fenar.regulated <-  str_c(fenar.raw,collapse=' ')
husam.regulated <-  str_c(husam.raw,collapse=' ')

 fenar.cleaned <- fenar.regulated %>% 
  cleanChars() %>% 
  fixAlifs() %>% 
  removeDiacritics() %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  cleanLatinChars() %>% 
  removeNewlineChars()
 
  husam.cleaned <- husam.regulated %>% 
  cleanChars() %>% 
  fixAlifs() %>% 
  removeDiacritics() %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  cleanLatinChars() %>% 
  removeNewlineChars()

fenar.frame <- data_frame(Book = "Fenari", fenar.cleaned = fenar.cleaned)
husam.frame <- data_frame(Book = "Husam", husam.cleaned = husam.cleaned)

fenar <- fenar.frame %>% unnest_tokens(word, fenar.cleaned)
husam <- husam.frame %>% unnest_tokens(word, husam.cleaned)

merged <- bind_rows(fenar, husam)

merged1 <- merged %>% anti_join(stop)


book_words <- merged1 %>% count(Book, word, sort = TRUE) %>% ungroup()

total_words <- book_words %>% group_by(Book) %>% summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words
```

```
## # A tibble: 3,667 x 4
##    Book   word        n total
##    <chr>  <chr>   <int> <int>
##  1 Husam  الانسان    79  4521
##  2 Fenari واما       48  4820
##  3 Husam  شي         41  4521
##  4 Fenari شي         40  4820
##  5 Fenari الانسان    39  4820
##  6 Husam  انسان      38  4521
##  7 Husam  واما       36  4521
##  8 Husam  زيد        35  4521
##  9 Husam  حيوان      33  4521
## 10 Husam  ينتج       32  4521
## # … with 3,657 more rows
```


```r
ggplot(book_words, aes(n/total, fill = Book)) + geom_histogram(show.legend = FALSE) + xlim(NA, 0.008) +
facet_wrap(~Book, ncol = 2, scales = "free_y")
```

![](https://github.com/oztop/arabictextanalysis/blob/master/unnamed-chunk-1-1.png)<!-- -->



```r
freq_by_rank <- book_words %>% group_by(Book) %>% mutate(rank = row_number(),
`term frequency` = n/total) 

freq_by_rank
```

```
## # A tibble: 3,667 x 6
## # Groups:   Book [2]
##    Book   word        n total  rank `term frequency`
##    <chr>  <chr>   <int> <int> <int>            <dbl>
##  1 Husam  الانسان    79  4521     1          0.0175 
##  2 Fenari واما       48  4820     1          0.00996
##  3 Husam  شي         41  4521     2          0.00907
##  4 Fenari شي         40  4820     2          0.00830
##  5 Fenari الانسان    39  4820     3          0.00809
##  6 Husam  انسان      38  4521     3          0.00841
##  7 Husam  واما       36  4521     4          0.00796
##  8 Husam  زيد        35  4521     5          0.00774
##  9 Husam  حيوان      33  4521     6          0.00730
## 10 Husam  ينتج       32  4521     7          0.00708
## # … with 3,657 more rows
```

```r
freq_by_rank %>%
ggplot(aes(rank, `term frequency`, color = Book)) + geom_line(size = 1.1, alpha = 0.8, show.legend = T) + scale_x_log10() +
scale_y_log10()
```

![](Preparations_lst_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
