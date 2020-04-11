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

![](Preparations_lst_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

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

![](Preparations_lst_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
rank_subset <- freq_by_rank %>% filter(rank < 500,
rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
```

```
## 
## Call:
## lm(formula = log10(`term frequency`) ~ log10(rank), data = rank_subset)
## 
## Coefficients:
## (Intercept)  log10(rank)  
##     -1.4662      -0.6638
```

```r
freq_by_rank %>%
ggplot(aes(rank, `term frequency`, color = Book)) +
geom_abline(intercept = -1.46, slope = -0.66  , color = "gray50", linetype = 2) + geom_line(size = 1.1, alpha = 0.8, show.legend = T) +
scale_x_log10() +
scale_y_log10()
```

![](Preparations_lst_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
book_words1 <- book_words %>% bind_tf_idf(word, Book, n)
book_words1
```

```
## # A tibble: 3,667 x 7
##    Book   word        n total      tf   idf tf_idf
##    <chr>  <chr>   <int> <int>   <dbl> <dbl>  <dbl>
##  1 Husam  الانسان    79  4521 0.0175      0      0
##  2 Fenari واما       48  4820 0.00996     0      0
##  3 Husam  شي         41  4521 0.00907     0      0
##  4 Fenari شي         40  4820 0.00830     0      0
##  5 Fenari الانسان    39  4820 0.00809     0      0
##  6 Husam  انسان      38  4521 0.00841     0      0
##  7 Husam  واما       36  4521 0.00796     0      0
##  8 Husam  زيد        35  4521 0.00774     0      0
##  9 Husam  حيوان      33  4521 0.00730     0      0
## 10 Husam  ينتج       32  4521 0.00708     0      0
## # … with 3,657 more rows
```

```r
book_words1 %>% select(-total) %>% arrange(desc(tf_idf))
```

```
## # A tibble: 3,667 x 6
##    Book   word         n      tf   idf  tf_idf
##    <chr>  <chr>    <int>   <dbl> <dbl>   <dbl>
##  1 Husam  اخره        30 0.00664 0.693 0.00460
##  2 Husam  اقول        28 0.00619 0.693 0.00429
##  3 Fenari اريد         9 0.00187 0.693 0.00129
##  4 Fenari مفهوم        9 0.00187 0.693 0.00129
##  5 Fenari ويسمى        9 0.00187 0.693 0.00129
##  6 Husam  بالتنافي     8 0.00177 0.693 0.00123
##  7 Fenari فانهما       8 0.00166 0.693 0.00115
##  8 Fenari معرف         8 0.00166 0.693 0.00115
##  9 Husam  اختلفتا      7 0.00155 0.693 0.00107
## 10 Husam  هاتين        7 0.00155 0.693 0.00107
## # … with 3,657 more rows
```

```r
plot <- book_words1 %>%
arrange(desc(tf_idf)) %>%
mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(Book) %>%
top_n(20) %>%
ungroup %>%
ggplot(aes(word, tf_idf, fill = Book)) + geom_col(show.legend = FALSE) +
labs(x = NULL, y = "tf-idf") +
facet_wrap(~Book, ncol = 2, scales = "free") + coord_flip()

ggplotly(plot)
```

<!--html_preserve--><div id="htmlwidget-bf95bc072ef7220d352e" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-bf95bc072ef7220d352e">{"x":{"data":[{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999],"base":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"x":[0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000719032344979196,0.000862838813975036,0.000862838813975036,0.00100664528297087,0.00100664528297087,0.00100664528297087,0.00100664528297087,0.00100664528297087,0.00115045175196671,0.00115045175196671,0.00129425822096255,0.00129425822096255,0.00129425822096255],"y":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26],"text":["word: يكذبان<br />tf_idf: 0.0007190323<br />Book: Fenari","word: والتعريف<br />tf_idf: 0.0007190323<br />Book: Fenari","word: معرفة<br />tf_idf: 0.0007190323<br />Book: Fenari","word: مباحث<br />tf_idf: 0.0007190323<br />Book: Fenari","word: الوضع<br />tf_idf: 0.0007190323<br />Book: Fenari","word: النظر<br />tf_idf: 0.0007190323<br />Book: Fenari","word: المميز<br />tf_idf: 0.0007190323<br />Book: Fenari","word: المشاركات<br />tf_idf: 0.0007190323<br />Book: Fenari","word: الجوهر<br />tf_idf: 0.0007190323<br />Book: Fenari","word: التصور<br />tf_idf: 0.0007190323<br />Book: Fenari","word: الانتاج<br />tf_idf: 0.0007190323<br />Book: Fenari","word: اشار<br />tf_idf: 0.0007190323<br />Book: Fenari","word: استثنا<br />tf_idf: 0.0007190323<br />Book: Fenari","word: احديهما<br />tf_idf: 0.0007190323<br />Book: Fenari","word: يصدقان<br />tf_idf: 0.0008628388<br />Book: Fenari","word: البعيد<br />tf_idf: 0.0008628388<br />Book: Fenari","word: جزاين<br />tf_idf: 0.0010066453<br />Book: Fenari","word: باعتبار<br />tf_idf: 0.0010066453<br />Book: Fenari","word: المعرف<br />tf_idf: 0.0010066453<br />Book: Fenari","word: اللزوم<br />tf_idf: 0.0010066453<br />Book: Fenari","word: احتراز<br />tf_idf: 0.0010066453<br />Book: Fenari","word: معرف<br />tf_idf: 0.0011504518<br />Book: Fenari","word: فانهما<br />tf_idf: 0.0011504518<br />Book: Fenari","word: ويسمى<br />tf_idf: 0.0012942582<br />Book: Fenari","word: مفهوم<br />tf_idf: 0.0012942582<br />Book: Fenari","word: اريد<br />tf_idf: 0.0012942582<br />Book: Fenari"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Fenari","legendgroup":"Fenari","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999],"base":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"x":[0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000613268905604906,0.000766586132006133,0.000766586132006133,0.000766586132006133,0.000766586132006133,0.000766586132006133,0.000766586132006133,0.000766586132006133,0.000919903358407359,0.000919903358407359,0.000919903358407359,0.000919903358407359,0.000919903358407359,0.000919903358407359,0.00107322058480859,0.00107322058480859,0.00122653781120981,0.00429288233923434,0.0045995167920368],"y":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"text":["word: والجزئية<br />tf_idf: 0.0006132689<br />Book: Husam","word: موضوعها<br />tf_idf: 0.0006132689<br />Book: Husam","word: موجبتين<br />tf_idf: 0.0006132689<br />Book: Husam","word: مقدمتين<br />tf_idf: 0.0006132689<br />Book: Husam","word: مختصة<br />tf_idf: 0.0006132689<br />Book: Husam","word: لامتناع<br />tf_idf: 0.0006132689<br />Book: Husam","word: كلى<br />tf_idf: 0.0006132689<br />Book: Husam","word: كانتا<br />tf_idf: 0.0006132689<br />Book: Husam","word: فلانه<br />tf_idf: 0.0006132689<br />Book: Husam","word: فردا<br />tf_idf: 0.0006132689<br />Book: Husam","word: ذكرناها<br />tf_idf: 0.0006132689<br />Book: Husam","word: انعكاسها<br />tf_idf: 0.0006132689<br />Book: Husam","word: مقولا<br />tf_idf: 0.0007665861<br />Book: Husam","word: تتناقضا<br />tf_idf: 0.0007665861<br />Book: Husam","word: بقولنا<br />tf_idf: 0.0007665861<br />Book: Husam","word: المنطقية<br />tf_idf: 0.0007665861<br />Book: Husam","word: الشرط<br />tf_idf: 0.0007665861<br />Book: Husam","word: الباقية<br />tf_idf: 0.0007665861<br />Book: Husam","word: الاقوال<br />tf_idf: 0.0007665861<br />Book: Husam","word: معينا<br />tf_idf: 0.0009199034<br />Book: Husam","word: قضية<br />tf_idf: 0.0009199034<br />Book: Husam","word: سميت<br />tf_idf: 0.0009199034<br />Book: Husam","word: سئل<br />tf_idf: 0.0009199034<br />Book: Husam","word: جزئيها<br />tf_idf: 0.0009199034<br />Book: Husam","word: الاصطلاحات<br />tf_idf: 0.0009199034<br />Book: Husam","word: هاتين<br />tf_idf: 0.0010732206<br />Book: Husam","word: اختلفتا<br />tf_idf: 0.0010732206<br />Book: Husam","word: بالتنافي<br />tf_idf: 0.0012265378<br />Book: Husam","word: اقول<br />tf_idf: 0.0042928823<br />Book: Husam","word: اخره<br />tf_idf: 0.0045995168<br />Book: Husam"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Husam","legendgroup":"Husam","showlegend":true,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":37.9178082191781,"r":7.30593607305936,"b":40.1826484018265,"l":63.5616438356165},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,0.401164284725929],"automargin":true,"type":"linear","autorange":false,"range":[-6.47129110481277e-05,0.00135897113201068],"tickmode":"array","ticktext":["0e+00","5e-04","1e-03"],"tickvals":[0,0.0005,0.001],"categoryorder":"array","categoryarray":["0e+00","5e-04","1e-03"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"","hoverformat":".2f"},"annotations":[{"text":"tf-idf","x":0.5,"y":-0.0471841704718417,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis"},{"text":"Fenari","x":0.200582142362964,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Husam","x":0.799417857637036,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,26.6],"tickmode":"array","ticktext":["يكذبان","والتعريف","معرفة","مباحث","الوضع","النظر","المميز","المشاركات","الجوهر","التصور","الانتاج","اشار","استثنا","احديهما","يصدقان","البعيد","جزاين","باعتبار","المعرف","اللزوم","احتراز","معرف","فانهما","ويسمى","مفهوم","اريد"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26],"categoryorder":"array","categoryarray":["يكذبان","والتعريف","معرفة","مباحث","الوضع","النظر","المميز","المشاركات","الجوهر","التصور","الانتاج","اشار","استثنا","احديهما","يصدقان","البعيد","جزاين","باعتبار","المعرف","اللزوم","احتراز","معرف","فانهما","ويسمى","مفهوم","اريد"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.401164284725929,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.401164284725929,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.598835715274071,"x1":1,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.598835715274071,"x1":1,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[-0.00022997583960184,0.00482949263163864],"tickmode":"array","ticktext":["0.000","0.001","0.002","0.003","0.004"],"tickvals":[0,0.001,0.002,0.003,0.004],"categoryorder":"array","categoryarray":["0.000","0.001","0.002","0.003","0.004"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.598835715274071,1],"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","hoverformat":".2f"},"yaxis2":{"type":"linear","autorange":false,"range":[0.4,30.6],"tickmode":"array","ticktext":["والجزئية","موضوعها","موجبتين","مقدمتين","مختصة","لامتناع","كلى","كانتا","فلانه","فردا","ذكرناها","انعكاسها","مقولا","تتناقضا","بقولنا","المنطقية","الشرط","الباقية","الاقوال","معينا","قضية","سميت","سئل","جزئيها","الاصطلاحات","هاتين","اختلفتا","بالتنافي","اقول","اخره"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"categoryorder":"array","categoryarray":["والجزئية","موضوعها","موجبتين","مقدمتين","مختصة","لامتناع","كلى","كانتا","فلانه","فردا","ذكرناها","انعكاسها","مقولا","تتناقضا","بقولنا","المنطقية","الشرط","الباقية","الاقوال","معينا","قضية","سميت","سئل","جزئيها","الاصطلاحات","هاتين","اختلفتا","بالتنافي","اقول","اخره"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,1],"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x2","title":"","hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":1},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"49d360fea3ae":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"49d360fea3ae","visdat":{"49d360fea3ae":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
