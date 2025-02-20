library(tidyverse)
library(stringr)
#Step 1
allentown = read.csv(file = "data/essentia.data.allentown.csv") 
data.tibb = read_csv(file = "data/essentia.data.csv")  
feat.funct = function(data, feature, allentown)
{
  data |>
    group_by(artist) |>
    #2.2
    summarize(minimum = min(get(feature), na.rm=T), 
              maximum = max(get(feature), na.rm=T), 
              LF = quantile(get(feature),.25, na.rm=T) - 1.5*IQR(get(feature), na.rm=T), 
              UF = quantile(get(feature), .75,na.rm=T) + 1.5*IQR(get(feature), na.rm=T)) |>
   #2.3
   mutate(out.of.range = allentown[[feature]] < minimum | allentown[[feature]] > maximum) |>
   mutate(unusual = allentown[[feature]] < LF | allentown[[feature]] > UF) |>
   #2.4
   mutate(description = case_when(out.of.range ~ "Out of Range",
                                  unusual ~ "Outlying",
                                  TRUE ~ "Within Range")) 
}
#Step 2
#To store necessary features
features.keep = tibble(c(artist, album, track, feature))
for(cols in colnames(data.tibb))
{
  
  if(class(data.tibb[[cols]]) == "numeric")
  {
    result = feat.funct(data.tibb, cols, allentown) |>
    if(str_count(description,"Out of Range") == 1)
    {
      append(features.keep, )
    }
    print(result)
  }
}


