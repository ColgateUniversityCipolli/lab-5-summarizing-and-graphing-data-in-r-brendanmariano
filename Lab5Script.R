library(tidyverse)
library(stringr)
library(xtable)
library(patchwork)
#Step 1
allentown = read_csv(file = "data/essentia.data.allentown.csv")
view(allentown)
data.tibb = read_csv(file = "data/essentia.data.csv")  
view(data.tibb)
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
view.features = tibble(the.frontb = character(), 
                       man.orch = character(), 
                       all.get.out = character(), 
                       feature = character())
features.keep = tibble(the.frontb = character(), 
                         man.orch = character(), 
                         all.get.out = character(), 
                         feature = character())
desired.feat.names = c("spectral_skewness", "melbands_spread",
                       "Perception", "chords_strength", "erbbands_skewness",
                       "average_loudness", "danceability", "Cognition",
                       "power", "dissonance")
                       
for(cols in colnames(data.tibb))
{
  #Analyzes numeric columns
  if(class(data.tibb[[cols]]) == "numeric")
  {
    result = feat.funct(data.tibb, cols, allentown) 
      newRow = tibble(all.get.out = result$description[1], 
                       man.orch = result$description[2], 
                        the.frontb = result$description[3],
                        feature = cols)
    if(sum(str_count("Within Range", result$description)) == 2 
       | sum(str_count("Within Range", result$description)) == 1) 
    {
      view.features = view.features|>
        bind_rows(newRow)
        print(cols)
        print(result)
        if(cols %in% desired.feat.names)
        { 
          features.keep = features.keep |>
            bind_rows(newRow)
        }
    }
  }
}
#finds number of times each band was within range overall
view(view.features)
#NOTE FROM OFFICE HOURS: Can use xtable function to copy and paste table into 
#sweeve document. 
#Should know how to create box plots
#Can make for loop that goes through and creates graphs
#Find features that matter in general even if they are the same

#Puts select features into 
write_csv(features.keep, "featuresToKeep.csv")

#Step 4
#MY SELF CREATED PLOT FOR DISSONANCE
#Loading Data
##################################
dat = read_csv("data/essentia.data.csv")
#################################
# Plots Dissonance
#################################
p1 = ggplot(data = dat1) + 
  geom_boxplot(aes(x = artist, y = dissonance)) +
  theme_light() +
  geom_hline(yintercept = pull(allentown, dissonance))
#################################
# Plot of Spectral_Skewness
#################################
p2 = ggplot(data = dat1) + 
  geom_boxplot((aes(x = artist, y = spectral_skewness))) +
  theme_light() +
  geom_hline(yintercept = pull(allentown, spectral_skewness))
################################
# Plots melbands_spread
################################
p3 = ggplot(data = dat1) + 
  geom_boxplot((aes(x = artist, y = melbands_spread))) +
  theme_light() +
  geom_hline(yintercept = pull(allentown, melbands_spread))
################################
#Plots Perception
################################
p4 = ggplot(data = dat1) + 
    geom_boxplot((aes(x = artist, y = Perception))) + 
    theme_light() +
    geom_hline(yintercept = pull(allentown,Perception))
################################
#Plots chords_strength
################################
p5 = ggplot(data = dat1) + 
  geom_boxplot((aes(x = artist, y = chords_strength))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,chords_strength))
################################
#Plots erbbands_skewness
################################
p6 = ggplot(data = dat1) + 
  geom_boxplot((aes(x = artist, y = erbbands_skewness))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,erbbands_skewness))
#Plots average_loudness
################################
p7 = ggplot(data = dat1) + 
  geom_boxplot((aes(x = artist, y = average_loudness))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown, average_loudness))
#Plots danceability
################################
p8 = ggplot(data = dat1) + 
  geom_boxplot((aes(x = artist, y = danceability))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,danceability))
#Plots Cognition
################################
p9 = ggplot(data = dat1) + 
  geom_boxplot((aes(x = artist, y = Cognition))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,Cognition))
#Plots power
################################
p10 = ggplot(data = dat1) + 
  geom_boxplot((aes(x = artist, y = power))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,power))
# "spectral_skewness", "melbands_spread",
# "Perception", "chords_strength", "errbbands_skewness",
# "average_loudness", "danceability", "Cognition",
# "power", "discrepancy", "dissonance"



################################
# Prints the plots
################################
p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8  + p9 + p9 + p10 + p11




