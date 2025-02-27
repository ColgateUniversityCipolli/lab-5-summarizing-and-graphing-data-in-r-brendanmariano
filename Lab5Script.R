library(tidyverse)
library(stringr)
library(xtable)
library(patchwork)
#Step 1
allentown = read_csv(file = "data/essentia.data.allentown.csv")
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
#To store features for evaluation
view.features = tibble(the.frontb = character(), 
                       man.orch = character(), 
                       all.get.out = character(), 
                       feature = character())
#Stores my chosen features
features.keep = tibble(the.frontb = character(), 
                         man.orch = character(), 
                         all.get.out = character(), 
                         feature = character())
#Stores names of chosen features
#Found after analyzing box plots
desired.feat.names = c("dissonance","spectral_skewness", "melbands_spread", "danceability", 
                       "chords_strength", "erbbands_skewness", "average_loudness","valence",
                       "Perception", "Cognition", "power", "Authentic")
#Iterates through each feature                       
for(cols in colnames(data.tibb))
{
  #Analyzes quantitative columns
  if(class(data.tibb[[cols]]) == "numeric")
  {
    result = feat.funct(data.tibb, cols, allentown) 
      #Potential row to be added
      newRow = tibble(all.get.out = result$description[1], 
                       man.orch = result$description[2], 
                        the.frontb = result$description[3],
                        feature = cols)
      #Accepts features where Allentown has either one or two artists within range
    if(sum(str_count("Within Range", result$description)) == 2 
       | sum(str_count("Within Range", result$description)) == 1) 
    {
      view.features = view.features|>
        bind_rows(newRow)
        #Attempts to create table of desired features
        if(cols %in% desired.feat.names)
        { 
          features.keep = features.keep |>
            bind_rows(newRow)
        }
    }
  }
}
#Puts select features into csv file to be used for table
write_csv(features.keep, "featuresToKeep.csv")

#Step 4: Creating plots
#Loading Data
##################################
dat = read_csv("data/essentia.data.csv") |>
  mutate(artists = case_when(artist == "The Front Bottoms" ~ "TFB",
                             artist == "Manchester Orchestra" ~ "MO",
                             artist == "All Get Out" ~ "AGO"))

#Plot features below
#################################
# Plots Dissonance
#################################
p.diss = ggplot(data = dat) + 
  geom_boxplot(aes(x = artists, y = dissonance)) +
  theme_light() +
  geom_hline(yintercept = pull(allentown, dissonance))
#################################
# Plot of Spectral_Skewness
#################################
p.spec = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = spectral_skewness))) +
  theme_light() +
  geom_hline(yintercept = pull(allentown, spectral_skewness))
################################
# Plots melbands_spread
################################
p.mel = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = melbands_spread))) +
  theme_light() +
  geom_hline(yintercept = pull(allentown, melbands_spread))
################################
#Plots chords_strength
################################
p.chord = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = chords_strength))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,chords_strength))
################################
#Plots erbbands_skewness
################################
p.erbb = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = erbbands_skewness))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,erbbands_skewness))
#Plots average_loudness
################################
p.loud = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = average_loudness))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown, average_loudness))
#Plots danceability
################################
p.dance = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = danceability))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,danceability))
#Plots valence
################################
p.valence = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = valence))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,valence))

# Lyrics related graphs
################################
#Plots Perception
################################
p.lperc = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = Perception))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,Perception))
#Plots Cognition
################################
p.lcog = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = Cognition))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,Cognition))
#Plots power
################################
p.lpow = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = power))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,power))
#Plots authentic
################################
p.lauthentic = ggplot(data = dat) + 
  geom_boxplot((aes(x = artists, y = Authentic))) + 
  theme_light() +
  geom_hline(yintercept = pull(allentown,Authentic))




################################
# Prints the plots
################################
p.diss + p.spec + p.mel + p.dance
p.chord + p.erbb + p.loud + p.valence  
p.lperc + p.lcog +p.lpow + p.lauthentic




