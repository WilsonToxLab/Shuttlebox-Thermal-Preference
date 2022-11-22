library(tidyverse)
library(lubridate)
library(cumstats)

#the type of .txt that is produced by Shuttlesoft doesn't work well with R

col_names <- c("Time",
               "Zone",
               "Tobj",
               "Tpref",
               "INCR",
               "DECR",
               "X pos",
               "Y pos",
               "Velocity",
               "Distance",
               "Time INCR",
               "Time DECR",
               "Delta T",
               "Hysteresis",
               "HI Set Point",
               "HI Hysteresis",
               "LO Set Point", 
               "LO Hysteresis", 
               "K", 
               "Max", 
               "Min", 
               "Max Rate",
               "Avoidance Upper Mean",
               "NA1",
               "NA2",
               "NA3")


## Read in ALL .txt files, convert to .csv ##

all_data <- list.files(pattern="*.txt")

for (file in all_data){
  the_data <- read.delim(file, header = FALSE, sep = "\t", quote = "\"",
                         dec = ".", fill = TRUE, comment.char = "", skip=6, col.names=col_names)
  write.csv(the_data, file = paste0(tools::file_path_sans_ext(basename(file)), "_N.csv"),
            na = "NA",
            sep = ",")
}

#Should print a .csv file for every .txt file adding _N to the name - makes the next part easier

#######################
## Cumulative Median ##   
#######################

#I use the file names to create a list of .csv files - so you need an empty folder without other .csv files for this to work

filelist <- list.files(pattern="_N.csv")

df.list = lapply(filelist, function(f) {
  
  file.in = read.csv(f, header=TRUE, stringsAsFactors=FALSE)
  
  data.frame(tail(cummedian(file.in$Tobj),1))
  
  
}
)

result_Tobj = do.call(rbind, df.list[sapply(df.list, is.data.frame)])

write.csv(result_Tobj, file="Tpref_EXP_1.csv")

#Should write a .csv file with fish # and the cumulative median of the T_obj - which is Temperature preference
#I checked that the data from this calculation matches with the data I got from Shuttlesoft (it automatically calculates Tpref)