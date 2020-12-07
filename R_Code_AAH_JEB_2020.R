## In order to obtain temperature preference from the dynamic mode ONLY you must have separate data files for acclimation and experimentation phases

library(tidyverse)
library(lubridate)
library(cumstats)

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
# This method requires the working directory to be completely empty except for the shuttlesoft data files (.txt)

all_data <- list.files(pattern="*.txt")

for (file in all_data){
  the_data <- read.delim(file, header = FALSE, sep = "\t", quote = "\"",
                         dec = ".", fill = TRUE, comment.char = "", skip=6, col.names=col_names)
  write.csv(the_data, file = paste0(tools::file_path_sans_ext(basename(file)), "_N.csv"),
            na = "NA",
            sep = ",")
}

# Converting all .txt files to .csv files so they are easier to work with in R
# Adding _N to the end of the file name so I can later distinguish them from the .txt files

#######################
## Cumulative Median ##   
#######################

# Taking the cumulative mean of occupied temperature - and pulling the last value as Tpref

filelist <- list.files(pattern="_N.csv")

df.list = lapply(filelist, function(f) {
  
  file.in = read.csv(f, header=TRUE, stringsAsFactors=FALSE)
  
  data.frame(tail(cummedian(file.in$Tobj),1))
  
  
}
)

result_Tobj = do.call(rbind, df.list[sapply(df.list, is.data.frame)])

write.csv(result_Tobj, file="Tpref_EXP_1.csv")

## combining all values of Tpref into a single vector - order of output depends on the order of .csv files within the filelist (based on file name)