# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program retrieves files from NCDC's (NOAA) Climate Divisional Dataset
# (ClimDiv) and prepares it in a way more accessable to the user. Specifically,
# it extracts only the state data to be used and transforms the monthly values
# into their own column so each row has a single "value" of interest. This only
# works for STATE data, however, could provide a framework for someone wanting
# to acess other data from the ClimDiv dataset as well. It would be helpful to
# check readme at ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/state-readme.txt
# for more info on the dataset you want to access before continuing.
#
# Input and preperations:
#     Dataset: e.g. tmax (max temp), sp03 (3-month SPI), etc. One value only.
#     State-codes: e.g 003 (AR), 004 005 (CA and CO), etc. Space seperated list.
# Script should be run from the location they wish the data to be dumped. No
# initial dataset is requried as it is acessed via FTP. Possible upgrade - allow
# user to enter state names as well. The last bit of this script has the state
# codes and their accompanying names as a start on this.
#
# Output: .csv files which follow a similar naming convention of all ClimDiv
# files, however, with "prepped" and state-codes between climdiv and datatype.
# Example: "climdiv-prepped-003-tmaxdv-vX.Y.Z-YYYYMMDD" for a AR tmax dataset.
#
# Author: April Walker
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
# This script will prompt the user to enter the dataset they wish to access and
# the states they wish to study. Only one dataset at a time, please, but
# multiple states accepted.
prompt = "Enter (single) dataset you wish to acquire (e.g tmax, sp03, cddc) \n"
dataset = readline(prompt)

prompt = "Enter state-codes for the appropriate locations (space-seperated) \n"
states = unlist(strsplit(readline(prompt), "\\s+"))

prompt = "Enter 'y or 'Y' for individual divisional datasets (Y/n) \n"
answer = readline(prompt)

if (states == "00" | states == "000") {
  states = 1:50
  states = as.character(states)
}

database = "st"
state_stop = 3
if (answer == "Y" | answer == "y") {
  database = "dv"
  state_stop = 2
}

ftp_filepath = "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/"

# download file which handles the YYYYMMDD naming convention format for files
download.file(paste(ftp_filepath, "procdate.txt", sep=""),
    destfile="procdate.txt", method="wget")
con = file("procdate.txt", "r")
procdate = readLines(con, n = 1)
close(con)

# MAIN METHOD

# declare and make some directories for easier data analysis
figures_dir = "main"
data_dir = "data"
extra_dir = "extra"
st_dir = "st"
dv_dir = "dv"

dir.create(figures_dir)
dir.create(data_dir)
dir.create(file.path(data_dir, st_dir))
dir.create(file.path(data_dir, dv_dir))
dir.create(extra_dir)


# concatenate the filename to access based on procdate and dataset
targetfile = paste("climdiv-", dataset, database, "-v1.0.0-", procdate, sep="")
datafile = paste(data_dir, "/", targetfile, sep="")
# download the file containing your dataset, saved under same name
if(!file.exists(datafile)) {
  download.file(paste(ftp_filepath, targetfile, sep=""),
      destfile=datafile, method="wget")
}

data = read.table(datafile, colClasses = "character")
# access the 4 components in the first column of our data so we can search based
# on state we wish to access
data = transform(data,
          state = substr(data[,1], 1, state_stop),
          div = substr(data[,1], state_stop + 1, 4),
          elem = substr(data[,1], 5, 6),
          year = substr(data[,1], 7, 10))
# we don't need elem or V1, as elem is given by our dataset and V1's was extracted
data = data[,c(14,15,17,2:13)]
# rename columns containing monthly values
data = rename(data, '1'  = V2,
                    '2'  = V3,
                    '3'  = V4,
                    '4'  = V5,
                    '5'  = V6,
                    '6'  = V7,
                    '7'  = V8,
                    '8'  = V9,
                    '9'  = V10,
                    '10' = V11,
                    '11' = V12,
                    '12' = V13)
# rearange data such that "month" is a column and the first in decending order
data2 = data %>%
  gather(month, value, '1':'12', na.rm=FALSE)
data2 = data2 %>%
  mutate(month = parse_number(month)) %>%
  select(state, div, year, month, value) %>%
  arrange(state, div, year, month)
# rename values as dataset's value
names(data2)[5] = dataset
data2$div = as.numeric(data2$div)
# your data should now look something like...
# head(data2)
#     state div year month  tmax
# 1   001   0 1895     1 52.70
# 2   001   0 1895     2 48.10

data = data.frame()
if (database == "st") {
  for (val in tmp_states) {
    data = subset(data2, state == str_pad(val, 2, "left", "0"))
    # develop output filename for each state
    outputfile = paste(data_dir, "/", st_dir, "/", "climdiv-prepped-", val, "-",
        dataset, database, "-v1.0.0-", procdate, ".csv")
    write.csv(data, file = outputfile, sep=",")
  }
} else {
  for (val in states) {
    for (i in c(1:10)) {
      data = subset(data2, state == str_pad(val, 2, "left", "0"))
      data = subset(data, div ==i)
      # develope output filename for each state
      if(nrow(data) != 0) {
        division = str_pad(i, 2, pad="0", "left")
        stdv_code = paste(val, division, sep="")
        outputfile = paste(data_dir, "/", dv_dir, "/", "climdiv-prepped-",
        stdv_code, "-", dataset, database, "-v1.0.0-", procdate, ".csv", sep="")
        write.csv(data, file = outputfile, sep=",")
      }
    }
  }
}
