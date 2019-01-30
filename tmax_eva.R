# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program prepares and analyzes temperature data from a given state in   #
# the climate divisions (NOAA nClimDiv) using the extreme value analysis      #
# software extRemes.                                                          #
# First, max data values are found for each year, then the data is analyzed   #
# using the function fevd.                                                    #
#                                                                             #
# This script should not be used for more than reference, as the statistical  #
# premise of the chosen functions is shakey at best                           #                                                                      
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(dplyr)
library(extRemes)
source("find_state.R")

prompt = "Enter state-codes for the appropriate locations (space-seperated) \n"
statecodes = unlist(strsplit(readline(prompt), "\\s+"))

prompt = "Enter 'y or 'Y' for individual divisional datasets (Y/n) \n"
answer = readline(prompt)

database = "st"
if (answer == "Y" | answer == "y") {
  database = "dv"
}

statenames = c()
for (val in statecodes) {
  name = find_state(val)
  statenames = append(statenames, name)
}

tmp = c()
if (database == "dv") {
  for (val in statecodes) {
    for (i in c(1:10)) {
      tmp = append(tmp, paste(val, i, sep=""))
    }
  }
}

statecodes = tmp

#change this dataset as needed
dataset = "tmax"
ftp_filepath = "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/"

download.file(paste(ftp_filepath, "procdate.txt", sep=""),
    destfile="procdate.txt", method="wget")
con = file("procdate.txt", "r")
procdate = readLines(con, n = 1)
close(con)

filenames = character()
for (val in statecodes) {
  filenames = append(filenames, paste("climdiv-prepped-", val, "-",
      dataset, database, "-v1.0.0-", procdate, ".csv", sep=""))
}

mean = c()
var = c()

i = 1
j = 0
new_data = list()


for (val in filenames) {

  if(file.exists(val)) {
    j = j + 1
    print(paste(val, statecodes[i]))
    trend_plot = paste("climdiv-", statecodes[i], dataset, "-trend.jpeg")
    # trend2_plot = paste("climdiv-", statecodes[i], dataset, "-trend2.jpeg")
    fevd_plot = paste("climdiv-", statecodes[i], dataset, ".jpeg")
    btrace_plot = paste("climdiv-", statecodes[i], dataset, "-btrace.jpeg")
    qq_plot = paste("climdiv-", statecodes[i], dataset, "-qq.jpeg")

    trend_file = paste("climdiv-", statecodes[i], dataset, "-trend.txt")
    # trend2_file = paste("climdiv-", statecodes[i], dataset, "-trend2.txt")
    ci_file = paste("climdiv-", statecodes[i], dataset, ".txt")

    data = read.csv(file=val, header=TRUE)
    data = subset(data, data[,6] != -99.90)

    ## The below uses a block maxima approach to determine extreme values ##
    yr = c(1978:2018)
    yr_ds = c()

    for (val in yr) {
      yr_data = subset(data, year == val)
      if(nrow(yr_data) !=0) {
        yr_ds= append(yr_ds, max(yr_data[,6]))
      } else {
        pint(paste("Warning: missing data for", val))
      }
    }

    new_data[[j]] = data.frame(year=yr, ds=yr_ds)
    names(new_data[[j]])[2] = dataset

    max_locs = order(new_data[[j]][,2], decreasing=TRUE)[1:2]

    max_data = new_data[[j]][max_locs, 2]

    jpeg(trend_plot)
    plot(new_data[[j]]$year, new_data[[j]][,2],
        col=ifelse(new_data[[j]][,2] == max_data[1] | new_data[[j]][,2] == max_data[2],
            "#B2182B", "black"),
        pch=ifelse(new_data[[j]][,2] == max_data[1] | new_data[[j]][,2] == max_data[2],
            19, 1))
    trend = lm(new_data[[j]][,2] ~ new_data[[j]]$year)
    trend_p = summary(trend)$coefficients[8]


    intercept = signif(coef(trend)[1], 5)
    slope = signif(coef(trend)[2], 5)
    eq = paste(dataset, "=", slope, "year +",intercept)
    abline(trend)
    mtext(eq, 3, line=-2)
    dev.off()

    mean = append(mean, signif(mean(new_data[[j]][,2]), 5))
    var = append(var, signif(var(new_data[[j]][,2]), 5))

    sink(trend_file, append=FALSE)
    print(paste("Mean:", mean[i], "   Variance:", var[i]))
    print(summary(trend))
    sink()

    fit0 = fevd(new_data[[j]][,2], new_data[[j]], type="Gumbel", units = "deg F")
    fit1 = fevd(new_data[[j]][,2], new_data[[j]], type="GEV", units = "deg F")

    test1 = lr.test(fit0, fit1)

    ci_shape = "NA"
    GEV_type = "Gumbel"

    if (test1$p.value < 0.05) {
      fit = fit1
      fitr = fit0
      ci_shape = ci(fit, type="parameter", which.par=3)
      if (ci_shape[2] < 0) {
        GEV_type = "Weibull"
      } else if (ci_shape[2] > 0) {
        GEV_type = "Frechet"
      }
    } else {
      fit = fit0
      fitr = fit1
    }

    if (GEV_type == "Gumbel") {
      print(GEV_type)
    }

    fit2 = fevd(new_data[[j]][,2], new_data[[j]], location.fun = ~ year, units = "deg F")
    fit3 = fevd(new_data[[j]][,2], new_data[[j]], scale.fun = ~ year, units = "deg F")
    fit4 = fevd(new_data[[j]][,2], new_data[[j]], location.fun = ~ year, scale.fun = ~ year,
        units = "deg F")

    flag = 0
    if (trend_p < 0.05) {
      test2 = lr.test(fit, fit2)
      test3 = lr.test(fit, fit3)
      test4 = lr.test(fit, fit4)
      test5 = lr.test(fit, fit5)
      test6 = lr.test(fit, fit6)
      test7 = lr.test(fit, fit7)

      if (test2$p.value < 0.05) {
          fitr2 = fit
          fita = fit2
          flag = 1
      } else if (test3$p.value < 0.05) {
          fitr2 = fit
          fita = fit3
          flag = 1
      } else if (test4$p.value < test3$p.value) {
          fitr2 = fit
          fita = fit4
          flag = 1
      } else if (test5$p.value < test4$p.value) {
          fitr2 = fit
          fita = fit5
          flag = 1
      } else if (test6$p.value < test5$p.value) {
          fitr2 = fit
          fita = fit6
          flag = 1
      } else if (test7$p.value < test6$p.value) {
          fitr2 = fit
          fita = fit7
          flag = 1
      }
    }

    if (flag == 0) {
      jpeg(qq_plot, width=700, height=350)
      par(mfcol=c(1,2))
      plot(fitr, type="qq")
      plot(fit, type="qq")
      dev.off()

      jpeg(fevd_plot)
      plot(fit)
      dev.off()

    } else {
      jpeg(qq_plot, width=1050, height=350)
      par(mfcol=c(1,3))
      plot(fitr, type="qq")
      plot(fitr2, type="qq")
      plot(fita, type="qq")
      dev.off()

      jpeg(fevd_plot)
      plot(fita)
      dev.off()

    }

    sink(ci_file, append=FALSE)
    print(GEV_type)
    print(ci_shape)
    print(fit)
    print(ci(fit, return.period = c(2, 20, 100)))
    sink()

  }
i = i + 1
}
if (database == "st") {
  print("Block Maxima Approach:")
  print(paste("Max mean:", max(mean), "at", statenames[which.max(mean)]))
  print(paste("Max var:", max(var), "at", statenames[which.max(var)]))
} else {
  print("Block Maxima Approach:")
  print(paste("Max mean:", max(mean), "at", statecodes[which.max(mean)]))
  print(paste("Max var:", max(var), "at", statecodes[which.max(var)]))
}
