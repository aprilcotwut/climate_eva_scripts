# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program developes a temperature duraction frequency curve for temperature
# data form the Climate Division in Cali and in Texas.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(tidyr)
library(dplyr)
library(extRemes)
library(zoo)

houston_max = list()
sacramento_max = list()

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

# These are the durations for which moving mean windows will be cacluated and
# analyzed using the block maximum GEV method
durations = c(1,2,3,4,5,6,7,10)
start_day = "2999-06-01"
end_day = "2999-08-31"
start_year = 1958
end_year = 2018
data_years = end_year - start_year
dataset = "TMAX"

extra_dir = "extra"
tdf_dir = "tdf"
figures_dir = "main"
dir.create(file.path(extra_dir, tdf_dir))
dir.create(file.path(figures_dir, tdf_dir))

# This fixes are data a bit
print("Reading data")
data = read.csv(file="data/1560085.csv", header=TRUE)
data$DATE = as.POSIXct(data$DATE)
houston = subset(data, STATION == "USW00012906" & format(as.Date(DATE,format =
    "%m-%d-%Y"),"%Y") < 1970)
altdata = read.csv(file="data/1560264.csv", header=TRUE)
houston = rbind(houston, altdata)
sacramento = subset(data, STATION == "USW00023271")
print("Filling holes in data")
houston = houston %>%
    mutate(DATE = as.Date(DATE)) %>%
    complete(DATE = seq.Date(min(DATE), max(DATE), by="day")) %>%
    fill(TMAX)
houston = as.data.frame(houston)
sacramento = sacramento %>%
    mutate(DATE = as.Date(DATE)) %>%
    complete(DATE = seq.Date(min(DATE), max(DATE), by="day")) %>%
    fill(TMAX)
sacramento = as.data.frame(sacramento)
data = rbind(houston, sacramento)

print("Beginning rolling mean and annual maxima process...")
i = 1
for (dur in durations) {
  if (dur%%2 == 1) {
    # If the duration is odd, we will add an even amount of days from before and
    # after the given window
    adj = (dur-1) /2
    dur_start = format((as.Date(start_day) - adj), format="%m-%d")
    dur_end = format((as.Date(end_day) + adj), format="%m-%d")
    print(paste("Analyzing", dur, "day duration means and annual maxima", sep=" "))
  } else if (dur%%2 == 0) {
    # If the duration is even, we will add half the durations days before and
    # half - 1 after
    adj = dur/2
    dur_start = format((as.Date(start_day) - adj + 1), format="%m-%d")
    dur_end = format((as.Date(end_day) + adj), format="%m-%d")
    print(paste("Analyzing", dur, "day duration means and annual maxima", sep=" "))
  }
  # Save the data from within this window to develop the rolling means
  tmp = subset(data, format(as.Date(DATE,format = "%m-%d-%Y"),"%m-%d") >=
      dur_start & format(as.Date(DATE,format = "%m-%d-%Y"),"%m-%d") <= dur_end)
  for (yr in 1958:2018) {
    tmp2 = subset(tmp, format(as.Date(DATE,format = "%m-%d-%Y"),"%Y") == yr)
    tmph = subset(tmp2, STATION == "USW00012906" | STATION == "USW00012918")
    tmps = subset(tmp2, STATION == "USW00023271")
    dates = seq(as.Date(paste(yr, dur_start, sep="-")), as.Date(paste(yr, dur_end, sep="-")), by = "day")
    true_dates = seq(as.Date(paste(yr, format(as.Date(start_day), format="%m-%d"), sep="-")),
        as.Date(paste(yr, format(as.Date(end_day), format="%m-%d"), sep="-")), by = "day")
    tmph = tmph %>%
        mutate(DATE = as.Date(DATE)) %>%
        complete(DATE = dates) %>%
        fill(TMAX)
    tmps = tmps %>%
        mutate(DATE = as.Date(DATE)) %>%
        complete(DATE = dates) %>%
        fill(TMAX)
    tmph = rollmean(tmph$TMAX, dur, align = "center")
    tmps = rollmean(tmps$TMAX, dur, align = "center")
    houston_max[[i]] = tryCatch(rbind(houston_max[[i]], cbind.data.frame(yr,
        max(tmph))), error = function(e) print("try-error"))
    if (class(houston_max[[i]]) == "character") {
      print(yr)
      houston_max[[i]] = cbind.data.frame(yr, max(tmph))
      sacramento_max[[i]] = cbind.data.frame(yr, max(tmps))
    } else {
      sacramento_max[[i]] = rbind(sacramento_max[[i]], cbind.data.frame(yr, max(tmps)))
    }
  }
  i = i + 1
}
#Editting new matrices
for (i in c(1:length(durations))) {
  names(houston_max[[i]]) = c("Year", "TMAX")
  names(sacramento_max[[i]]) = c("Year", "TMAX")
  houston_max[[i]]$TMAX = as.double(houston_max[[i]]$TMAX)
  sacramento_max[[i]]$TMAX = as.double(sacramento_max[[i]]$TMAX)
}

# print("Process finished! Error checking before continuing...")
# for (i in c(1:(length(durations)-1))) {
#   for (yr in 1958:2018) {
#     tmph = subset(houston_max[[i]], Year == yr)$TMAX
#     tmph2 = subset(houston_max[[i+1]], Year == yr)$TMAX
#     tmps = subset(sacramento_max[[i]], Year == yr)$TMAX
#     tmps2 = subset(sacramento_max[[i+1]], Year == yr)$TMAX
#     if (tmph + 0.2 < tmph2) {
#       if (i < length(durations) - 1) {
#         tmph3 = subset(houston_max[[i+2]], Year == yr)$TMAX
#         if (tmph < tmph3) {
#           print(paste("Error occured in houston process at duration", durations[i],
#               "and year", yr, sep=" "))
#           print(paste(tmph, tmph3, sep=" "))
#         } else {
#           print(paste(tmph, tmph2, sep=" "))
#         }
#       }
#       print(paste(tmph, tmph2, sep=" "))
#     } else if (tmps + 0.2 < tmps2) {
#       if (i < length(durations) - 1) {
#         tmps3 = subset(sacramento_max[[i+2]], Year == yr)$TMAX
#         if (tmps < tmps3) {
#           print(paste("Error occured in sacramento process at duration", durations[i],
#               "and year", yr, sep=" "))
#               print(paste(tmps, tmps3, sep=" "))
#         } else {
#           print(paste(tmps, tmps2, sep=" "))
# } } } } }

print("Begin initial trend tests")
for (i in c(1:length(durations))) {
  mk_test_h = cor.test(houston_max[[i]]$Year, houston_max[[i]]$TMAX, method="kendall")
  tau_h = as.double(mk_test_h$estimate)
  p_h = mk_test_h$p.value

  mk_test_s = cor.test(sacramento_max[[i]]$Year, sacramento_max[[i]]$TMAX, method="kendall")
  tau_s = as.double(mk_test_s$estimate)
  p_s = mk_test_s$p.value

  #Trend plots
  #First houston plot
  trend_plot = paste(extra_dir, "/", tdf_dir, "/houston-", durations[i], "day",
      "-trend.jpeg", sep="")
  max_locs = order(houston_max[[i]]$TMAX, decreasing=TRUE)[1:2]
  max_data = houston_max[[i]][max_locs, 2]
  jpeg(trend_plot)
  plot(houston_max[[i]]$Year, houston_max[[i]]$TMAX,
      col=ifelse(houston_max[[i]]$TMAX == max_data[1] | houston_max[[i]]$TMAX == max_data[2],
          "red", "black"),
      pch=ifelse(houston_max[[i]]$TMAX == max_data[1] | houston_max[[i]]$TMAX == max_data[2],
          19, 1))
  trend = lm(houston_max[[i]]$TMAX ~ houston_max[[i]]$Year)
  intercept = signif(coef(trend)[1], 5)
  slope = signif(coef(trend)[2], 5)
  eq = paste(dataset, "=", slope, "year +",intercept)
  abline(trend)
  mtext(eq, 3, line=-2)

  trend = lm(houston_max[[i]]$TMAX ~ poly(houston_max[[i]]$Year, 2, raw = TRUE))
  predicted = predict(trend, data.frame(x=houston_max[[i]]$Year), interval='confidence', level=0.95)
  lines(houston_max[[i]]$Year, predicted[,1], col="blue")

  intercept_p = signif(coef(trend)[1], 5)
  coef1_p = signif(coef(trend)[2], 5)
  coef2_p = signif(coef(trend)[3], 5)
  eq = paste(dataset, "=", coef2_p, "year^2 +", coef1_p, "year +", intercept_p)
  mtext(eq, 3, line=-3)
  dev.off()
  #Then sacramento plot
  trend_plot = paste(extra_dir, "/", tdf_dir, "/sacramento-", durations[i], "day",
      "-trend.jpeg", sep="")
  max_locs = order(sacramento_max[[i]]$TMAX, decreasing=TRUE)[1:2]
  max_data = sacramento_max[[i]][max_locs, 2]
  jpeg(trend_plot)
  plot(sacramento_max[[i]]$Year, sacramento_max[[i]]$TMAX,
      col=ifelse(sacramento_max[[i]]$TMAX == max_data[1] | sacramento_max[[i]]$TMAX == max_data[2],
          "red", "black"),
      pch=ifelse(sacramento_max[[i]]$TMAX == max_data[1] | sacramento_max[[i]]$TMAX == max_data[2],
          19, 1))
  trend = lm(sacramento_max[[i]]$TMAX ~ sacramento_max[[i]]$Year)
  intercept = signif(coef(trend)[1], 5)
  slope = signif(coef(trend)[2], 5)
  eq = paste(dataset, "=", slope, "year +",intercept)
  abline(trend)
  mtext(eq, 3, line=-2)

  trend = lm(sacramento_max[[i]]$TMAX ~ poly(sacramento_max[[i]]$Year, 2, raw = TRUE))
  predicted = predict(trend, data.frame(x=sacramento_max[[i]]$Year), interval='confidence', level=0.95)
  lines(sacramento_max[[i]]$Year, predicted[,1], col="blue")

  intercept_p = signif(coef(trend)[1], 5)
  coef1_p = signif(coef(trend)[2], 5)
  coef2_p = signif(coef(trend)[3], 5)
  eq = paste(dataset, "=", coef2_p, "year^2 +", coef1_p, "year +", intercept_p)
  mtext(eq, 3, line=-3)
  dev.off()
}
print("End trend tests, begin GEV fitting")

dfs = list()
fits = list()
bf = list()

return_lvls = list()
ci_vals = list()

dfs[[1]] = houston_max
dfs[[2]] = sacramento_max
df_names = c("houston", "sacramento")
i = 1
for (val in dfs) {
  bf[[i]] = list()
  fits[[i]] = list()
  for(j in 1:length(durations)) {
    print("Applying Mann Kendall Test")
    mk_test = cor.test(val[[j]]$Year, val[[j]]$TMAX, method="kendall")
    tau = as.double(mk_test_h$estimate)
    p = mk_test_h$p.value

    fits[[i]][[j]] = list()
    print("Developing stationary fits")
    fits[[i]][[j]][[1]] = fevd(val[[j]]$TMAX, val[[j]], type="Gumbel", units = "deg F")
    fits[[i]][[j]][[2]] = fevd(val[[j]]$TMAX, val[[j]], type="GEV", units = "deg F")
    test = lr.test(fits[[i]][[j]][[1]], fits[[i]][[j]][[2]])

    ci_shape = "NA"
    GEV_type = "Gumbel"
    type_a = "Gumbel"
    type_r = "GEV"

    type = "GEV"

    if (test$p.value < 0.05) {
      fit_a = fits[[i]][[j]][[2]]
      fit_r = fits[[i]][[j]][[1]]
      type_a = "GEV"
      type_r = "Gumbel"
      ci_shape = ci(fit_a, type="parameter", which.par=3)
      if (ci_shape[2] < 0) {
        GEV_type = "Weibull"
      } else if (ci_shape[2] > 0) {
        GEV_type = "Frechet"
      }
    } else {
      fit_a = fits[[i]][[j]][[1]]
      fit_r = fits[[i]][[j]][[2]]
    }
    # The following are the possible nonstationary fits
    if(p < 0.05) {
      ("Developing nonstationary fits")
      # fits with location ~ year
      fits[[i]][[j]][[3]] = fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j]][[4]] = fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), units = "deg F")
      # fits with scale ~ year
      fits[[i]][[j]][[5]] = fevd(val[[j]]$TMAX, val[[j]], type = type, scale.fun =
          ~ poly(((Year - start_year)/ data_years), 1), units = "deg F")
      fits[[i]][[j]][[6]] = fevd(val[[j]]$TMAX, val[[j]], type = type, scale.fun =
          ~ poly(Year, 2), units = "deg F")
      # fits with location ~ year and scale ~ year
      fits[[i]][[j]][[7]] = fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), scale.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j]][[8]] = fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), scale.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[i]][[j]][[9]] = fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), scale.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), units = "deg F")
      fits[[i]][[j]][[10]] = fevd(val[[j]]$TMAX, val[[j]], type = type, location.fun =
          ~ poly(((Year - start_year)/ data_years), 1, raw = TRUE), scale.fun =
          ~ poly(((Year - start_year)/ data_years), 2, raw = TRUE), units = "deg F")
    # Determine the best fit

      print("Determining best fit")
      crit = c()
      tmp = c()
      tmp2 = c()
      for (it in 3:(length(fits[[i]][[j]]))) {
        print("Attempting AIC use for best fit...")
        tmp = append(tmp, fits[[i]][[j]][[it]]$result$value)
        try({tmp2 = append(tmp2, as.double(summary(fits[[i]][[j]][[it]])$AIC))})
      }
      if (length(tmp2) != 0) {
        print("Success!")
        best_fit = fits[[i]][[j]][[which.min(tmp2) + 2]]
        worst_fit = fits[[i]][[j]][[which.max(tmp2) + 2]]
        crit = tmp2

      } else {
        print("Failure! Suing MLE as alternative")
        best_fit = fits[[i]][[j]][[which.min(tmp) + 2]]
        worst_fit = fits[[i]][[j]][[which.max(tmp) + 2]]
        crit = tmp
      }
      bf[[i]][[j]] = best_fit
    } else {
      best_fit = fit_a
      worst_fit = fit_r
      bf[[i]][[j]] = best_fit
    }
  }
  i = i + 1
}

periods = c(2, 20, 100)
lin_seq = (((start_year:2018) - start_year)/ data_years)
poly_seq = ((((start_year:2018) - start_year)/ data_years)^2)
threshold = seq()
lin_par = c("mu1", "sigma1")
poly_par = c("mu2", "sigma2")
print("Finding return levels and confidence interval...")
for (i in c(1:length(dfs))) {
  return_lvls[[i]] = list()
  ci_vals[[i]] = list()
  for (j in c(1:length(durations))) {
    rl_plot = paste(extra_dir, "/", tdf_dir, "/rlplot-", dataset, "-", durations[j], "-",
        df_names[i], ".jpeg", sep="")
    par = names(bf[[i]][[j]]$results$par)
    ns_par = intersect(lin_par, par)
    vals = list()
    for (val in ns_par) {
      assign(val, lin_seq)
      vals[[val]] = get(val)
    }
    ns_par = intersect(poly_par, par)
    for (val in ns_par) {
      assign(val, poly_seq)
      vals[[val]] = get(val)
    }
    if (is.null(bf[[i]][[j]]$num.pars$shape)) {
        bf[[i]][[j]]$num.pars$shape = 0
    }
    v = make.qcov(bf[[i]][[j]], vals = vals)
    # v[,"threshold"]
    return_lvls[[i]][[j]] = return.level(bf[[i]][[j]], return.period = periods)
    ci_vals[[i]][[j]] = list()
    for (k in c(1:length(periods))) {
      ci_vals[[i]][[j]][[k]] = ci(bf[[i]][[j]], alpha = 0.05, return.period = periods[k], qcov = v)
    }
    try({
    jpeg(rl_plot, width=500, height=500)
    plot(bf[[i]][[j]], type="rl", rperiods = periods)
    dev.off()
    })
  }
}

for (i in c(1:length(dfs))) {
  for (j in c(1:length(durations))) {
    for (k in c(1:length(periods))) {
      x = start_year:end_year
      y = return_lvls[[i]][[j]][,k]
      ci_l = ci_vals[[i]][[j]][[k]][,1]
      ci_u = ci_vals[[i]][[j]][[k]][,3]
      tdf_plot = paste(figures_dir, "/", tdf_dir, "/tdf-", durations[j], "-", dataset,
          "-", df_names[i], "-", periods[k], "yr.jpeg", sep="")
      jpeg(tdf_plot, width=500, height=750)
      plot(x, y, ylim=range(c(ci_l, ci_u)), ylab="Temperature", xlab="Year",
          main = paste(periods[k], "-Year Return Levels for ", durations[j],
          " Day ", dataset, " Events in ", simpleCap(df_names[i]), sep=""), type = "o")
      arrows(x, ci_l, x, ci_u, length=0.05, angle=90, code=3)
      dev.off()
    }
  }
}

# Dooby doo doo
