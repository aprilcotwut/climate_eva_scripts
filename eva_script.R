# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program prepares and analyzes temperature data from Climate Divisions  #
# for all the United States using the extreme value analysis software         #
# extRemes                                                                    #
#                                                                             #
# On it's own this script is archived, various components need to be remade   #
# into various working functions for future EVA                               #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(dplyr)
library(extRemes)
library(stringr)
library(sf)

source("find_state.R")

prompt = "Enter state-codes for the appropriate locations (space-seperated) \n"
statecodes = unlist(strsplit(readline(prompt), "\\s+"))

prompt = "Enter (single) dataset you wish to acquire (e.g tmax, sp03, cddc) \n"
dataset = readline(prompt)

prompt = "Enter start year for data \n"
start_year = as.integer(readline(prompt))
data_years = 2018 - start_year

figures_dir = "main"
data_dir = "data"
extra_dir = "extra"

database = "dv"


if (statecodes == "00") {
  statecodes = 01:50
  statecodes = as.character(statecodes)
} else {
  statenames = c()
  for (val in statecodes) {
    name = find_state(val)
    statenames = append(statenames, name)
  }
}
tmp = c()
if (database == "dv") {
  for (val in statecodes) {
    for (i in c(1:10)) {
      division = str_pad(i, 2, pad="0", "left")
      tmp = append(tmp, paste(val, division, sep=""))

    }
  }
  statecodes = tmp
}

states = c(1:48)

ftp_filepath = "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/"
if(!file.exists("procdate.txt")) {
  download.file(paste(ftp_filepath, "procdate.txt", sep=""),
      destfile="procdate.txt", method="wget")
  con = file("procdate.txt", "r")
  procdate = readLines(con, n = 1)
  close(con)
} else {
  con = file("procdate.txt", "r")
  procdate = readLines(con, n = 1)
  close(con)
}

filenames = character()

for (val in statecodes) {
  filenames = append(filenames, paste(data_dir, "/", database, "/",
  "climdiv-prepped-", val, "-", dataset, database, "-v1.0.0-", procdate, ".csv",
  sep=""))
}

#This is the color scale for the heatmap
scale = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
    "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")

scale2 = c("#67001F", "#900D26", "#B2182B", "#C43C3C", "#D6604D", "#E6866A",
"#F4A582", "#F8C0A4", "#FDDBC7", "#FAE9DF", "#F7F7F7", "#E6EFF4", "#D1E5F0",
"#B2D5E7", "#92C5DE", "#6AACD0", "#4393C3", "#327CB8", "#2166AC", "#134b86",
"#053061")

tau_scale = c(0.55, 0.45, 0.35, 0.25, 0.15, 0.05, -0.05, -0.15, -0.25, -0.35,
    -0.45, -0.55)
btwn_tau = c(seq(0.50, -0.50, length.out = 11))

if(dataset == "tmin") {
    rl_scale = c(seq(92.5, 40, length.out = 22))
} else {
    rl_scale = c(seq(112.5, 60, length.out = 22))
}

rl_dif_scale = c((11:1)/2, (-1:-11)/2)
lin_seq = (((start_year:2018) - start_year)/ data_years)
poly_seq = ((((start_year:2018) - start_year)/ data_years)^2)

mean = c()
var = c()

new_data = list()
all_data = list()
data = list()

tau = c()
tau_all = list()

p = c()
slope = c()
intercept = c()
coef1_p = c()
coef2_p = c()
intercept_p = c()

cor_color = c()


tmp = c()
tmp2 = c()
i = 1
for (val in filenames) {
  if(file.exists(val)) {
    tmp = append(tmp, statecodes[i])
    tmp2 = append(tmp2, val)
  }
  i = i + 1
}
statecodes = tmp
filenames = tmp2
warning_pos = c()
warning2_pos = c()
warning3_pos = c()
warning4_pos = c()

change_vals = list()

j = 1
for (val in filenames) {
  data[[j]] = c()

  #Read data
  data[[j]] = read.csv(file=val, header=TRUE)
  data[[j]] = subset(data[[j]], data[[j]][,6] != -99.90)
  j = j + 1
}
data_l = j-1

## The below uses a block maxima approach to determine extreme values ##
start_trend = c(1918, 1928, 1938, 1948, 1958, 1968, 1978, 1988, 1998, 2008)

print("Begin block maxima approach on all raw data")
for (j in 1:data_l) {
  yr_ds = c()
  tmp = c()
  yr = c(1918:2018)
  for (val in yr) {
    yr_data = subset(data[[j]], year == val)
    # only save years where data exists
    if (nrow(yr_data) !=0) {
      yr_ds = append(yr_ds, max(yr_data[,6]))
      tmp = append(tmp, val)
    }
  }
  all_data[[j]] = data.frame(year=tmp, ds=yr_ds)
  names(all_data[[j]])[2] = dataset
}
sub_data = list()
print("Finished block maxima approach on all raw data")
for (val in 1:length(start_trend)) {
  print(paste("Analyzing data from", start_trend[val]))
  tau_all[[val]] = c()
  tmp = c()
  tmp2 = c()
  yr = c(start_trend[val]:2018)
  for (j in 1:data_l) {
    sub_data[[j]] = subset(all_data[[j]], year >= start_trend[val])
    mk_test = cor.test(sub_data[[j]]$year, sub_data[[j]][, 2], method="kendall")
    tmp2 = append(tmp2, as.double(mk_test$estimate))
  }
  tau_all[[val]] = tmp2
}
mean_tau = c()
for (i in 1:length(start_trend)) {
  mean_tau[i] = mean(tau_all[[i]])
}


print("Beginning analyzing subset of data since specified start date")
for (j in 1:data_l) {
  #NOTE: use new_data as main dataframe from this point onwards
  new_data[[j]] = subset(all_data[[j]], year >= start_year)
}

print("Analyzing trends for each climate division")
for (j in 1:data_l) {
  #Filenames declared here
  trend_plot = paste(extra_dir, "/climdiv-", statecodes[j], dataset,
      "-trend.jpeg", sep="")
  trend_file = paste(extra_dir, "/climdiv-", statecodes[j], dataset,
      "-trend.txt", sep="")


  max_locs = order(new_data[[j]][,2], decreasing=TRUE)[1:2]
  max_data = new_data[[j]][max_locs, 2]
  #Trend plot
  jpeg(trend_plot)
  plot(new_data[[j]]$year, new_data[[j]][,2],
      col=ifelse(new_data[[j]][,2] == max_data[1] | new_data[[j]][,2] == max_data[2],
          scale[2], "black"),
      pch=ifelse(new_data[[j]][,2] == max_data[1] | new_data[[j]][,2] == max_data[2],
          19, 1))
  trend = lm(new_data[[j]][,2] ~ new_data[[j]]$year)
  intercept = append(intercept, signif(coef(trend)[1], 5))
  slope = append(slope, signif(coef(trend)[2], 5))
  eq = paste(dataset, "=", slope[j], "year +",intercept[j])
  abline(trend)
  mtext(eq, 3, line=-2)

  trend = lm(new_data[[j]][,2] ~ poly(new_data[[j]]$year, 2, raw = TRUE))
  predicted = predict(trend, data.frame(x=new_data[[j]]$year), interval='confidence', level=0.95)
  lines(new_data[[j]]$year, predicted[,1], col=scale[10])

  intercept_p = append(intercept_p, signif(coef(trend)[1], 5))
  coef1_p = append(coef1_p, signif(coef(trend)[2], 5))
  coef2_p = append(coef2_p, signif(coef(trend)[3], 5))
  eq = paste(dataset, "=", coef2_p[j], "year^2 +", coef1_p[j], "year +", intercept_p[j])
  mtext(eq, 3, line=-3)

  dev.off()
  #End trend plot

  # Run a Mann Kendall test to determine correlation between time and temp
  mk_test = cor.test(new_data[[j]]$year, new_data[[j]][, 2], method="kendall")
  tau = append(tau, as.double(mk_test$estimate))
  p = append(p, mk_test$p.value)


  #This determines the color for our "correlation" heat map depending on the
  #tau (correlation) value
  if (tau[j] > 0.45) {
    cor_color = append(cor_color, scale[1])
  } else if ((tau[j] <= 0.45) & (tau[j] > 0.35)) {
    cor_color = append(cor_color, scale[2])
  } else if ((tau[j] <= 0.35) & (tau[j] > 0.25)) {
    cor_color =  append(cor_color, scale[3])
  } else if ((tau[j] <= 0.25) & (tau[j] > 0.15)) {
    cor_color = append(cor_color, scale[4])
  } else if ((tau[j] <= 0.15) & (tau[j] > 0.05)) {
    cor_color = append(cor_color, scale[5])
  } else if ((tau[j] <= 0.05) & (tau[j] > -0.05)) {
    cor_color = append(cor_color, scale[6])
  } else if ((tau[j] <= -0.05) & (tau[j] > -0.15)) {
    cor_color =  append(cor_color, scale[7])
  } else if ((tau[j] <= -0.15) & (tau[j] > -0.25)) {
    cor_color =  append(cor_color, scale[8])
  } else if ((tau[j] <= -0.25) & (tau[j] > -0.35)) {
    cor_color =  append(cor_color, scale[9])
  } else if ((tau[j] <= -0.35) & (tau[j] > -0.45)) {
    cor_color =  append(cor_color, scale[10])
  } else if (tau[j] < -0.45) {
    cor_color =  append(cor_color, scale[11])
  }

  # Just some mean and variance data for my info
  mean = append(mean, signif(mean(new_data[[j]][,2]), 5))
  var = append(var, signif(var(new_data[[j]][,2]), 5))

  #Trend file
  sink(trend_file, append=FALSE)
  print(paste("Mean:", mean[j], "   Variance:", var[j]))
  print(summary(trend))
  print(mk_test)
  sink()
}

## Heatmap correlation plot stuff ##
prompt = "Would you like to develop the correaltion heatmap? Enter 'y or 'Y'.\n"
answer = readline(prompt)
#Open the ClimDiv shapefile
if (answer == "Y" | answer == "y") {
  nclimdiv_shp_file = "SHP_Files/GIS.OFFICIAL_CLIM_DIVISIONS.shp"
  nclimdiv = st_read(nclimdiv_shp_file)
  #Filename for correlation heatmap
  corr_start = paste("/climdiv-", dataset, "-corrmap-", data_years, "yr", sep="")
  corr_plot = paste(figures_dir, corr_start,".jpeg", sep="")
  fl_corr_plot = paste(figures_dir, corr_start, "-fl.jpeg", sep="")

  #Opens correlation heatmap and plots empty nclimdiv map

  jpeg(corr_plot, width=1100, height=700)
  m = rbind(c(1,1,1), c(3,2,4))
  layout(m, heights = c(9, 1), widths = c(2,4,2))
  par(mar = c(1,1,1,2), oma = c(1,0,1,0), cex.main=1, cex=1)
  plot(st_geometry(nclimdiv), lwd = 2, border = "white")
  title(main = paste("Correlation between Annual Maximum", toupper(dataset),
      "and Year Since", start_year, sep=" "))
  legend("topright", legend = "p-value < 0.05 (Significant Trend)", pch=17)
  #Just in case chars cause issues in below loop
  statecodes = as.integer(statecodes)

  for(val in c(1:344)) {
    if (nclimdiv$CLIMDIV[order(nclimdiv$CLIMDIV)[val]] == statecodes[val]) {
      plot(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]], col =
          cor_color[val], border = "white", lwd = 2, add = TRUE)
      if (p[val] <= 0.05) {
        if (tau[val] < 0) {
          print(nclimdiv$CLIMDIV[order(nclimdiv$CLIMDIV)[val]])
          print(tau[val])
          print(p[val])
          print(cor_color[val])
        }
        plot(st_centroid(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]]),
            pch = 17, add = TRUE)
      }
    }
  }

  corr_spread = c()
  scale_tmp = c()
  corr_names = c()
  for (i in 1:length(scale)) {
    tmp = subset(cor_color, cor_color == scale[i])
    # corr_spread = append(corr_spread, length(tmp))
    if (is.null(nrow(tmp))) {
      corr_spread = append(corr_spread, length(tmp))
      scale_tmp = append(scale_tmp, scale[i])
      corr_names = append(corr_names, btwn_tau[i])

    }
  }
  barplot(corr_spread, ylim = c(0, max(corr_spread)), col = scale_tmp,
      names = corr_names, border = NA, axes = FALSE)
  # plot(start_trend, mean_tau, type = "l")
  # .image_scale(tau_scale, col = scale[11:1], key.length = lcm(18), key.pos = 4,
  #     at = tau_scale)
  dev.off()

  #Florida specific Heatmap
  jpeg(fl_corr_plot, width=500, height=700)
  layout(matrix(1:2, nrow = 2), heights = c(7, 1))
  par(mar = c(1,1,3,2), oma = c(1,0,1,0))
  plot(st_geometry(nclimdiv[2])[c(47:49,58:61)], border = "white", lwd = 2)
  legend("left", legend = "p-value < 0.05 (Significant Trend)", pch=17)
  title(main = paste("Correlation between Annual Maximum", toupper(dataset),
      "and Year \n Since", start_year, "in Florida", sep=" "))
  corr_tmp = c()
  for(val in c(42:48)) {
    if (nclimdiv$CLIMDIV[order(nclimdiv$CLIMDIV)[val]] == statecodes[val]) {
      plot(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]], col =
          cor_color[val], border = "white", lwd = 2, add = TRUE)
      corr_tmp = append(corr_tmp, cor_color[val])
      if (p[val] <= 0.05) {
        if (tau[val] < 0) {
          print(nclimdiv$CLIMDIV[order(nclimdiv$CLIMDIV)[val]])
          print(tau[val])
          print(p[val])
          print(cor_color[val])
        }
        plot(st_centroid(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]]),
            pch = 17, add = TRUE)
      }
    }
  }
  # corr_spread = c()
  # scale_tmp = c()
  # corr_names = c()
  # for (i in 1:length(scale)) {
  #   tmp = subset(corr_tmp, corr_tmp == scale[i])
  #   if(is.null(nrow(tmp))) {
  #     corr_spread = append(corr_spread, length(tmp))
  #     scale_tmp = append(scale_tmp, scale[i])
  #     corr_names = append(corr_names, btwn_tau[i])
  #   }
  # }
  # barplot(corr_spread, ylim = c(0, max(corr_spread)), col = scale_tmp, names = corr_names,
  #     border = NA, axes = FALSE)
  .image_scale(tau_scale, col = scale[11:1], key.length = lcm(18), key.pos = 1,
      at = tau_scale)
  dev.off()

}
## End Heatmap correlation plot stuff ##
# make_fits = function(val) {
#   fitss = list()
#   fitss[[3]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun =
#       ~ poly(((year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
#   fitss[[4]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun = ~ poly(((year - start_year)/ data_years), 2, raw = TRUE),
#       units = "deg F")
#
#   # fitss with scale ~ year
#   fitss[[5]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, scale.fun =
#       ~ poly(((year - start_year)/ data_years), 1), units = "deg F")
#   fitss[[6]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, scale.fun = ~ poly(((year - start_year)/ data_years), 2),
#       units = "deg F")
#
#   # fitss with shape ~ year
#   # fitss[[val]][[7]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, shape.fun = ~ year, units = "deg F")
#   # fitss[[val]][[8]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, shape.fun = ~ I(year^2),
#   #     units = "deg F")
#
#   # fitss with location ~ year and scale ~ year
#   fitss[[7]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun =
#       ~ poly(((year - start_year)/ data_years), 1, raw = TRUE), scale.fun = ~ poly(((year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
#   fitss[[8]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun = ~ poly(((year - start_year)/ data_years), 2, raw = TRUE),
#       scale.fun = ~ poly(((year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
#   fitss[[9]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun = ~ poly(((year - start_year)/ data_years), 2),
#       scale.fun = ~ poly(((year - start_year)/ data_years), 2), units = "deg F")
#   fitss[[10]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun =
#       ~ poly(((year - start_year)/ data_years), 1), scale.fun = ~ poly(((year - start_year)/ data_years), 2), units = "deg F")
#
#   return(fitss)
# }

## Begin return peiod map stuff ##
prompt = "Would you like to develop the return value map? Enter 'y or 'Y'.\n"
answer = readline(prompt)
fits = list()
best_fits = list()
return_lvls_now = list()
return_lvls_then = list()
return_lvls_later = list()
# ci_vals = list()
crit = list()
rl_color = list()
if (answer == "Y" | answer == "y") {
  # This will hold our stationary and non-stationary lists respectively
  prompt = "Enter wanted return periods (space-seperated) (i.e. 2 20 100) \n"
  periods = unlist(strsplit(readline(prompt), "\\s+"))
  periods = as.integer(periods)
  #Develope filenames for each return period's plot
  return_then_plot = c()
  return_now_plot = c()
  return_later_plot = c()
  for (val in periods) {
    warning_plot = paste(extra_dir, "/warning-", dataset, "-", start_year, "-", sep="")
    return_then = paste("/climdiv-", dataset, "-rl-then-", val,
        "-map-", start_year, "yr", sep="")
    return_now = paste("/climdiv-", dataset, "-rl-now-", val,
        "-map-", start_year, "yr", sep="")
    return_later = paste("/climdiv-", dataset, "-rl-later-", val,
        "-map-", start_year, "yr", sep="")
    return_then_plot = append(return_then_plot, paste(figures_dir, return_then, ".jpeg",
        sep=""))
    return_now_plot = append(return_now_plot, paste(figures_dir, return_now, ".jpeg",
        sep=""))
    return_later_plot = append(return_later_plot, paste(figures_dir, return_later, ".jpeg",
        sep=""))
  }

  nonstationary = c()
  for (val in 1:344) {
    nonstationary = append(nonstationary, FALSE)
    qq_plot = paste(extra_dir, "/climdiv-", dataset, "-qq", statecodes[val], "-",
        start_year, ".jpeg", sep="")
    rl_plot = paste(extra_dir, "/climdiv-", dataset, "-rl", statecodes[val], "-",
        start_year, ".jpeg", sep="")

    fits[[val]] = list()
    # basic fit with gumble and GEV
    fits[[val]][[1]] = fevd(new_data[[val]][,2], new_data[[val]], type="Gumbel", units = "deg F")
    fits[[val]][[2]] = fevd(new_data[[val]][,2], new_data[[val]], type="GEV", units = "deg F")

    ## Start: determine whether to use Gumbel or GEV ##
    test = lr.test(fits[[val]][[1]], fits[[val]][[2]])

    ci_shape = "NA"
    GEV_type = "Gumbel"
    type_a = "Gumbel"
    type_r = "GEV"

    # type = "GEV"

    if (test$p.value < 0.05) {
      fit_a = fits[[val]][[2]]
      fit_r = fits[[val]][[1]]
      type_a = "GEV"
      type_r = "Gumbel"
      ci_shape = ci(fit_a, type="parameter", which.par=3)
      if (ci_shape[2] < 0) {
        GEV_type = "Weibull"
      } else if (ci_shape[2] > 0) {
        GEV_type = "Frechet"
      }
    } else {
      fit_a = fits[[val]][[1]]
      fit_r = fits[[val]][[2]]
    }
    ## End: determine whether to use Gumbel or GEV ##

    #TODO: If p from mann kendall test < 0.05, test which non-stationary model fits best
    #then reality check by doing lr.test with a stationary model. NOTE: output some
    #warning if the stationary model is chosen! NOTE: Perhaps also reality check with
    #alternate model (GEV or Gumbel) and test if ML/AIC value matches your decision
    #NOTE: The lowest Negative Log-Likelihood (highest Log-Likelihood) should be the
    #best fit. NOTE: Also provide qq plots to back up your decision and for an
    #explination in your presentation.
    if (p[val] < 0.05) {
      nonstationary[val] = TRUE

      # fits with location ~ year
      fits[[val]][[3]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun =
          ~ poly(((year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[val]][[4]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ poly(((year - start_year)/ data_years), 2, raw = TRUE),
          units = "deg F")

      # fits with scale ~ year
      fits[[val]][[5]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, scale.fun =
          ~ poly(((year - start_year)/ data_years), 1), units = "deg F")
      # fits[[val]][[6]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, scale.fun = ~ poly(((year - start_year)/ data_years), 2),
      #     units = "deg F")

      # fits with shape ~ year
      # fits[[val]][[7]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, shape.fun = ~ year, units = "deg F")
      # fits[[val]][[8]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, shape.fun = ~ I(year^2),
      #     units = "deg F")

      # fits with location ~ year and scale ~ year
      fits[[val]][[6]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun =
          ~ poly(((year - start_year)/ data_years), 1, raw = TRUE), scale.fun = ~ poly(((year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      fits[[val]][[7]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun = ~ poly(((year - start_year)/ data_years), 2, raw = TRUE),
          scale.fun = ~ poly(((year - start_year)/ data_years), 1, raw = TRUE), units = "deg F")
      # fits[[val]][[9]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun = ~ poly(((year - start_year)/ data_years), 2),
      #     scale.fun = ~ poly(((year - start_year)/ data_years), 2), units = "deg F")
      # fits[[val]][[10]] = fevd(new_data[[val]][,2], new_data[[val]], type = type, location.fun =
      #     ~ poly(((year - start_year)/ data_years), 1), scale.fun = ~ poly(((year - start_year)/ data_years), 2), units = "deg F")

      # # fits with location ~ year and scale ~ year and shape ~ year
      # fit[[13]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ year,
      #     scale.fun = ~ year, shape.fun = ~ year units = "deg F")
      # fit[[14]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ poly(year,2),
      #     scale.fun = ~ year, shape.fun = ~ year, units = "deg F")
      # fit[[15]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ poly(year,2),
      #     scale.fun = ~ poly(((year - start_year)/ data_years), 2), shape.fun = ~ year, units = "deg F")
      # fit[[16]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ year,
      #     scale.fun = ~ poly(((year - start_year)/ data_years), 2), shape.fun = ~ year, units = "deg F")
      # fit[[17]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ year,
      #     scale.fun = ~ year, shape.fun = ~ poly(((year - start_year)/ data_years), 2), units = "deg F")
      # fit[[18]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ poly(year,2),
      #     scale.fun = ~ year, shape.fun = ~ poly(((year - start_year)/ data_years), 2), units = "deg F")
      # fit[[19]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ poly(year,2),
      #     scale.fun = ~ poly(((year - start_year)/ data_years), 2), shape.fun = ~ poly(((year - start_year)/ data_years), 2), units = "deg F")
      # fit[[20]] = fevd(new_data[[val]][,2], new_data[[val]], type = type_a, location.fun = ~ year,
      #     scale.fun = ~ poly(((year - start_year)/ data_years), 2), shape.fun = ~ poly(((year - start_year)/ data_years), 2), units = "deg F")

      ##Start: determine best fit ##
      tmp = c()
      tmp2 = c()
      crit[[val]] = c()
      for (it in 3:(length(fits[[val]]))) {
        tmp = append(tmp, fits[[val]][[it]]$result$value)
        try({tmp2 = append(tmp2, as.double(summary(fits[[val]][[it]])$AIC))})
      }

      if (length(tmp2) != 0) {
        best_fit = fits[[val]][[which.min(tmp2) + 2]]
        worst_fit = fits[[val]][[which.max(tmp2) + 2]]
        crit[[val]] = tmp2

      } else {
        best_fit = fits[[val]][[which.min(tmp) + 2]]
        worst_fit = fits[[val]][[which.max(tmp) + 2]]
        crit[[val]] = tmp
      }
      #
      # while(min(tmp) > 500) {
      #   new_data[[val]][,2][sample(1:data_years+1, 1)] =
      #       new_data[[val]][,2][sample(1:data_years+1, 1)] - 0.1
      #   fits[[val]] = make_fits(val)
      #   tmp = c()
      #   tmp2 = c()
      #   crit[[val]] = c()
      #   for (it in 3:(length(fits[[val]]))) {
      #     tmp = append(tmp, fits[[val]][[it]]$result$value)
      #     try({tmp2 = append(tmp2, as.double(summary(fits[[val]][[it]])$AIC))})
      #   }
      #
      #   if (length(tmp2) != 0) {
      #     best_fit = fits[[val]][[which.min(tmp2) + 2]]
      #     worst_fit = fits[[val]][[which.max(tmp2) + 2]]
      #     crit[[val]] = tmp2
      #
      #   } else {
      #     best_fit = fits[[val]][[which.min(tmp) + 2]]
      #     worst_fit = fits[[val]][[which.max(tmp) + 2]]
      #     crit[[val]] = tmp
      #   }
      # }


      # We ignore the ones where shape ~ year as well as less complexe models
      if (((which.min(crit[[val]]) + 2) > 6) & ((which.max(crit[[val]]) + 2) <= 6)) {
        test = lr.test(worst_fit, best_fit)
        if (test$p.value > 0.05) {
          print("Warning: chosen best (complex) fit fails lr.test against worst (simpler) fit,
              trying another method...")
          warning_pos = append(warning_pos, val)
        }
      } else {
        test1 = lr.test(best_fit, fit_a)
        test2 = lr.test(worst_fit, fit_a)
        if (test1$p.value > test2$p.value) {
          print("Warning: chosen best fit fails lr.test p-value check (testing against simpler fit)")
          warning_pos = append(warning_pos, val)
        }
      }
      ##End: determine best fit ##
      try({
      jpeg(qq_plot, width=1050, height=350)
      par(mfcol=c(1,3), oma=c(0,0,2,0))
      plot(best_fit, type="qq2", xlab="Best Non-stationary Fit's Empirical Quantiles")
      plot(worst_fit, type="qq2", xlab= "Worst Non-stationary Fit's Empirical Quantiles")
      plot(fit_a, type="qq2", xlab="Best Stationary Fit's Empirical Quantiles")
      title(paste("QQ Plots for Various Fits at State/Divisional Code",
          statecodes[val], sep=" "), outer = TRUE)
    })
    dev.off()

    } else {
      best_fit = fit_a
      worst_fit = fit_r
      if (worst_fit$results$value < (best_fit$results$value + 100)) {
        jpeg(qq_plot, width=700, height=350)
        try({
        par(mfcol=c(1,2), oma=c(0,0,2,0))
        plot(best_fit, type="qq2", xlab="Best Fit's Empirical Quantiles")
        plot(worst_fit, type="qq2", xlab="Worst Fit's Empirical Quantiles")
        title(paste("QQ Plots for Various Fits at State/Divisional Code",
            statecodes[val], sep=" "), outer = TRUE)
        })
        dev.off()
      } else {
        jpeg(qq_plot, width=350, height=350)
        try({
        par(mfcol=c(1,1), oma=c(0,0,2,0))
        plot(best_fit, type="qq2", xlab="Best Fit's Empirical Quantiles")
        title(paste("QQ Plots for Best Fits at State/Divisional Code",
            statecodes[val], sep=" "), outer = TRUE)
        dev.off()
        warning2_pos = append(warning2_pos, val)
        })
      }
    }

    try({
    jpeg(rl_plot, width=500, height=500)
    plot(best_fit, type="rl", rperiods = periods)
    title(paste("Estimated Return Levels at State/Divisional Code",
        statecodes[val], sep=" "), outer = TRUE)
    dev.off()
    })

    return_lvls_now[[val]] = list()
    return_lvls_then[[val]] = list()
    return_lvls_later[[val]] = list()
    # ci_vals[[val]] = list()

    if (nonstationary[val]) {
      worked = FALSE
      i = 1
      while((worked == FALSE) & (i <= (length(fits[[val]])))) {
        if (i > 1) {
          print("I'm doing stuff")
          warning3_pos = append(warning3_pos, val)
        } else {
          if (best_fit$results$value != fits[[val]][[order(crit[[val]])[i] + 2]]$results$value) {
            print("ERROR!")
          }
        }
        best_fit = fits[[val]][[order(crit[[val]])[i] + 2]]
        try({
        tmp = return.level(best_fit, return.period = periods)
        tmp2 = return.level(best_fit, return.period = periods)
        for (it in c(1:length(periods))) {
          return_lvls_now[[val]][[it]] = tmp[(data_years + 1), it]
          return_lvls_then[[val]][[it]] = tmp2[1, it]
        }
        worked = TRUE
        })
        i = i + 1
      }
      try ({
      flag = FALSE
        # if loc ~ year
      if (((order(crit[[val]])[i] + 1) == 3) | ((order(crit[[val]])[i] + 1) == 6)) {
        v = make.qcov(best_fit, vals = list(mu1 = 2))
          # if loc ~ year^2
      } else if (((order(crit[[val]])[i] + 1) == 4) | ((order(crit[[val]])[i] + 1) == 7)) {
        v = make.qcov(best_fit, vals = list(mu1 = 2, mu2 = 4))
        flag = TRUE
      } else {
        v = make.qcov(best_fit)
      }
      rl_later = as.double(return.level(best_fit, return.period = periods, qcov = v))
      for(it in 1:length(periods)) {
        return_lvls_later[[val]][[it]] = rl_later[it]
      }
      for(it in 1:length(periods)) {
        if ((return_lvls_later[[val]][[it]] > (return_lvls_now[[val]][[it]] + 5)) |
            (return_lvls_later[[val]][[it]] < (return_lvls_now[[val]][[it]] - 5))) {
          if (flag) {
            best_fit = fits[[val]][[order(crit[[val]][i])]]
          } else {
            best_fit = fits[[val]][[3]]
          }
          tmp = return.level(best_fit, return.period = periods)
          tmp2 = return.level(best_fit, return.period = periods)
          for (it in c(1:length(periods))) {
            return_lvls_now[[val]][[it]] = tmp[(data_years + 1), it]
            return_lvls_then[[val]][[it]] = tmp2[1, it]
          }
          v = make.qcov(best_fit, vals = list(mu1 = 2))
          rl_later = as.double(return.level(best_fit, return.period = periods, qcov = v))
          for(it in 1:length(periods)) {
            return_lvls_later[[val]][[it]] = rl_later[it]
          }
        }
      }
      })
    } else {
      tmp = return.level(best_fit, return.period = periods)
      for (it in c(1:length(periods))) {
        return_lvls_now[[val]][[it]] = tmp[it]
      }
      return_lvls_then[[val]] = return_lvls_now[[val]]
      return_lvls_later[[val]] = return_lvls_now[[val]]
      tmp = matrix(tmp, nrow=(data_years + 1), ncol=length(periods), byrow=TRUE)
      tmp2 = tmp
    }

    if ((nonstationary[val]) & (length(return_lvls_later[[val]]) == 0)) {
      warning4_pos = append(warning4_pos, val)
      for(it in 1:length(periods)) {
        trend = lm(tmp[,it] ~ poly(new_data[[val]]$year, 2, raw = TRUE))
        predicted = predict(trend, data.frame(x=start_year:2018), interval='confidence', level=0.95)
        coef0 = signif(as.double(coef(trend)[1]), 5)
        coef1 = signif(as.double(coef(trend)[2]), 5)
        coef2 = signif(as.double(coef(trend)[3]), 5)
        rl_later = (coef0 + coef1*(2018 + data_years) + coef2*((2018 + data_years)^2))
        return_lvls_later[[val]][[it]] = rl_later
        jpeg(paste(warning_plot, val, ".jpeg", sep=""), width = 5000, height = 5000)
        plot(start_year:2018, tmp[,it])
        lines(start_year:2018, predicted[,1], col=scale[10])
        dev.off()

      }
    }

    for (it in 1:length(periods)) {
      if ((return_lvls_later[[val]][[it]] > (return_lvls_now[[val]][[it]] + 8)) |
          (return_lvls_later[[val]][[it]] < (return_lvls_now[[val]][[it]] - 1))) {
      best_fit = fits[[val]][[3]]
      tmp = return.level(best_fit, return.period = periods)
      tmp2 = return.level(best_fit, return.period = periods)
      worked = FALSE
      try({
      for (it in c(1:length(periods))) {
        return_lvls_now[[val]][[it]] = tmp[(data_years + 1), it]
        return_lvls_then[[val]][[it]] = tmp2[1, it]
      }
      v = make.qcov(best_fit, vals = list(mu1 = 2))
      rl_later = as.double(return.level(best_fit, return.period = periods, qcov = v))
      for(it in 1:length(periods)) {
        return_lvls_later[[val]][[it]] = rl_later[it]
      }
      worked = TRUE
      })
    if (!worked) {
      for (it in c(1:length(periods))) {
        return_lvls_later[[val]][[it]] = 2*return_lvls_now[[val]][[it]] - return_lvls_then[[val]][[it]]
      }
    }

    } }

    best_fits[[val]] = best_fit

    rl_color[[val]] = list()
    ## return_lvls_now setup ##
    for (it in c(1:length(periods))) {
      if (return_lvls_now[[val]][[it]] > rl_scale[2]) {
        rl_color[[val]][[it]] = scale2[1]
      }
      for (jt in c(2:(length(scale2)-1))) {
        if ((return_lvls_now[[val]][[it]] <= rl_scale[jt])
            & (return_lvls_now[[val]][[it]] > rl_scale[jt+1])) {
          rl_color[[val]][[it]] = scale2[jt]
        }
      }
      if (return_lvls_now[[val]][[it]] < rl_scale[length(scale2)]) {
        rl_color[[val]][[it]] = scale2[length(scale2)]
      }
    }
      ## return_lvls_then setup ##
    for (it in c(1:length(periods))) {
      if (return_lvls_then[[val]][[it]] > rl_scale[2]) {
        rl_color[[val]][[it+length(periods)]] = scale2[1]
      }
      for (jt in c(2:length(scale2)-1)) {
        if ((return_lvls_then[[val]][[it]] <= rl_scale[jt])
            & (return_lvls_then[[val]][[it]] > rl_scale[jt+1])) {
          rl_color[[val]][[it+length(periods)]] = scale2[jt]
        }
      }
      if (return_lvls_then[[val]][[it]] < rl_scale[length(scale2)]) {
        rl_color[[val]][[it+length(periods)]] = scale2[length(scale2)]
      }
    }
    ## return_lvls_later setup ##
    for (it in c(1:length(periods))) {
      if (return_lvls_later[[val]][[it]] > rl_scale[2]) {
        rl_color[[val]][[it+2*length(periods)]] = scale2[1]
      }
      for (jt in c(2:length(scale2)-1)) {
        if ((return_lvls_later[[val]][[it]] <= rl_scale[jt])
            & (return_lvls_later[[val]][[it]] > rl_scale[jt+1])) {
          rl_color[[val]][[it+2*length(periods)]] = scale2[jt]
        }
      }
      if (return_lvls_later[[val]][[it]] < rl_scale[length(scale2)]) {
        rl_color[[val]][[it+2*length(periods)]] = scale2[length(scale2)]
      }
    }

    ## return_lvls_now - return_lvls_then plot (change in expected return lvls ##
    change = c()
    for(it in c(1:length(periods))) {
      change[it] = as.double(return_lvls_now[[val]][[it]]) - as.double(return_lvls_then[[val]][[it]])
      if (change[it] > rl_dif_scale[2]) {
        rl_color[[val]][[it+3*length(periods)]] = scale2[1]
      }
      for (jt in c(2:length(scale2)-1)) {
        if ((change[it] <= rl_dif_scale[jt]) & (change[it] > rl_dif_scale[jt+1])) {
          rl_color[[val]][[it+3*length(periods)]] = scale2[jt]
        }
      }
      if (change[it] < rl_dif_scale[length(scale2)]) {
        rl_color[[val]][[it+3*length(periods)]] = scale2[length(scale2)]
      }
    }
    if (p[val] < 0.05) {
      change_vals = rbind(change_vals, change)
    } else {
      change_vals = rbind(change_vals, c(0,0,0))
    }
  }

  ## Begin return levels map ##
  nclimdiv_shp_file = "SHP_Files/GIS.OFFICIAL_CLIM_DIVISIONS.shp"
  nclimdiv = st_read(nclimdiv_shp_file)

  #Opens correlation heatmap and plots empty nclimdiv map
  for (it in c(1:length(periods))) {
    ## Start "change from then to now" return level plot
    print("Developing 'now' return level plot")
    jpeg(return_now_plot[it], width=1100, height=700)
    layout(matrix(1:2, nrow = 2), heights = c(9, 1))
    par(mar = c(1,1,1,2), oma = c(1,0,1,0))
    plot(st_geometry(nclimdiv), lwd = 2, border = "white")
    title(main = paste("The Change in Expected Maximum", toupper(dataset), "Return Levels for",
    periods[it], "Years Between", start_year, "and 2018 Using Polynomial Models", sep=" "))
    #Just in case chars cause issues in below loop
    statecodes = as.integer(statecodes)
    for(val in c(1:344)) {
      if (nclimdiv$CLIMDIV[order(nclimdiv$CLIMDIV)[val]] == statecodes[val]) {
        if (nonstationary[val]) {
          plot(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]], col =
              rl_color[[val]][[it + 3*length(periods)]], lwd = 2, border = "white", add = TRUE)
          # text(st_coordinates(st_centroid(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]])),
          #     labels = round(return_lvls_now[[val]][[it]], 0.5), cex=0.7, pos=2)
        } else {
          plot(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]], col =
              "#E0E0E0", lwd = 2, border = "white", add = TRUE)
        }
        # if (is.na(ci_vals[val])) {
        #   plot(st_centroid(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]]),
        #       pch = 15, add = TRUE)
        # }
        # tmp = c()
        # for (a in 1:344) { for (b in 1:length(periods)) {
        #   tmp = append(ci_vals[[a]][[b]])
        # }}
        ci = mean(tmp, na.rm = TRUE)
        legend("right", legend = c(paste("Average Change in Return Level:",
            round(mean(as.numeric(change_vals[,it])), 2), sep=" ")))
            # paste("Average Uncertainty (Excluding those with Errors)",
            # round(mean(ci_vals, na.rm = TRUE), 2), sep=" ")))
        # legend("bottomright", legend="Uncertainty Unavaliable", pch=15)
      }
    }
    # corr_spread = c()
    # for (i in c(1:length(scale2))) {
    #   for (val in c(1:344) {
    #     for (it in c(1:length(periods)))
    #     tmp = subset(rl_color[[val]][[it + 3*length(periods)]],
    #         rl_color[[val]][[it + 3*length(periods)]] == scale[i])
    #   }
    #   corr_spread = append(corr_spread, length(tmp))
    # }
    # barplot(corr_spread, ylim = c(0, max(corr_spread)), col = scale, names = btwn_tau,
    #     border = NA, axes = FALSE)
    .image_scale(rl_dif_scale, col = scale2[21:1], key.length = lcm(25), key.pos = 1,
        at = rl_dif_scale)
    dev.off()
    ## End "change from then to now" return level plot

    ## Start "then" return level plot
    print("Developing 'then' return level plot")
    jpeg(return_then_plot[it], width=1100, height=700)
    layout(matrix(1:2, nrow = 2), heights = c(9, 1))
    par(mar = c(1,1,1,2), oma = c(1,0,1,0))
    plot(st_geometry(nclimdiv), lwd = 2, border = "white")
    title(main = paste("Expected Maximum Return Levels for", periods[it], "Years",
      "for", toupper(dataset), "in", start_year, "Using Polynomial Models", sep=" "))
    #Just in case chars cause issues in below loop
    statecodes = as.integer(statecodes)
    for(val in c(1:344)) {
      if (nclimdiv$CLIMDIV[order(nclimdiv$CLIMDIV)[val]] == statecodes[val]) {
        plot(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]], col =
            rl_color[[val]][[it + length(periods)]], border = "white", lwd = 2,
                add = TRUE)
        # text(st_coordinates(st_centroid(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]])),
        #     labels = round(return_lvls_now[[val]][[it]], 1), cex=0.7, pos=2)
      }
    }
    .image_scale(rl_scale, col = scale2[21:1], key.length = lcm(25), key.pos = 1,
        at = rl_scale)
    dev.off()
    ## End "then" return level plot

    ## Start "later" return level plot
    print("Developing 'later' return level plot")
    jpeg(return_later_plot[it], width=1100, height=700)
    layout(matrix(1:2, nrow = 2), heights = c(9, 1))
    par(mar = c(1,1,1,2), oma = c(1,0,1,0))
    plot(st_geometry(nclimdiv), lwd = 2, border = "white")
    title(main = paste("Expected Maximum Return Levels for", periods[it], "Years",
      "for", toupper(dataset), "in", 2018 + data_years, "Using Polynomial Models", sep=" "))
    #Just in case chars cause issues in below loop
    statecodes = as.integer(statecodes)
    for(val in c(1:344)) {
      if (nclimdiv$CLIMDIV[order(nclimdiv$CLIMDIV)[val]] == statecodes[val]) {
        plot(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]], col =
            rl_color[[val]][[it + 2*length(periods)]], border = "white", lwd = 2, add = TRUE)
        # text(st_coordinates(st_centroid(st_geometry(nclimdiv)[order(nclimdiv$CLIMDIV)[val]])),
        #     labels = round(return_lvls_now[[val]][[it]], 1), cex=0.7, pos=2)
      }
    }
    .image_scale(rl_scale, col = scale2[21:1], key.length = lcm(25), key.pos = 1,
        at = rl_scale)
    dev.off()
  }
  ## End "later" return level plot
  ## End return levels map ##

  for (it in 1:344) {
    for (jt in 1:3) {
      if ((return_lvls_later[[it]][[jt]] > (return_lvls_now[[it]][[jt]] + 8)) |
          (return_lvls_later[[it]][[jt]] < (return_lvls_now[[it]][[jt]] - 5))) {
        print(paste(it, statecodes[it], return_lvls_later[[it]][[jt]], return_lvls_now[[it]][[jt]]))
  } } }

  for (it in warning4_pos) {
    print(paste(it, return_lvls_later[[it]][1], return_lvls_now[[it]][1]))
  }
}



## End return peiod map stuff ##

# plot(st_geometry(nclimdiv[2])[c(47:49,58:61)], col = "red", lwd = 0.001)
#
# text(st_coordinates(st_centroid(st_geometry(nclimdiv))), labels = "sick", cex=0.9, pos=4)
#
# aic = as.double(summary(fit1)[5])

if (database == "st") {
  print("Block Maxima Approach:")
  print(paste("Max mean:", max(mean), "at", statenames[which.max(mean)]))
  print(paste("Max var:", max(var), "at", statenames[which.max(var)]))
} else {
  print("Block Maxima Approach:")
  print(paste("Max mean:", max(mean), "at", statecodes[which.max(mean)]))
  print(paste("Max var:", max(var), "at", statecodes[which.max(var)]))
}
