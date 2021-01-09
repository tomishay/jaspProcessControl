#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

controlCharts <- function(jaspResults, dataset, options){

#Make the Process ID and clean the dataset
  variables <- unlist(options$variables)
  total <- unlist(options$total)
  D <- unlist(options$D)
  numeric_variables <- c(variables, total, D)
  numeric_variables  <- numeric_variables[numeric_variables != ""]
  
  ID <- options$ProcessID
  makeID <- ID != ""
  numberMissing <- 0
  
  dataset         <- .readDataSetToEnd(columns.as.numeric = numeric_variables, exclude.na.listwise = numeric_variables)
#Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)
  
  jaspResults[["intro"]] <- createJaspHtml("Select one of the control charts to from the interface.", "p")
  jaspResults[["intro"]]$position <- 0
#X bar chart
  if(options$Xbarchart && is.null(jaspResults[["Xbarchart"]])){
    jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 900, height = 400)
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarchart", "variables"))
    jaspResults[["XbarPlot"]]$position <- 11
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .Xbarchart(dataset = dataset, options = options)
  }
#R Chart
  if(options$Rchart && is.null(jaspResults[["Rchart"]])){
    jaspResults[["RPlot"]] <- createJaspPlot(title = "R chart", width = 900, height = 400)
    jaspResults[["RPlot"]]$dependOn(c("Rchart", "variables"))
    jaspResults[["RPlot"]]$position <- 11
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .Rchart(dataset = dataset, options = options)
  }
#S Chart
  if(options$Schart && is.null(jaspResults[["Schart"]])){
    jaspResults[["SPlot"]] <- createJaspPlot(title = "S chart", width = 900, height = 400)
    jaspResults[["SPlot"]]$dependOn(c("Schart", "variables"))
    jaspResults[["SPlot"]]$position <- 11
    SPlot<- jaspResults[["SPlot"]]
    SPlot$plotObject <- .Schart(dataset = dataset, options = options)
  }
#ImR chart
  if(options$ImRchart){
    if(is.null(jaspResults[["ImRchart"]])){
      jaspResults[["ImRchart"]] <- createJaspContainer(gettext("ImR charts"))
      jaspResults[["ImRchart"]]$dependOn(c("ImRchart", "variables"))
      jaspResults[["ImRchart"]]$position <- 11
    }

    ImRplot <- jaspResults[["ImRchart"]]

    for(var in variables){
      ImRplot[[var]] <- .ImRchart(dataset = dataset, options = options, variable = var)
    }
  }
#P chart
  if(options$Defectivescharts){
    if(options$TypeDefectives == "pchart" && is.null(jaspResults[["pchart"]])){
      jaspResults[["PchartPlot"]] <- createJaspPlot(title = "P chart", width = 900, height = 400)
      jaspResults[["PchartPlot"]]$dependOn(c("total", "D", "Defectivescharts", "TypeDefectives"))
      jaspResults[["PchartPlot"]]$position <- 11
      PPlot <- jaspResults[["PchartPlot"]]
      PPlot$plotObject <- .Pchart(dataset = dataset, options = options)
    }
  }
}

#Functions for control charts 
.Schart <- function(dataset, options){
  data1 <- dataset[, encodeColNames(options$variables)]
  Stdv <- apply(data1, 1, function(x) sd(x))
  subgroups <- c(1:length(Stdv))
  data_plot <- data.frame(subgroups = subgroups, Stdv = Stdv)
  
  sixsigma <- qcc::qcc(data1, type ='S', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$Stdv, UCL + 1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks)
  dfLabel <- data.frame(
    x = max(xLimits) + 1,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("SD = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = Stdv)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$Stdv > UCL | data_plot$Stdv < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroup Standard Deviation" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroup', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
.ImRchart <- function(dataset, options, variable){
  title <- gettextf("Variable: %s", variable )
  ppPlot <- createJaspPlot(width = 900, height = 400, title = title)
  ppPlot$dependOn(optionContainsValue = list(variables = variable))

  #data
  data <- data.frame(process = dataset[[.v(variable)]])
  subgroups <- c(1:length(data$process))
  sixsigma <- qcc::qcc(data$process, type ='xbar.one', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 1, UCL + 1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(subgroups))
  xLimits <- range(xBreaks)
  dfLabel <- data.frame(
    x = max(xLimits) + 1,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("Mean = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )

  p <- ggplot2::ggplot(data, ggplot2::aes(x = subgroups, y = process)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data$process > UCL | data$process < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Value" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Observations', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  ppPlot$plotObject <-  p
  return(ppPlot)
}

.Pchart <- function(dataset, options){
  ready <- options$D > 0 & options$total > 0
  if (!ready)
    return()
  
  data1 <- data.frame(D = dataset[, encodeColNames(options$D)], sample = dataset[, encodeColNames(options$total)])
  data1$P <- data1$D/data1$sample
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)
  
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "p", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$P ,UCL + 0.1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(subgroups))
  xLimits <- range(xBreaks)
  dfLabel <- data.frame(
    x = max(xLimits) + 1,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("Mean P = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroups' Proportions" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}
.Xbarchart<- function(dataset, options){
    ready <- (length(options$variables) > 1)
    if (!ready)
      return()
  
  data1 <- dataset[, encodeColNames(options$variables)]
  means <- rowMeans(data1)
  subgroups <- 1:length(means)
  data_plot <- data.frame(subgroups = subgroups, means = means)
  
  sixsigma <- qcc::qcc(data1, type ='xbar', plot=FALSE)
  center <- sixsigma$center
  sd1 <- sixsigma$std.dev
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 1, UCL + 1, data_plot$means))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(subgroups))
  xLimits <- range(xBreaks)
  dfLabel <- data.frame(
    x = max(xLimits) + 1,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("Mean = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = means)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$means > UCL | data_plot$means < LCL, "red", "gray")) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroups' Mean" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}

# Function to create R chart
.Rchart <- function(dataset, options){
    ready <- (length(options$variables) > 1)
    if (!ready)
      return()

  #Arrange data and compute
  data1 <- dataset[, encodeColNames(options$variables)]
  range <- apply(data1, 1, function(x) max(x) - min(x))
  subgroups <- 1:length(range)
  data_plot <- data.frame(subgroups = subgroups, range = range)
  
  sixsigma <- qcc::qcc(data1, type= 'R', plot = FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(UCL, data_plot$range, UCL + 1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks)
  dfLabel <- data.frame(
    x = max(xLimits) + 1,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("Range = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = range)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$range > UCL | data_plot$range < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL,LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroups' Range" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = "Subgroups" ,breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}


