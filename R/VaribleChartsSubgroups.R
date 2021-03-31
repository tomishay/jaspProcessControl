VaribleChartsSubgroups <- function(jaspResults, dataset, options){
  variables <- unlist(options$variables)
  time <- options$time
  makeTime <- time != ""
  numberMissingSplitBy <- 0

  if (is.null(dataset)) {
    if (makeTime) {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = time)
      dataset.factors <- .readDataSetToEnd(columns=variables, columns.as.factor=time)
    } else {
      dataset         <- .readDataSetToEnd(columns.as.numeric=variables)
      dataset.factors <- .readDataSetToEnd(columns=variables)
    }
  }
  if (makeTime && length(variables) > 0) {
    splitFactor      <- dataset[[.v(time)]]
    splitLevels      <- levels(splitFactor)
  }
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)

  jaspResults[["intro"]] <- createJaspHtml(gettext("Select one of the control charts from the interface."))
  jaspResults[["intro"]]$position <- 0
#X bar chart
  if(options$Xbarchart && is.null(jaspResults[["Xbarchart"]]) &&  length(options$variables) > 1){
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X bar chart"), width = 700, height = 350)
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarchart", "variables"))
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset[, options$variables], options = options)

    jaspResults[["RPlot"]] <- createJaspPlot(title =  gettext("R chart"), width = 700, height = 350)
    jaspResults[["RPlot"]]$dependOn(c("Xbarchart", "variables"))
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .RchartNoId(dataset = dataset[, options$variables], options = options)
  }

#S Chart
  if(options$Schart && is.null(jaspResults[["Schart"]]) &&  length(options$variables) > 1){
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X bar chart"), width = 700, height = 350)
    jaspResults[["XbarPlot"]]$dependOn(c("Schart", "variables"))
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset[, options$variables], options = options)

    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("S Chart"), width = 700, height = 350)
    jaspResults[["SPlot"]]$dependOn(c("Schart", "variables"))
    SPlot<- jaspResults[["SPlot"]]
    SPlot$plotObject <- .Schart(dataset = dataset, options = options)
  }
#Date chart
  if(options$Dchart && is.null(jaspResults[["Dchart"]]) &&  length(options$variables) > 1){
    jaspResults[["DPlot"]] <- createJaspPlot(title = gettext("Xbar & S Chart"), width = 700, height = 350)
    jaspResults[["DPlot"]]$dependOn(c("Dchart", "variables"))
    DPlot<- jaspResults[["DPlot"]]
    DPlot$plotObject <- Xbar.date(dataset = dataset, options = options)
  }
}

#Functions for control charts
.Schart <- function(dataset, options){
  data1 <- dataset[, options$variables]
  Stdv <- apply(data1, 1, function(x) sd(x))
  subgroups <- c(1:length(Stdv))
  data_plot <- data.frame(subgroups = subgroups, Stdv = Stdv)

  sixsigma <- qcc::qcc(data1, type ='S', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL + 0.1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- c(0,max(xBreaks) + 5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = Stdv)) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "darkred") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Standard Deviation") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$Stdv > UCL | data_plot$Stdv < LCL, 'red', 'gray')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
Xbar.date <- function(dataset, options){
  #Xbar
  data2 <- dataset[, options$variables]
  means <- rowMeans(data2)
  subgroups2 <- c(1:length(means))
  data_plot2 <- data.frame(subgroups = subgroups2, means = means)
  sixsigma2 <- qcc::qcc(data2, type ='xbar', plot=FALSE)
  center2 <- sixsigma2$center
  sd1 <- sixsigma2$std.dev
  UCL2 <- max(sixsigma2$limits)
  LCL2 <- min(sixsigma2$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL2, UCL2))
  yLimits <- range(yBreaks)
  xBreaks2 <- jaspGraphs::getPrettyAxisBreaks(dataset[[.v(options$time)]])
  dfLabel2 <- data.frame(
    x = max(subgroups2),
    y = c(center2, UCL2, LCL2),
    l = c(
      gettextf("CL = %g", round(center2, 3)),
      gettextf("UCL = %g",   round(UCL2, 3)),
      gettextf("LCL = %g",   round(LCL2, 3))
    )
  )

  p <- ggplot2::ggplot(data_plot2, ggplot2::aes(x = subgroups2, y = means)) +
    ggplot2::geom_hline(yintercept =  center2, color = 'green', size = 1) +
    ggplot2::geom_hline(yintercept = c(UCL2, LCL2), color = "red", linetype = "dashed", size = 1) +
    ggplot2::geom_label(data = dfLabel2, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Subgroup mean") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_discrete(name = gettext('Subgroup'), breaks = c(xBreaks2), limits = c(xBreaks2)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot2$means > UCL2 | data_plot2$means < LCL2, "red", "blue")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
