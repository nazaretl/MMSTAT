
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MMSTATsample_param** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : MMSTATsample_param

Published in : MMSTAT

Description : 'Shows estimated parameters for univariate data sample. The user can interactively
choose the parameter that is estimated (mean, median, standard deviation, interquartile range) and
the sample size. Also, variables of the data sets CARS, USCRIME and BOSTONHOUSING are available.
The upper panel shows a histogram of the parameter estimates of all previously drawn samples. The
lower panel shows a scatterplot of the whole population (green) and the current sample (orange). A
box indicates the interquartile range and the mean.'

Keywords : 'plot, scatterplot, histogram, boxplot, mean, median, quantile, visualization, data
visualization, parameter, interactive, uscrime, standard deviation, sampling, empirical,
estimation, distribution'

See also : 'BCS_Hist1, BCS_Hist2, MSRsca_bmw_vw, BCS_Boxplot, MMSTATtime_series_1, MMSTATlinreg,
MMSTATconfmean, MMSTATconfi_sigma, MMSTATassociation, MMSTAThelper_function'

Author : Sigbert Klinke

Code Editor : Yafei Xu

Submitted : 21/08/2015

Input : MMSTAThelper_function

Output : Interactive shiny application

Datafiles : CARS.rds, USCRIME.rds, BOSTONHOUSING.rds

Example : 'Uses the variable POPULATION of the USCRIME data set. It shows the histogram of the mean
estimates in the upper panel and the comparison of the population and the sample in the lower
panel.'

```

![Picture1](MMSTATsample_param.png)


### R Code:
```r
# ------------------------------------------------------------------------------
# Name of Quantlet: MMSTATsample_param
# ------------------------------------------------------------------------------
# Published in:     MMSTAT
# ------------------------------------------------------------------------------
# Description:      Shows estimated parameters for univariate data sample.
#                   The user can interactively choose the parameter that is estimated
#                   (mean, median, standard deviation, interquartile range) and the sample size.
#                   Also, variables of the data sets CARS, USCRIME and BOSTONHOUSING are available.
#                   The upper panel shows a a histogram of the parameter estimates of
#                   all previously drawn samples.
#                   The lower panel shows a scatterplot of the whole population (green) and
#                   the current sample (orange). A box indicates the interquartile range and
#                   the mean. 
# ------------------------------------------------------------------------------
# Keywords:         plot, scatterplot, histogram, boxplot, mean, median, quantile,
#                   visualization, data visualization, parameter, interactive, 
#                   uscrime, standard deviation, sampling, empirical, estimation,
#                   distribution
# ------------------------------------------------------------------------------
# Usage:            MMSTAThelper_function
# ------------------------------------------------------------------------------
# Output:           Interactive shiny application
# ------------------------------------------------------------------------------
# Example:          Uses the variable POPULATION of the USCRIME data set.
#                   It shows the histogram of the mean estimates in the upper panel
#                   and the comparison of the population and the sample in the lower
#                   panel.              
# ------------------------------------------------------------------------------
# See also:         BCS_Hist1, BCS_Hist2, MSRsca_bmw_vw, BCS_Boxplot,
#                   MMSTATtime_series_1, MMSTATlinreg, MMSTATconfmean, 
#                   MMSTATconfi_sigma, MMSTATassociation, MMSTAThelper_function
# ------------------------------------------------------------------------------
# Author :          Sigbert Klinke
# ------------------------------------------------------------------------------
# Code Editor:      Yafei Xu
# ------------------------------------------------------------------------------
# Datafiles:        CARS.rds, USCRIME.rds, BOSTONHOUSING.rds
# ------------------------------------------------------------------------------

# please use "Esc" key to jump out of the Shiny app
rm(list = ls(all = TRUE))
graphics.off()

# please set working directory setwd('C:/...') 
# setwd('~/...')    # linux/mac os
# setwd('/Users/...') # windows

source("MMSTAThelper_function.r")

############################### SUBROUTINES ##################################
### server ###################################################################

dpc = gettext(c("MEAN", "MEDIAN", "STDDEV", "IQR"), "name")

mmstat$vartype = "numvars"
mmstat.ui.elem("param", "selectInput", 
               label   = gettext("Select parameter"), 
               choices = dpc, 
               value   = "MEAN")
mmstat.ui.elem("size", "sampleSize")
mmstat.ui.elem("go", "drawSample")
mmstat.ui.elem("speed", "speedSlider")
mmstat.ui.elem("dataset", "dataSet", 
               choices = mmstat.getDataNames("USCRIME", "CARS", 
                                             "BOSTONHOUSING"))
mmstat.ui.elem("variable", "variable1", vartype = "numeric")
mmstat.ui.elem("cex", "fontSize")

param = c()

drawIqrBoxWithPoints = function(x, jitter, ylim, box.param = NULL, points.param = NULL) {
  if (is.list(points.param) || is.null(points.param) || points.param) {
    points.param$x = x
    points.param$y = ylim[1] + diff(ylim) * jitter
    suppressWarnings(do.call("points", points.param))
  }
  if (is.list(box.param) || is.null(box.param) || box.param) {
    q                 = quantile(x, c(0.25, 0.5, 0.75), na.rm = T)
    box.param$xleft   = q[1]
    box.param$xright  = q[3]
    box.param$ybottom = ylim[1]
    box.param$ytop    = ylim[2]
    suppressWarnings(do.call("rect", box.param))
    box.param$x       = c(q[2], q[2])
    box.param$y       = ylim
    if (!is.null(box.param$border)) 
      box.param$col = box.param$border
    suppressWarnings(do.call("lines", box.param))
  }
}

server = shinyServer(function(input, output, session) {
  
  output$paramUI = renderUI({
    mmstat.ui.call("param")
  })
  output$goUI = renderUI({
    mmstat.ui.call("go")
  })
  output$speedUI = renderUI({
    mmstat.ui.call("speed")
  })
  output$datasetUI = renderUI({
    mmstat.ui.call("dataset")
  })
  output$cexUI = renderUI({
    mmstat.ui.call("cex")
  })
  
  output$variableUI = renderUI({
    inp = mmstat.getValues(NULL, dataset = input$dataset)
    mmstat.ui.call("variable", 
                   choices = mmstat.getVarNames(inp$dataset, "numeric"))
  })
  
  output$sizeUI = renderUI({
    var = getVar()
    mmstat.ui.call("size", ticks = var$ticks, max = length(var$ticks))
  })
  
  getVar = reactive({
    inp          = mmstat.getValues(NULL, dataset = input$dataset, 
                                  variable = input$variable)
    var          = mmstat.getVar(inp$dataset, inp$variable)
    var$ticks    = mmstat.ticks(var$n, nmin = 30)
    dec          = mmstat.dec(c(var$mean, var$median))
    var$decimal  = dec$decimal
    var[["pos"]] = 2 * (var$mean < var$median)
    param <<- c()
    var
  })
  
  getSize = reactive({
    var = getVar()
    inp = mmstat.getValues(NULL, param = input$param, size = input$size)
    if (inp$param == "MEAN") 
      param <<- var$mean
    if (inp$param == "MEDIAN") 
      param <<- var$median
    if (inp$param == "STDDEV") 
      param <<- var$sd
    if (inp$param == "IQR") 
      param <<- var$iqr
    var$ticks[inp$size]
  })
  
  drawSample = reactive({
    input$go
    inp = mmstat.getValues(NULL, speed = input$speed, param = input$param)
    if (inp$speed > 0) 
      invalidateLater(500/inp$speed, session)
    var = getVar()
    repeat {
      # ensure at least two samples
      size   = getSize()
      index  = sample(var$n, size = size, replace = T)
      sample = var$values[index]
      if (inp$param == "MEAN") 
        param <<- c(param, mean(sample))
      if (inp$param == "MEDIAN") 
        param <<- c(param, median(sample))
      if (inp$param == "STDDEV") 
        param <<- c(param, sd(sample))
      if (inp$param == "IQR") 
        param <<- c(param, IQR(sample))
      if (length(param) > 2) 
        break
    }
    index
  })
  
  
  output$samplePlot = renderPlot({
    mmstat.log(sprintf("samplePlot %s", input$param))
    var = getVar()
    inp = mmstat.getValues(NULL, param = input$param, cex = input$cex)
    drawSample()
    par(mar = c(5, 0, 2, 0))
    hist(param, 
         breaks   = "Scott", 
         freq     = F, 
         axes     = F, 
         xlab     = var$xlab, 
         ylab     = "", 
         main     = sprintf(gettext("Histogram and %s based on %.0f samples of size n=%.0f"), 
                            inp$param, length(param), getSize()), 
         cex.axis = inp$cex, 
         cex.lab  = inp$cex, 
         cex.main = 1.2 * inp$cex, 
         cex.sub  = inp$cex)
    rug(param)
    usr = par("usr")
    mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
    box()
    if (inp$param == "MEAN") {
      lty = "dotted"
      col = mmstat$col[[1]]
      abline(v = var$mean, lwd = 3, lty = lty, col = col)
      text(var$mean, 0.95 * usr[4], pos = 4, sprintf("%.*f", var$dec, var$mean), 
           col = col, cex = inp$cex)
    }
    if (inp$param == "MEDIAN") {
      lty = "dashed"
      col = mmstat$col[[3]]
      abline(v = var$median, lwd = 3, lty = lty, col = col)
      text(var$median, 0.95 * usr[4], pos = 4, sprintf("%.*f", var$dec, var$median), 
           col = col, cex = inp$cex)
    }
    if (inp$param == "STDDEV") {
      lty = "dotted"
      col = mmstat$col[[2]]
      abline(v = var$sd, lwd = 3, lty = lty, col = col)
      text(var$sd, 0.95 * usr[4], pos = 4, sprintf("%.*f", var$dec, var$sd), 
           col = col, cex = inp$cex)
    }
    if (inp$param == "IQR") {
      lty = "dashed"
      col = mmstat$col[[4]]
      abline(v = diff(var$quart), lwd = 3, lty = lty, col = col)
      text(diff(var$quart), 0.95 * usr[4], pos = 4, 
           sprintf("%.*f", var$dec, diff(var$quart)), 
           col = col, cex = inp$cex)
    }
  })
  
  output$outputSamplePlot = renderPlot({
    var = getVar()
    index = drawSample()
    inp = mmstat.getValues(NULL, cex = input$cex, param = input$param)
    par(mar = c(5, 0, 2, 0))
    plot(range(var$values), c(-0.05, 1), 
         type     = "n", 
         axes     = F, 
         main     = gettext("Population and sample"), 
         xlab     = var$xlab, sub = var$sub, cex.axis = inp$cex, 
         cex.lab  = inp$cex, 
         cex.main = 1.2 * inp$cex, 
         cex.sub  = inp$cex)
    usr = par("usr")
    mmstat.axis(1, usr[1:2], cex.axis = inp$cex)
    drawIqrBoxWithPoints(var$values, var$jitter, 
                         ylim         = c(0, 0.45), 
                         box.param    = list(border = mmstat$col[[1]], lwd = 2), 
                         points.param = list(col = mmstat$col[[9]], 
                         pch          = 19,
                         cex          = 0.5 * inp$cex))
    
    drawIqrBoxWithPoints(var$values[index], var$jitter[index], 
                         ylim         = 0.5 + c(0, 0.45 * sqrt(length(index)/var$n)), 
                         box.param    = list(border = mmstat$col[[2]], lwd = 2), 
                         points.param = list(col = mmstat$col[[10]], 
                         pch          = 19, 
                         cex          = 0.5 * inp$cex))
    box()
    
  })
  
  output$logText = renderText({
    mmstat.getLog(session)
  })
})

############################### SUBROUTINES ##################################
### ui #######################################################################

ui = shinyUI(fluidPage(
  
  div(class="navbar navbar-static-top",
      div(class = "navbar-inner", 
          fluidRow(column(4, div(class = "brand pull-left", 
                                 gettext("Distribution of sample parameters"))),
                   column(2, checkboxInput("showsample", 
                                           gettext("Sample parameter"), 
                                           TRUE)),
                   column(2, checkboxInput("showspeed", 
                                           gettext("Specify speed"), 
                                           FALSE)),
                   column(2, checkboxInput("showdata", 
                                           gettext("Data choice"), 
                                           FALSE)),
                   column(2, checkboxInput("showoptions", 
                                           gettext("Options"), 
                                           FALSE))))),
    
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = 'input.showsample',
        uiOutput("paramUI"),
        br(),
        uiOutput("sizeUI"),
        br(),
        uiOutput("goUI")
      ),
      conditionalPanel(
        condition = 'input.showspeed',
        br(),
        uiOutput("speedUI")
      ),
      conditionalPanel(
        condition = 'input.showdata',  
        hr(),
        uiOutput("datasetUI"),
        uiOutput("variableUI")
      ),
      conditionalPanel(
        condition = 'input.showoptions',
        hr(),
        uiOutput("cexUI")
      )
    ),
    
      mainPanel(plotOutput("samplePlot"),
                plotOutput("outputSamplePlot", height = "200px"))),

      htmlOutput("logText")
))
  
############################### SUBROUTINES ##################################
### shinyApp #################################################################

shinyApp(ui = ui, server = server)

```
