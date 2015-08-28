# ------------------------------------------------------------------------------
# Name of Quantlet: MMSTATtest_proportion
# ------------------------------------------------------------------------------
# Published in:     MMSTAT
# ------------------------------------------------------------------------------
# Description:      Shows an interactive interface to show the the rejection area in a test of proportion which
#                   uses a normal approximation.
#                   The user can interactively choose the test type (two sided, less or greater), the hypothetical proportion,
#                   the significance level (in %) and the sample size. Also, a table with summary statistics regarding
#                   the sample, population and the test is given.
#                   The sample can be drawn for variables of the data sets
#                   BOSTON HOUSING and CREDIT.
# ------------------------------------------------------------------------------
# Keywords:         test, plot, scatterplot, normal distribution, normal,
#                   visualization, data visualization, parameter, interactive,
#                   normal approximation, sampling, population, Hypothesis Testing
# ------------------------------------------------------------------------------
# Usage:            MMSTAThelper_function
# ------------------------------------------------------------------------------
# Output:           Interactive shiny application
# ------------------------------------------------------------------------------
# Example:          Shows a two sided test with hypothetical proportion = 0.35,
#                   significance level alpha = 10 % and sample size n = 40.
#                   The variable SAVINGS of the CREDIT data set is selected.           
# ------------------------------------------------------------------------------
# See also:         norm, BCS_NormPdfCdf, SMStestuscomp, hotellingstat,
#                   MMSTATtime_series_1, MMSTATlinreg, MMSTATconfmean, 
#                   MMSTATconfi_sigma, MMSTATassociation, MMSTAThelper_function
# ------------------------------------------------------------------------------
# Author:           Yafei Xu
# ------------------------------------------------------------------------------
# Datafiles:        CREDIT.rds, BOSTONHOUSING.rds
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

mmstat.plotTestRegions = function(crit, xlim, ylim, cex, close = F, col = "black", 
  label = NULL, pos = 1) {
  lines(xlim, c(ylim[1], ylim[1]), col = col)
  lines(xlim, c(ylim[2], ylim[2]), col = col)
  if (close) {
    lines(c(xlim[1], xlim[1]), ylim, col = col)
    lines(c(xlim[2], xlim[2]), ylim, col = col)
  }
  cu = max(crit[1], xlim[1])
  if (crit[1] >= xlim[1]) {
    lines(c(cu, cu), ylim, col = col)
    text((cu + xlim[1])/2, mean(ylim), mmstat.math("\\\"&H[1];\\\""), 
         cex = cex, col = col)
  }
  co = min(crit[2], xlim[2])
  if (crit[2] <= xlim[2]) {
    lines(c(co, co), ylim, col = col)
    text((co + xlim[2])/2, mean(ylim), mmstat.math("\\\"&H[1];\\\""), 
         cex = cex, col = col)
  }
  text((co + cu)/2, mean(ylim), mmstat.math("\\\"&H[0];\\\""), cex = cex, col = col)
  if (!is.null(text)) {
    if (pos == 2) 
      text(xlim[1], mmstat.pos(ylim, -0.25), label, col = col, cex = cex, pos = 4)
    if (pos == 4) 
      text(xlim[2], mmstat.pos(ylim, -0.25), label, col = col, cex = cex, pos = 2)
  }
}


mmstat.ui.elem("test", "radioButtons", 
               label    = gettext("Choose test type"), 
               choices  = gettext(c("two.sided", "less", "greater"), "name"), 
               selected = "two.sided")
mmstat.ui.elem("pi0", "sliderInput", 
               label = HTML(gettext("Hypothetical proportion (&pi;<sub>0</sub>)")), 
               min   = 0, 
               max   = 1, 
               value = 0.5, 
               step  = 0.05)
mmstat.ui.elem("alpha", "significance")
mmstat.ui.elem("size", "sampleSize")
mmstat.ui.elem("go", "drawSample")
mmstat.ui.elem("dataset", "dataSet", 
               choices = mmstat.getDataNames("CREDIT", "BOSTONHOUSING"))
mmstat.ui.elem("variable", "variable1", vartype = "binary")
mmstat.ui.elem("cex", "fontSize")
mmstat$vartype = "binvars"


drawVariableValues = function(x, jitter    = NULL, ylim, 
                              box.param    = NULL, 
                              points.param = NULL, 
                              param        = NULL) {
  if (is.numeric(x)) {
    if (is.list(points.param) || is.null(points.param) || points.param) {
      points.param$x = x
      points.param$y = ylim[1] + diff(ylim) * jitter
      suppressWarnings(do.call("points", points.param))
    }
    if (is.list(box.param) || is.null(box.param) || box.param) {
      q                 = quantile(x, c(0.25, 0.5, 0.75))
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
  if (is.factor(x)) {
    tab = prop.table(table(x))
    xp = as.numeric(c(0, cumsum(tab)))
    for (i in seq(tab)) {
      args         = param
      args$xleft   = xp[i]
      args$xright  = xp[i + 1]
      args$ybottom = ylim[1]
      args$ytop    = ylim[2]
      args$col     = mmstat$col[[i + param$col]]
      suppressWarnings(do.call("rect", args))
      if (!is.null(param$level) && param$level) {
        args        = param
        args$x      = mean((xp[i:(i + 1)]))
        args$y      = mean(ylim)
        args$labels = gettext(names(tab)[i])
        suppressWarnings(do.call("text", args))
      }
    }
  }
}

server = shinyServer(function(input, output, session) {
  
  output$testUI = renderUI({
    mmstat.ui.call("test")
  })
  output$pi0UI = renderUI({
    mmstat.ui.call("pi0")
  })
  output$alphaUI = renderUI({
    mmstat.ui.call("alpha")
  })
  output$goUI = renderUI({
    mmstat.ui.call("go")
  })
  output$datasetUI = renderUI({
    mmstat.ui.call("dataset")
  })
  output$cexUI = renderUI({
    mmstat.ui.call("cex")
  })
  
  output$variableUI = renderUI({
    mmstat.log("variableUI")
    inp = mmstat.getValues(NULL, dataset = input$dataset)
    mmstat.ui.call("variable", choices = mmstat.getVarNames(inp$dataset, "binary"))
  })
  
  output$sizeUI = renderUI({
    var = getVar()
    mmstat.ui.call("size", ticks = var$ticks, max = length(var$ticks))
  })
  
  getVar = reactive({
    mmstat.log("getVar")
    var         = mmstat.getVar(isolate(input$dataset), input$variable)
    var$ticks   = mmstat.ticks(var$n, nmin = 40)
    var$decimal = 2
    var
  })
  
  drawSample = reactive({
    mmstat.log("drawSample")
    input$go
    inp    = mmstat.getValues(NULL, size = input$size)
    var    = getVar()
    index  = sample(var$n, var$ticks[inp$size], replace = T)
    values = var$values[index]
    tab    = table(values)
    sample = list(values = values, 
    n      = length(values), 
    tab    = tab, 
    prop   = prop.table(tab))
    sample
  })
  
  doTest = reactive({
    var    = getVar()
    sample = drawSample()
    inp    = mmstat.getValues(NULL, 
                              pi0   = input$pi0, 
                              test  = input$test, 
                              alpha = input$alpha)
    test   = list(pi0          = inp$pi0, 
                  statistic    = sample$prop[1], 
                  cond         = sample$n * inp$pi0 * (1 - inp$pi0), 
                  var          = inp$pi0 * (1 - inp$pi0)/sample$n, 
                  alpha        = inp$alpha, 
                  distribution = mmstat.math(" &hat(Pi)%~~%N(pi[0], pi[0](1-pi[0])/n); "))
    if (inp$test == "two.sided") {
      test$h0.html    = "&pi;=&pi;<sub>0</sub>"
      test$h1.html    = "&pi;&ne;&pi;<sub>0</sub>"
      test$hypotheses = mmstat.math("&H[0];: &pi==pi[0]; vs. &H[1];: &pi!=pi[0]; ")
      test$cu         = inp$pi0 - qnorm(1 - mmstat$alpha[inp$alpha]/200) * 
                        sqrt(inp$pi0 * (1 - inp$pi0)/sample$n)
      test$co         = inp$pi0 + qnorm(1 - mmstat$alpha[inp$alpha]/200) * 
                        sqrt(inp$pi0 * (1 - inp$pi0)/sample$n)
    }
    if (inp$test == "less") {
      test$h0.html    = "&pi;&ge;&pi;<sub>0</sub>"
      test$h1.html    = "&pi;&lt;&pi;<sub>0</sub>"
      test$hypotheses = mmstat.math("&H[0];: &pi>=pi[0]; vs. &H[1];: &pi<pi[0]; ")
      test$cu         = inp$pi0 - qnorm(1 - mmstat$alpha[inp$alpha]/100) * 
                        sqrt(inp$pi0 * (1 - inp$pi0)/sample$n)
      test$co         = +Inf
    }
    if (inp$test == "greater") {
      test$h0.html    = "&pi;&le;&pi;<sub>0</sub>"
      test$h1.html    = "&pi;&gt;&pi;<sub>0</sub>"
      test$hypotheses = mmstat.math("&H[0];: &pi<=pi[0]; vs. &H[1];: &pi>pi[0]; ")
      test$cu         = -Inf
      test$co         = inp$pi0 + qnorm(1 - mmstat$alpha[inp$alpha]/100) * 
                        sqrt(inp$pi0 * (1 - inp$pi0)/sample$n)
    }
    test$decision  = ifelse((sample$prop[1] < test$cu) || (sample$prop[1] > test$co), 
                            gettext("Reject H<sub>0</sub>"), 
                            gettext("Can not reject H<sub>0</sub>"))
    test$cond.text = ifelse(sample$n * inp$pi0 * (1 - inp$pi0) < 9, 
                            mmstat.math(gettext("Normal approximation of 
                                                test statistics &hat(Pi); 
                                                is NOT applicable!")), "")
    test
  })
  
  readHTML = reactive({
    var    = getVar()
    sample = drawSample()
    test   = doTest()
    inp    = mmstat.getValues(NULL, cex = input$cex)
    np1p   = sample$n * inp$pi0 * (1 - inp$pi0)
    html   = mmstat.html(gettext("distText.html"), 
                         cex       = 100 * inp$cex, 
                         h0        = test$h0.html, 
                         h1        = test$h1.html, 
                         pi0       = test$pi0, 
                         ex        = test$pi0, 
                         varx      = test$var, 
                         cond      = test$cond, 
                         alpha     = mmstat$alpha[test$alpha], 
                         cu        = test$cu, 
                         co        = test$co, 
                         statistic = test$statistic, 
                         decision  = test$decision, 
                         sprop     = sample$prop[1], 
                         ssize     = sample$n, 
                         dataname  = var$dataname, 
                         varname   = var$xlab, 
                         pprop     = var$prop[1], 
                         psize     = var$n)
    html
  })
  
  output$outputPlot = renderPlot({
    mmstat.log("outputPlot")
    var    = getVar()
    sample = drawSample()
    test   = doTest()
    inp    = mmstat.getValues(NULL, 
                              alpha = input$alpha, 
                              pi0   = input$pi0, 
                              cex   = input$cex, 
                              test  = input$test)
    xi     = (0:100)/100
    sd     = sqrt(inp$pi0 * (1 - inp$pi0)/sample$n)
    yi     = dnorm(xi, mean = inp$pi0, sd = sd)
    ya     = 0.4 * max(yi)
    ylim   = c(-ya, max(yi))
    xlim   = c(0, 1)
    par(mar = c(5, 0, 2, 0))
    plot(xi, yi, 
         type     = "l", 
         xlab     = mmstat.math(" &pi; "), 
         ylab     = "", 
         ylim     = ylim, 
         xlim     = xlim, 
         axes     = "F", 
         cex.axis = inp$cex, 
         cex.lab  = inp$cex, 
         cex.main = 1.2 * inp$cex, 
         cex.sub  = inp$cex)
    title(main    = test$hypotheses, 
          sub     = test$cond.text, 
          col.sub = "red", 
          cex     = inp$cex)
    mmstat.axis(1, c(0, 1))
    # mmstat.axis(2, c(0,max(yi)))
    box()
    usr = par("usr")
    xt  = qnorm(ifelse(inp$pi0 <= 0.5, 0.7, 0.3), mean = inp$pi0, sd = sd)
    yt  = dnorm(xt, mean = inp$pi0, sd = sd)
    text(xt, yt, test$distribution, pos = 4, cex = inp$cex)
    mmstat.plotTestRegions(c(test$cu, test$co), 
                           xlim  = c(0, 1), 
                           ylim  = -ya * c(0.05, 0.4), 
                           cex   = inp$cex, 
                           close = T)
    drawVariableValues(as.factor(var$values), 
                       ylim  = -ya * c(0.75, 1), 
                       param = list(cex = inp$cex, col = 0, border = NA, level = T))
    drawVariableValues(as.factor(sample$values), 
                       ylim  = -ya * c(0.45, 0.45 + 0.25 * sqrt(sample$n/var$n)), 
                       param = list(cex = inp$cex, col = 8, border = NA, level = F))
    abline(v = inp$pi0, col = "grey")
    
  })
  
  output$distText = renderText({
    mmstat.log("called 'distText'")
    html = readHTML()
    html
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
          fluidRow(column(4, div(class = "brand pull-left", gettext("Test of proportion"))),
                   column(2, checkboxInput("showtest", gettext("Test parameter"), TRUE)),
                   column(2, checkboxInput("showsample", gettext("Sample parameter"), TRUE)),
                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = 'input.showtest',
        uiOutput("testUI"),
        br(),
        uiOutput("pi0UI"),
        uiOutput("alphaUI")
      ),
      conditionalPanel(
        condition = 'input.showsample',
        hr(),
        uiOutput("sizeUI"),
        br(),
        uiOutput("goUI")
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
    
    mainPanel(plotOutput("outputPlot"),
              HTML('<hr>'),
              htmlOutput("distText"))
  ),
  
  htmlOutput("logText")
  ))
  
############################### SUBROUTINES ##################################
### shinyApp #################################################################

shinyApp(ui = ui, server = server)
