# ------------------------------------------------------------------------------
# Name of Quantlet: MMSTATmonty_hall
# ------------------------------------------------------------------------------
# Published in:     MMSTAT
# ------------------------------------------------------------------------------
# Description:      Shows a plot of relative frequency of successful trials under
#                   two scenarios of success probabilities of 1/3 and 2/3. The user
#                   can choose an option of guest points and a type of guest's decisions. 
# ------------------------------------------------------------------------------
# Keywords:         plot, scatterplot, mean, visualization, probability,
#                   estimation, parameter, interactive
# ------------------------------------------------------------------------------
# Usage:            MMSTAThelper_function
# ------------------------------------------------------------------------------
# Output:           Interactive shiny application
# ------------------------------------------------------------------------------
# Example:          Shows a plot of relative frequency of successful trials under
#                   241 times of directions in the scenario of "keep door". It 
#                   converges to 1/3, which is the success probability.              
# ------------------------------------------------------------------------------
# See also:         BCS_Bincdf, SFEclt, MVAcltbern, MMSTAThelper_function,
#                   MMSTATtime_series_1, MMSTATlinreg, MMSTATconfmean, 
#                   MMSTATconfi_sigma, MMSTATassociation
# ------------------------------------------------------------------------------
# Author:           Yafei Xu
# ------------------------------------------------------------------------------


# please use "Esc" key to jump out of the Shiny app
rm(list = ls(all = TRUE))
graphics.off()

# please set working directory setwd('C:/...') 
# setwd('~/...')    # linux/mac os
# setwd('/Users/...') # windows

libraries = c("png")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

source("MMSTAThelper_function.r")

############################### SUBROUTINES ##################################
### server ###################################################################

pdc = gettext(c("to left door", "to middle door", 
                "to right door", "randomly to a door"), 
                "num")
aoc = gettext(c("keep door", "change door"), "num")
mmstat.ui.elem("pointdoor", "radioButtons", 
               selected = 1, 
               label    = gettext("1. Guest points"), 
               choices  = pdc)
mmstat.ui.elem("afteropen", "radioButtons", 
               selected = 1, 
               label    = gettext("3. Guest decides to"), 
               choices  = aoc)
mmstat.ui.elem("go", "actionButton", label = gettext("Make a deal"))
mmstat.ui.elem("speed", "speedSlider")
mmstat.ui.elem("cex", "fontSize")
doors = 3

trial = function(type, point = 4, after = 1) {
  if (bitwAnd(type, 3)) {
    # reset and/or append
    if (bitwAnd(type, 1)) 
      mh$success <<- vector("logical", 0)
      mh$price   <<- sample(doors, 1)
      mh$part    <<- ifelse(point == 4, sample(doors, 1), as.integer(point))
      prob           = rep(1, 3)
      prob[mh$part]  = 0
      prob[mh$price] = 0
    if (mh$price == mh$part) {
      index = which(prob > 0)
      prob[min(index)] = 0.5
      prob[max(index)] = 0.5
    }
    mh$show    <<- sample(doors, 1, prob = prob)
    mh$open    <<- ifelse(after == 1, mh$part, 6 - mh$part - mh$show)
    mh$success <<- c(mh$success, mh$price == mh$open)
  }
  if (bitwAnd(type, 4)) {
    # update
    mh$open <<- ifelse(after == 1, mh$part, 6 - mh$part - mh$show)
    mh$success[length(mh$success)] <<- (mh$price == mh$open)
  }
}

point = function(i) {
  outfile = ifelse(mh$part == i, "PointTo.png", "Empty.png")
}

door = function(i, time) {
  if (time == 1) {
    outfile = ifelse(mh$price == i, "CarClose.png", "GoatClose.png")
  }
  if (time == 2) {
    outfile = ifelse(mh$price == i, "CarClose.png", 
                     ifelse(mh$show == i, c("GoatOpen.png"), "GoatClose.png"))
  }
  if (time == 3) {
    outfile = ifelse(mh$open == i, 
                     ifelse(mh$price == i, "CarOpen.png", c("GoatOpen.png")), 
                     ifelse(mh$show == i, c("GoatOpen.png"), 
                     ifelse(mh$price == i, 
                            "CarClose.png", "GoatClose.png")))
  }
  outfile
}

mh = list(success = vector("logical", 0), 
          price   = 0, 
          part    = 0, 
          show    = 0, 
          open    = 0, 
          time    = 0)

trial(3)

server = shinyServer(function(input, output, session) {
  
  go = observe({    
    inp = mmstat.getValues(NULL, 
                           go        = input$go, 
                           afteropen = isolate(input$afteropen), 
                           pointdoor = isolate(input$pointdoor), 
                           speed     = isolate(input$speed))
    trial(2, inp$pointdoor, inp$afteropen)
    mh$time <<- ifelse(inp$speed > 0, 3, 0)
  })
  
  output$goUI = renderUI({
    mmstat.ui.call("go")
  })
  output$cexUI = renderUI({
    mmstat.ui.call("cex")
  })
  output$pointdoorUI = renderUI({
    mmstat.ui.call("pointdoor")
  })
  output$afteropenUI = renderUI({
    mmstat.ui.call("afteropen")
  })
  output$hostopensUI = renderText({
    gettext("2. Host opens door with a goat")
  })
  output$speedUI = renderUI({
    mmstat.ui.call("speed")
  })
  
  strategy = observe({
    inp = mmstat.getValues(NULL, 
                           afteropen = input$afteropen, 
                           pointdoor = input$pointdoor, 
                           speed     = isolate(input$speed))
    trial(3, inp$pointdoor, inp$afteropen)
    mh$time <<- ifelse(inp$speed > 0, 3, 0)
  })
  
  output$distPlot = renderPlot({
    input = mmstat.getValues(NULL, cex = input$cex)
    invalidateLater(200, session)
    if (length(mh$success) > 1) 
      xlim = c(1, length(mh$success)) else xlim = c(1, 2)
    plot(cumsum(mh$success)/seq(mh$success), 
         xlim     = xlim, 
         ylim     = c(0, 1), 
         pch      = 19, 
         xlab     = gettext("Trial"), 
         ylab     = gettext("Relative frequency of successful trials"), 
         main     = sprintf(gettext("Trial %0.f: %s"), 
                            length(mh$success), 
                            ifelse(mh$price == mh$open, 
                                   gettext("Got price :)"), 
                                   gettext("Got goat :("))), 
         cex.axis = input$cex, 
         cex.lab  = input$cex, 
         cex.main = 1.2 * input$cex, 
         cex.sub  = input$cex)
    abline(h = 1/3, col = "gray20")
    abline(h = 2/3, col = "gray20")
  })
  
  output$myPlot1 = renderPlot({
    if (mh$time < 3) {
      mh$time <<- mh$time + 1
      time = 1000
    } else if (input$speed > 0) {
      trial(2, input$pointdoor, input$afteropen)
      mh$time <<- 3
      time = 1000/input$speed
    } else {
      time = 1000
    }
    invalidateLater(200, session)
    par(mfrow = c(2, 3))
    ###### [1,1]
    ima11 = readPNG(paste("\\www\\", door(1, mh$time), sep = ""))
    plot(5, 5, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 10), ylim = c(0, 10))   
    lim = par()
    rasterImage(ima11, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

  
    ###### [1,2]
    ima12 = readPNG(paste("\\www\\", door(2, mh$time), sep = ""))
    plot(5, 5, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 10), ylim = c(0, 10))
    lim = par()
    rasterImage(ima12, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  
    ###### [1,3]
    ima13 = readPNG(paste("\\www\\", door(3, mh$time), sep = ""))
    plot(5, 5, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 10), ylim = c(0, 10))
    lim = par()
    rasterImage(ima13, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  
    ###### [2,1]
    ima21 = readPNG(paste("\\www\\", point(1), sep = ""))
    plot(5, 5, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 10), ylim = c(0, 10))
    lim = par()
    rasterImage(ima21, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  
    ###### [2,2]
    ima22 = readPNG(paste("\\www\\", point(2), sep = ""))
    plot(5, 5, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 10), ylim = c(0, 10))
    lim = par()
    rasterImage(ima22, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  
    ###### [2,3]
    ima23 = readPNG(paste("\\www\\", point(3), sep = ""))
    plot(5, 5, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 10), ylim = c(0, 10))
    lim = par()
    rasterImage(ima23, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
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
          fluidRow(column(4, div(class = "brand pull-left", gettext("Let's Make a Deal"))),
                   column(2, checkboxInput("showdeal", gettext("Make a deal"), TRUE)),
                   column(2, checkboxInput("showspeed", gettext("Specify speed"), FALSE)),
                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),  
    
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = 'input.showdeal',
        uiOutput("pointdoorUI"),
        br(),
        htmlOutput("hostopensUI"),
        br(),
        uiOutput("afteropenUI"), 
        br(),
        uiOutput("goUI")
      ),
      conditionalPanel(
        condition = 'input.showspeed',
        hr(),
        uiOutput("speedUI")
      ),
      conditionalPanel(
        condition = 'input.showoptions',
        hr(),
        uiOutput("cexUI")
      )
      ),

      mainPanel(
        plotOutput("distPlot"),
    plotOutput("myPlot1")
      )),

    htmlOutput("logText")
    
))  

############################### SUBROUTINES ##################################
### shinyApp #################################################################

shinyApp(ui = ui, server = server)
