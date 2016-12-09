
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **MMSTATdice_game** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : MMSTATdice_game

Published in : MMSTAT

Description : 'Shows the conditional probability that the dice is either fair or loaded given the
number of rolled sixes (X) in the upper panel. The lower panel shows a bar plot of the conditional
probability to roll a specific number of sixes given a fair/loaded dice. The user can interactively
choose (1) the number of rolls, (2) the number of rolled sixes for the upper panel and (3) the
probability to roll a six with the loaded dice.'

Keywords : 'conditional distribution, plot, data visualization, visualization, cdf, interactive,
estimation, parameter, parametric'

See also : 'BCS_Hist1, BCS_Hist2, MVAcondnorm, COPdaxnormhist MMSTATtime_series_1, MMSTATlinreg,
MMSTATconfmean, MMSTATconfi_sigma, MMSTATassociation, MMSTAThelper_function'

Author : Sigbert Klinke

Code Editor : Yafei Xu

Input : MMSTAThelper_function

Output : Interactive shiny application

Example : 'Sets the number of rolls equal to 20, the number of sixes is set to 1 and the
probability for six with loaded dice is set to 0.66.'

```

![Picture1](MMSTATdice_game.png)


### R Code:
```r
# ------------------------------------------------------------------------------
# Name of Quantlet: MMSTATdice_game
# ------------------------------------------------------------------------------
# Published in:     MMSTAT
# ------------------------------------------------------------------------------
# Description:      Shows the conditional probability that the dice is either fair or loaded
#                   given the number of rolled sixes (X) in the upper panel.
#                   The lower panel shows a bar plot of the conditional probability to roll
#                   a specific number of sixes given a fair/loaded dice.
#                   The user can interactively choose (1) the number of rolls,
#                   (2) the number of rolled sixes for the upper panel and (3) the probability
#                   to roll a six with the loaded dice.
# ------------------------------------------------------------------------------
# Keywords:         conditional distribution, plot, data visualization, 
#                   visualization, cdf, interactive, estimation, parameter, 
#                   parametric  
# ------------------------------------------------------------------------------
# Usage:            MMSTAThelper_function
# ------------------------------------------------------------------------------
# Output:           Interactive shiny application
# ------------------------------------------------------------------------------
# Example:          Sets the number of rolls equal to 20, the number of sixes is set to 1
#                   and the probability for six with loaded dice is set to 0.66.          
# ------------------------------------------------------------------------------
# See also:         BCS_Hist1, BCS_Hist2, MVAcondnorm, COPdaxnormhist
#                   MMSTATtime_series_1, MMSTATlinreg, MMSTATconfmean, 
#                   MMSTATconfi_sigma, MMSTATassociation, MMSTAThelper_function
# ------------------------------------------------------------------------------
# Author :          Sigbert Klinke
# ------------------------------------------------------------------------------
# Code Editor:      Yafei Xu
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

mmstat.ui.elem('rolls', 'sliderInput', 
               label = gettext("Number of rolls:"),
               min   = 1, 
               max   = 20, 
               value = 3)
mmstat.ui.elem("prob", 'sliderInput', 
               label = gettext("Probability for six with loaded dice:"),
               min   = 0, 
               max   = 1, 
               step  = .16666, 
               value = .16666*2)
mmstat.ui.elem("cex", 'fontSize')
mmstat.ui.elem("sixes", 'sliderInput', 
               label = gettext("Number of sixes (X):"),
               min   = 0, 
               max   = 3, 
               value = 1)

ddbinom = function (x, size, prob) {
  if (prob <= 0) {
    return (as.numeric(x == 0))
  }
  if (prob >= 1) {
    return (as.numeric(x == size))
  }
  dbinom(x, size, prob)
}

server = shinyServer(function(input, output, session) {
  
  output$rollsUI  = renderUI({ mmstat.ui.call('rolls') })
  output$probUI   = renderUI({ mmstat.ui.call('prob') })
  output$cexUI    = renderUI({ mmstat.ui.call('cex') })
  output$sixesUI  = renderUI({ mmstat.ui.call('sixes') })
  output$distPlot = renderPlot({
    inp = mmstat.getValues(NULL, 
                           cex   = input$cex,
                           prob  = input$prob,
                           rolls = input$rolls)
    t   = 0:inp$rolls
    w0  = dbinom(t, inp$rolls, 1/6)
    w1  = ddbinom(t, inp$rolls, inp$prob)
    mp  = barplot(rbind(w1,w0), 
                  main     = gettext("P(Number of sixes | Dice type)"), 
                  ylim     = c(0,1), 
                  axes     = F, 
                  col      = c(mmstat$col[[2]], mmstat$col[[1]]), 
                  beside   = T, 
                  cex.axis = inp$cex, 
                  cex.lab  = inp$cex, 
                  cex.main = 1.2*inp$cex, 
                  cex.sub  = inp$cex)
    legend("topright", gettext(c("loaded dice (W=1)", "fair dice (W=0)")), 
           cex  = inp$cex, 
           fill = c(mmstat$col[[2]], 
           mmstat$col[[1]]),)
    mp  = colMeans(mp)
    axis(1, at = mp, labels=sprintf("%.0f", t), cex.axis = inp$cex)
    axis(2, at = (0:5)/5, labels = sprintf("%.1f", (0:5)/5), cex.axis = inp$cex)
    box()
  })

  output$formula = renderText({
    inp = mmstat.getValues(NULL, 
                           cex   = input$cex,
                           prob  = input$prob,
                           sixes = input$sixes,
                           rolls = input$rolls)
    t  = 0:inp$rolls
    w0 = dbinom(t, inp$rolls, 1/6)
    w1 = ddbinom(t, inp$rolls, inp$prob)
    p1 = w1[1 + inp$sixes] / (w1[1 + inp$sixes] + w0[1 + inp$sixes])
    p0 = w0[1 + inp$sixes] / (w1[1 + inp$sixes] + w0[1 + inp$sixes])
    
    paste0(sprintf('<table style="font-size:%.0f%%"><tr style="color:%s"><td>', 
                   90*inp$cex, mmstat$col[[2]]),
           sprintf('P(W=1|X=%.0f) = ', inp$sixes),
           '</td><td align = "center">',
           sprintf(' (P(X=%.0f|W=1)*P(W=1)) / (P(X=%.0f|W=0)*P(W=0)+P(X=%.0f|W=1)*P(W=1))', 
                   inp$sixes, inp$sixes, inp$sixes),
           '</td><td> = </td><td align = "center">',
           sprintf(' (%.3f*0.5) / (%.3f*0.5+%.3f*0.5)', 
                   w1[1+inp$sixes], w0[1+inp$sixes], w1[1+inp$sixes]),
           '</td><td> = </td><td align = "center">',
           sprintf(' %0.3f', p1),
           '</td></tr><tr><td><br><br></td></tr>',
           sprintf('<tr style="color:%s"><td>', mmstat$col[[1]]),
           sprintf('P(W=0|X=%.0f) = ', inp$sixes),
           '</td><td align="center">',
           sprintf(' (P(X=%.0f|W=0)*P(W=0)) / (P(X=%.0f|W=0)*P(W=0)+P(X=%.0f|W=1)*P(W=1))', 
                   inp$sixes, inp$sixes, inp$sixes),
           '</td><td> = </td><td align="center">',
           sprintf(' (%.3f*0.5) / (%.3f*0.5+%.3f*0.5)', 
                   w0[1+inp$sixes], w0[1+inp$sixes], w1[1+inp$sixes]),
           '</td><td> = </td><td align="center">',
           sprintf(' %0.3f', p0),
           '</td></tr><tr><td><br><br></td></tr></table>')
  })
  
  output$logText = renderText({
    mmstat.getLog(session)
  })
})

############################### SUBROUTINES ##################################
### ui #######################################################################

ui = shinyUI(fluidPage(
  div(class = "navbar navbar-static-top",
      div(class = "navbar-inner", 
          fluidRow(column(4, div(class = "brand pull-left", 
                                gettext("Dice rolling"))),
                   column(2, checkboxInput("showgame", 
                                           gettext("Game parameter"), TRUE)),
                   column(2, checkboxInput("showprob", 
                                           gettext("Probability"), TRUE)),
                   column(2, checkboxInput("showoptions", 
                                           gettext("Options"), FALSE))))),
    
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = 'input.showgame',
          uiOutput("rollsUI"),
          br(),
          uiOutput("sixesUI")
          ),
        conditionalPanel(
          condition = 'input.showprob',
          br(),
          uiOutput("probUI")
          ),
        conditionalPanel(
          condition = 'input.showoptions',
          hr(),
          uiOutput("cexUI")
          )
        ),
    
      mainPanel(htmlOutput("formula"),
                plotOutput("distPlot"))),

    htmlOutput("logText")
))

############################### SUBROUTINES ##################################
### shinyApp #################################################################

shinyApp(ui = ui, server = server)

```
