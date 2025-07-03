### ---------------------------------------------------------------- ###
### TORS viewer
###
### An interactive interface for the UoB Total Ozone Reactivity System
### (TORS) instrument
###
### ==> to run the application, launch R and execute the command:
###       runApp("app.R")
###
### version 0.9.3, June 2024
### author: RS
### ---------------------------------------------------------------- ###

library(shiny)
library(shinythemes)

#options(browser="firefox")

## ------------------------------------------------------------------ ##
## UI section

ui <- fluidPage(theme=shinytheme("paper"),

  ## ------------------------------------------ ##
  ## title
  titlePanel("TORS viewer [v0.9.3]"),
  hr(style="border-color:black; border-width:2px;"),

  ## ------------------------------------------ ##
  ## tabset layout
  tabsetPanel(

    ## ------------------ ##
    ## first panel [data variables]
    tabPanel("Data", fluid=TRUE, br(),
      sidebarLayout(position="right",
        ## data side window
        sidebarPanel(width=3,
          numericInput(inputId="data.hrs",
                       label="hours to display:",
                       min=1,
                       max=72,
                       step=1,
                       value=6),
          textInput(inputId="react.min",
                    label="minimum reactivity to display:",
                    value=0),
          textInput(inputId="react.max",
                    label="maximum reactivity to display:",
                    value=1e-3),
          sliderInput(inputId="o3.range",
                      label="ozone range:",
                      min=0,
                      max=300,
                      step=5,
                      value=c(0,200)),
          hr(style="border-color:black; border-width:2px;"),
          h5("ozone reactivity (s-1):"),
          h6(textOutput("react.calc")),
          h6("equivalent mixing ratio of:"),
          radioButtons(inputId="spec.var",
                       label="",
                       choices=c("NO",
                                 "isoprene",
                                 "a-pinene",
                                 "b-pinene",
                                 "limonene",
                                 "b-caryophyllene"),
                       selected="a-pinene",
                       inline=TRUE),
          h6(textOutput("spec.mr"))
        ),
        ## data plot window
        mainPanel(width=9,
          plotOutput(outputId="dataPlot")
        )
      )
    ),  # --- end first panel

    ## ------------------ ##
    ## second panel [diagnostic variables]
    tabPanel("Diagnostic", fluid=TRUE, br(),
      sidebarLayout(position="right",
        ## diagnostic side window
        sidebarPanel(width=3,
          numericInput(inputId="diagn.hrs",
                       label="hours to display:",
                       min=1,
                       max=72,
                       value=6),
          radioButtons(inputId="diagn.var",
                       label="diagnostic variables:",
                       choices=c("INTENSITY",
                                 "NOISE",
                                 "FLOW",
                                 "PRESSURE"),
                       selected="INTENSITY",
                       inline=FALSE),
        ),
        ## diagnostic plot window
        mainPanel(width=9,
          plotOutput(outputId="diagnostPlot")
        )
      )
    ),  # --- end second panel

    ## ------------------ ##
    ## third panel [system variables]
    tabPanel("System", fluid=TRUE, br(),
      h5("mass flow controllers (slpm):"),
      ## column one [mass flow controllers]
      column(width=6,
        fluidRow(
          column(width=3,
            numericInput(inputId="mfc1.set",
                         label="O3 lamp",
                         min=0,
                         max=0.5,
                         step=0.01,
                         value=0.5)),
          column(width=3,
            h6(textOutput("mfc1.read")))
        ),
        fluidRow(
          column(width=3,
            numericInput(inputId="mfc2.set",
                         label="ZA dilution",
                         min=0,
                         max=2,
                         step=0.01,
                         value=1.2)),
          column(width=3,
            h6(textOutput("mfc2.read")))
        ),
      fluidRow(
        column(width=3,
          numericInput(inputId="mfc3.set",
                       label="OH scrubber",
                       min=0,
                       max=0.5,
                       step=0.01,
                       value=0)),
        column(width=3,
          h6(textOutput("mfc3.read")))
      ),
      fluidRow(
        column(width=3,
          numericInput(inputId="mfc4.set",
                       label="background",
                       min=0,
                       max=5,
                       step=0.01,
                       value=5)),
        column(width=3,
          h6(textOutput("mfc4.read")))
      ),
      fluidRow(
        column(width=3,
          numericInput(inputId="mfc5.set",
                       label="pump",
                       min=0,
                       max=5,
                       step=0.01,
                       value=1)),
        column(width=3,
          h6(textOutput("mfc5.read")))
      )
    ),
    ## column two [instrument flows]
    column(width=6,
      fluidRow(
        h5("reactor (slpm):"),
        h6(textOutput("reactor.flow")),
        h5("sample (slpm):"),
        h6(textOutput("sample.flow")),
        h5("vent (slpm):"),
        h6(textOutput("vent.flow")),
        hr(),
        h5("residence time (s):"),
        h6(textOutput("tau.sec")))
      )
    ),  # --- end third panel

    ## ------------------ ##
    ## fourth panel [configuration info]
    tabPanel("Configuration", fluid=TRUE, br(),
      fluidRow(
        column(width=6,
          textInput(inputId="data.dir",
                    label="data directory:",
                    value="~/code/tors-viewer/"),
          textInput(inputId="expt.dir",
                    label="experiment directory:",
                    value="logfiles/"),
          textInput(inputId="monit1",
                    label="BOX1 logfile:",
                    value="teraterm1_181201"),
          textInput(inputId="monit2",
                    label="BOX2 logfile:",
                    value="teraterm2_181201")),
        column(width=6,
          numericInput(inputId="temp.c",
                       label="reactor temperature (C):",
                       step=1,
                       value=25),
          numericInput(inputId="pres.mbar",
                       label="reactor pressure (mbar):",
                       step=1,
                       value=1013),
          numericInput(inputId="inlet1",
                       label="BOX1 inlet flow (slpm):",
                       step=0.1,
                       value=0.9),
          numericInput(inputId="inlet2",
                       label="BOX2 inlet flow (slpm):",
                       step=0.1,
                       value=0.9))
      )
    )   # --- end fourth panel

  )   # --- end tabsetPanel

)   # --- end UI section --- #

## ------------------------------------------------------------------ ##
## SERVER section

server <- function(input, output, session) {

  ## ---------------------------------------- ##
  ## ozone measurements and reactivity
  df.data <- reactive({
    invalidateLater(60000, session)  # update every 60 seconds
    ## read data files -->> Thermo 491 monitors, logging via teraterm
    box1 <- fRead_Thermo(paste0(input$data.dir, input$expt.dir), input$monit1, "49i")
    box2 <- fRead_Thermo(paste0(input$data.dir, input$expt.dir), input$monit2, "49i")
    df.box <- merge(box1, box2, by="Datetime", suffixes=c("_1","_2"))
    ## calculate ozone reactivity
    df.box$delta <- df.box$o3_1 - df.box$o3_2  # difference between box1 and box2
    df.box$ratio <- df.box$o3_2 / df.box$o3_1  # ratio between box2 and box1
    df.box$reactivity <- -log(df.box$ratio) / df.flows()$tau
    ## temperature and pressure in standard units
    temp.k <- fConvTemp(input$temp.c, "C", "K")
    pres.pa <- fConvPress(input$pres.mbar, "mbar", "Pa")
    ## rate coefficients of ozonolysis reactions
    switch(input$spec.var,
           "NO" = {
             rate.coeff <- fKBi(2.07e-12, -1400, temp.k)[[1]]
           },
           "isoprene" = {
             rate.coeff <- fKBi(1.05e-14, -2000, temp.k)[[1]]
           },
           "a-pinene" = {
             rate.coeff <- fKBi(8.22e-16, -640, temp.k)[[1]]
           },
           "b-pinene" = {
             rate.coeff <- fKBi(1.39-15, -1280, temp.k)[[1]]
           },
           "limonene" = {
             rate.coeff <- fKBi(2.91e-15, -770, temp.k)[[1]]
           },
           "b-caryophyllene" = {
             rate.coeff <- 1.2e-14
           })
    ## equivalent mixing ratios of selected species
    df.box$species <- df.box$reactivity/rate.coeff
    df.box$species.ppb <- fConcGas(df.box$species, "ND", "ppb", temp.k, pres.pa)
    return(df.box)
  })

  ## average ozone reactivity (5 minutes)
  output$react.calc <- renderText({
    az <- nrow(df.data())
    aa <- az - 5
    aa <- ifelse(aa > 0, aa, 1)
    react.5m <- mean(df.data()$reactivity[aa:az], na.rm=TRUE)
    format(react.5m, digits=4, scientific=TRUE)
  })

  ## average equivalent mixing ratio (5 minutes)
  output$spec.mr <- renderText({
    az <- nrow(df.data())
    aa <- az - 5
    aa <- ifelse(aa > 0, aa, 1)
    spec.mr <- mean(df.data()$species.ppb[aa:az], na.rm=TRUE)
    paste(format(spec.mr, digits=4, scientific=FALSE), "ppb")
  })

  ## ---------------------------------------- ##
  ## data plots (first panel)
  output$dataPlot <- renderPlot({
    ## x-axis
    xt <- df.data()$Datetime
    az <- nrow(df.data())
    aa <- az - (input$data.hrs * 60)
    aa <- ifelse(aa>0, aa, 1)
    ## y-axis
    y1 <- df.data()$o3_1
    y2 <- df.data()$o3_2
    y3 <- df.data()$reactivity
    y4 <- df.data()$ratio
    y5 <- df.data()$delta
    ## make plot
    par(mfrow=c(4,1))
    plot(xt[aa:az], y3[aa:az], type="b", col="darkgreen",
         ylim=c(as.numeric(input$react.min),as.numeric(input$react.max)),
         cex=1.5, cex.main=2.5, cex.axis=1.5,
         main=expression("O"[3]~"reactivity"), xlab="", ylab=expression("s"^-1))
    grid()
    plot(xt[aa:az], y1[aa:az], type="b", col="darkblue",
         ylim=input$o3.range, cex=1.5, cex.main=2.5, cex.axis=1.5,
         main=expression("O"[3]~"mixing ratio"), xlab="", ylab=expression("ppb"))
    lines(xt, y2, type="b", col="darkred", cex=2)
    grid()
    legend("topleft", c("box1","box2"), col=c("darkblue","darkred"),
           lty=1, pch=1, ncol=2, bg="white", inset=0.03 , cex=2.5)
    plot(xt[aa:az], y4[aa:az], type="b", col="darkorange",
         ylim=c(0.2,1.2), cex=1.5, cex.main=2.5, cex.axis=1.5,
         main=expression("O"[3]~"ratio"), xlab="", ylab=expression("BOX2/BOX1"))
    abline(h=c(0,1), lty=2)
    grid()
    plot(xt[aa:az], y5[aa:az], type="b", col="darkorchid",
         ylim=c(-10,10), cex=1.5, cex.main=2.5, cex.axis=1.5,
         main=expression(Delta*"(O"[3]*")"), xlab="", ylab=expression("BOX1-BOX2 (ppb)"))
    abline(h=0, lty=2)
    grid()
  },
  height=1200, width=900)

  ## ---------------------------------------- ##
  ## diagnostic plots (second panel)
  output$diagnostPlot <- renderPlot({
    ## x-axis
    xt <- df.data()$Datetime
    az <- nrow(df.data())
    aa <- az - (input$diagn.hrs * 60)
    aa <- ifelse(aa > 0, aa, 1)
    ## y-axis
    switch(input$diagn.var,
           "INTENSITY" = {
             y1a <- df.data()$cellai_1
             y1b <- df.data()$cellbi_1
             y2a <- df.data()$cellai_2
             y2b <- df.data()$cellbi_2
             str.a <- "CELL A"
             str.b <- "CELL B"
           },
           "NOISE" = {
             y1a <- df.data()$noisa_1
             y1b <- df.data()$noisb_1
             y2a <- df.data()$noisa_2
             y2b <- df.data()$noisb_2
             str.a <- "CELL A"
             str.b <- "CELL B"
           },
           "FLOW" = {
             y1a <- df.data()$flowa_1
             y1b <- df.data()$flowb_1
             y2a <- df.data()$flowa_2
             y2b <- df.data()$flowb_2
             str.a <- "CELL A"
             str.b <- "CELL B"
           },
           "PRESSURE" = {
             y1a <- df.data()$pres_1
             y1b <- rep(0, az)
             y2a <- df.data()$pres_2
             y2b <- rep(0, az)
             str.a <- "PRESSURE"
             str.b <- ""
           })
    ## make plot
    par(mfrow=c(2,2))
    plot(xt[aa:az], y1a[aa:az], type="b", col="darkblue",
         main=str.a, xlab="", ylab="BOX 1")
    grid()
    plot(xt[aa:az], y1b[aa:az], type="b", col="darkblue",
         main=str.b, xlab="", ylab="BOX 1")
    grid()
    plot(xt[aa:az], y2a[aa:az], type="b", col="darkred",
         main=str.a, xlab="", ylab="BOX 2")
    grid()
    plot(xt[aa:az], y2b[aa:az], type="b", col="darkred",
         main=str.b, xlab="", ylab="BOX 2")
    grid()
  },
  height=900, width=900)

  ## ---------------------------------------- ##
  ## instrument flows and residence time
  df.flows <- reactive({
    ## mass flow controller calibration
    mfc1 <- (input$mfc1.set * 0.952) + 0.0036   # O3 lamp
    mfc2 <- (input$mfc2.set * 0.989) - 0.0421   # ZA dilution
    mfc3 <- (input$mfc3.set * 1.0134) - 0.0024  # OH scrubber
    mfc4 <- (input$mfc4.set * 0.869) - 0.0539   # background
    mfc5 <- (input$mfc5.set * 0.7188) + 0.0227  # pump
    ## reactor, sample, vent flows
    reactor <- (mfc5 + input$inlet1 + input$inlet2) - (mfc1 + mfc2 + mfc3)
    sample <- input$inlet2 + mfc5
    vent <- mfc4 - reactor
    ## residence time, with reactor volume = 5990 cm3
    tau <- (5.99 / reactor) * 60
    df.flows <- data.frame(mfc1, mfc2, mfc3, mfc4, mfc5, reactor, sample, vent, tau)
    return(df.flows)
  })

  ## O3 lamp flow
  output$mfc1.read <- renderText({
    mfc1.read <- ifelse(df.flows()$mfc1 < 0, 0, df.flows()$mfc1)
    format(mfc1.read, digits=3, scientific=FALSE)
  })

  ## ZA dilution flow
  output$mfc2.read <- renderText({
    mfc2.read <- ifelse(df.flows()$mfc2 < 0, 0, df.flows()$mfc2)
    format(mfc2.read, digits=3, scientific=FALSE)
  })

  ## OH scrubber flow
  output$mfc3.read <- renderText({
    mfc3.read <- ifelse(df.flows()$mfc3 < 0, 0, df.flows()$mfc3)
    format(mfc3.read, digits=3, scientific=FALSE)
  })

  ## background flow
  output$mfc4.read <- renderText({
    mfc4.read <- ifelse(df.flows()$mfc4 < 0, 0, df.flows()$mfc4)
    format(mfc4.read, digits=3, scientific=FALSE)
  })

  ## pump flow
  output$mfc5.read <- renderText({
    mfc5.read <- ifelse(df.flows()$mfc5 < 0, 0, df.flows()$mfc5)
    format(mfc5.read, digits=3, scientific=FALSE)
  })

  ## reactor flow
  output$reactor.flow <- renderText({
    reactor.flow <- ifelse(df.flows()$reactor < 0, 0, df.flows()$reactor)
    format(reactor.flow, digits=3, scientific=FALSE)
  })

  ## sample (inlet) flow
  output$sample.flow <- renderText({
    sample.flow <- ifelse(df.flows()$sample < 0, 0, df.flows()$sample)
    format(sample.flow, digits=3, scientific=FALSE)
  })

  ## vent (exhaust) flow
  output$vent.flow <- renderText({
    vent.flow <- ifelse(df.flows()$vent < 0, 0, df.flows()$vent)
    format(vent.flow, digits=3, scientific=FALSE)
  })

  ## residence time
  output$tau.sec <- renderText({
    tau.sec <- ifelse(df.flows()$tau < 0, 0, df.flows()$tau)
    format(tau.sec, digits=3, scientific=FALSE)
  })

}   # --- end SERVER section --- #

## ------------------------------------------------------------------ ##
## RUN section

shinyApp(ui=ui, server=server)

## ------------------------------------------------------------------ ##
