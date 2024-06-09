### ---------------------------------------------------------------- ###
### TORS viewer
###
### --> requires atmosch-R :
###       https://github.com/rs028/atmosch-R/
###
### version 0.9.1, June 2024
### author: RS
### ---------------------------------------------------------------- ###

require(shiny)
require(shinythemes)

#options(browser="firefox")

## ------------------------------------------------------------------ ##
## UI section

ui <- fluidPage(theme=shinytheme("paper"),

                ## ------------------------------------------ ##
                ## title
                titlePanel("TORS viewer"),

                ## ------------------------------------------ ##
                ## tabset layout
                tabsetPanel(

                  ## ------------------ ##
                  ## first panel (main variables)
                  tabPanel("Main", fluid=TRUE,
                           sidebarLayout(position="right",
                                         ## main side window
                                         sidebarPanel(width=3,
                                                      numericInput(inputId="start1",
                                                                   label="hours displayed:",
                                                                   min=1,
                                                                   max=72,
                                                                   step=1,
                                                                   value=6
                                                      ),
                                                      textInput(inputId="min1",
                                                                label="minimum reactivity:",
                                                                value=0
                                                      ),
                                                      textInput(inputId="max1",
                                                                label="maximum reactivity:",
                                                                value=1e-3
                                                      ),
                                                      sliderInput(inputId="range1",
                                                                  label="ozone range:",
                                                                  min=0,
                                                                  max=300,
                                                                  step=5,
                                                                  value=c(0,150)
                                                      ),
                                                      hr(style="border-color:black; border-width:3px;"),
                                                      h4("ozone reactivity (s-1):"),
                                                      h5(textOutput("reac1")),
                                                      h5("equivalent to mixing ratio (ppb):"),
                                                      radioButtons(inputId="spec",
                                                                   label="",
                                                                   choices=c("NO",
                                                                             "isoprene",
                                                                             "a-pinene",
                                                                             "limonene"),
                                                                   selected="a-pinene"
                                                                   ),
                                                      h6(textOutput("spec.mr"))
                                         ),
                                         ## main plot window
                                         mainPanel(width=9,
                                                   plotOutput(outputId="mainPlot")
                                         )
                           )
                  ),

                  ## ------------------ ##
                  ## second panel (diagnostic variables)
                  tabPanel("Diagnostic", fluid=TRUE,
                           sidebarLayout(position="right",
                                         ## diagnostic side window
                                         sidebarPanel(width=3,
                                                      numericInput(inputId="start2",
                                                                   label="hours displayed:",
                                                                   min=1,
                                                                   max=72,
                                                                   value=6
                                                      ),
                                                      radioButtons(inputId="var2",
                                                                   label="diagnostic variables:",
                                                                   choices=c("INTENSITY",
                                                                             "NOISE",
                                                                             "FLOW",
                                                                             "PRESSURE"),
                                                                   selected="INTENSITY"
                                                      )
                                         ),
                                         ## diagnostic plot window
                                         mainPanel(width=9,
                                                   plotOutput(outputId="secondPlot")
                                         )
                           )
                  ),

                  ## ------------------ ##
                  ## third panel (system configuration)
                  tabPanel("Configuration", fluid=TRUE,
                           fluidRow(br(),
                             column(width=6,
                                    textInput(inputId="dir3",
                                              label="data directory",
                                              value="~/TORS/phase4_waseda/"
                                    ),
                                    textInput(inputId="dat3",
                                              label="experiment directory",
                                              value="raw/"
                                    ),
                                    textInput(inputId="one3",
                                              label="box1",
                                              value="teraterm1_181201"
                                    ),
                                    textInput(inputId="two3",
                                              label="box2",
                                              value="teraterm2_181201"
                                    )
                             ),
                             column(width=6,
                                    numericInput(inputId="time3",
                                                 label="residence time (s):",
                                                 step=1,
                                                 value=128
                                    ),
                                    numericInput(inputId="temp3",
                                                 label="temperature (C):",
                                                 step=1,
                                                 value=25
                                    ),
                                    numericInput(inputId="pres3",
                                                 label="pressure (mbar):",
                                                 step=1,
                                                 value=1013
                                    )
                             )
                           )
                  ),

                  ## ------------------ ##
                  ## fourth panel (system variables)
                  tabPanel("System", fluid=TRUE,
                           fluidRow(br(),
                                    column(width=6,
                                    numericInput(inputId="time3",
                                                 label="residence time (s):",
                                                 step=1,
                                                 value=128
                                    ),
                                    numericInput(inputId="temp3",
                                                 label="temperature (C):",
                                                 step=1,
                                                 value=25
                                    ),
                                    numericInput(inputId="pres3",
                                                 label="pressure (mbar):",
                                                 step=1,
                                                 value=1013
                                    )
                             )
                           )
                  )

                )  # --- end tabsetPanel --- #

)  # --- end fluidPage --- #

## ------------------------------------------------------------------ ##
## SERVER section

server <- function(input, output, session) {

  ## ------------------ ##
  ##  (1 minute interval)
  df.data <- reactive({
    invalidateLater(60000, session)
    ## read data files
    box1 <- fRead_Thermo(paste(input$dir3, input$dat3, sep=""), input$one3, "49i")
    box2 <- fRead_Thermo(paste(input$dir3, input$dat3, sep=""), input$two3, "49i")
    df.box <- merge(box1, box2, by="Datetime", suffixes=c("_1","_2"))
    ## calculate ozone reactivity
    df.box$delta <- df.box$o3_1 - df.box$o3_2
    df.box$reactivity <- -log(df.box$o3_2 / df.box$o3_1) / input$time3
    ## calculate temperature and pressure in standard units
    temp.k <- fConvTemp(input$temp3, "C", "K")
    pres.pa <- fConvPress(input$pres3, "mbar", "Pa")
    ## calculate rate coefficient of ozonolysis reaction
    switch(input$spec,
           "NO"={
             rate.coeff <- fKBi(2.07e-12, -1400, temp.k)[[1]]
           },
           "isoprene"={
             rate.coeff <- fKBi(1.05e-14, -2000, temp.k)[[1]]
           },
           "a-pinene"={
             rate.coeff <- fKBi(8.22e-16, -640, temp.k)[[1]]
           },
           "limonene"={
             rate.coeff <- fKBi(2.91e-15, -770, temp.k)[[1]]
           })
    ## calculate equivalent mixing ratio for selected species
    df.box$species <- df.box$reactivity/rate.coeff
    df.box$species.ppb <- fConcGas(df.box$species, "ND", "ppb", temp.k, pres.pa)
    return(df.box)
  })

  ## ------------------ ##
  ## calculate average ozone reactivity (5 minutes)
  output$reac1 <- renderText({
    az <- nrow(df.data())
    aa <- az - 5
    aa <- ifelse(aa>0, aa, 1)
    react.5m <- mean(df.data()$reactivity[aa:az], na.rm=TRUE)
    format(react.5m, digits=4, scientific=TRUE)
  })

  ## ------------------ ##
  ## calculate average equivalent mixing ratio (5 minutes)
  output$spec.mr <- renderText({
    az <- nrow(df.data())
    aa <- az - 5
    aa <- ifelse(aa>0, aa, 1)
    spec.mr <- mean(df.data()$species.ppb[aa:az], na.rm=TRUE)
    format(no.ppb, digits=4, scientific=FALSE)
  })

  ## ------------------ ##
  ## primary panel plots (main variables)
  output$mainPlot <- renderPlot({
    ## x-axis
    xt <- df.data()$Datetime
    az <- nrow(df.data())
    aa <- az - (input$start1 * 60)
    aa <- ifelse(aa>0, aa, 1)
    ## y-axis
    y1 <- df.data()$o3_1
    y2 <- df.data()$o3_2
    y3 <- df.data()$reactivity
    y4 <- df.data()$delta
    ## plot main variables
    par(mfrow=c(3,1))
    plot(xt[aa:az], y3[aa:az], type="b", col="darkgreen",
         ylim=c(as.numeric(input$min1),as.numeric(input$max1)),
         cex=1.5, cex.main=2.5, cex.axis=1.5,
         main=expression("O"[3]~"reactivity (s"^-1*")"), xlab="", ylab="")
    grid(); abline(h=0, lty=2)
    plot(xt[aa:az], y1[aa:az], type="b", col="darkblue",
         ylim=input$range1, cex=1.5, cex.main=2.5, cex.axis=1.5,
         main=expression("O"[3]~"concentration (ppb)"), xlab="", ylab="")
    lines(xt, y2, type="b", col="darkred", cex=2)
    grid(); abline(h=0, lty=2)
    legend("topleft", c("box1","box2"), col=c("darkblue","darkred"),
           lty=1, pch=1, ncol=2, bg="white", inset=0.03 , cex=2.5)
    plot(xt[aa:az], y4[aa:az], type="b", col="darkorchid",
         ylim=c(-10,10), cex=1.5, cex.main=2.5, cex.axis=1.5,
         main=expression(Delta*"(O"[3]*")"), xlab="", ylab="")
    grid(); abline(h=0, lty=2)
  }, height=800, width=900)

  ## ------------------ ##
  ## secondary panel plots (diagnostic variables)
  output$secondPlot <- renderPlot({
    ## x-axis
    xt <- df.data()$Datetime
    az <- nrow(df.data())
    aa <- az - (input$start2 * 60)
    aa <- ifelse(aa>0, aa, 1)
    ## y-axis
    switch(input$var2,
           "INTENSITY"={
             y1a <- df.data()$cellai_1
             y1b <- df.data()$cellbi_1
             y2a <- df.data()$cellai_2
             y2b <- df.data()$cellbi_2
             str.a <- "CELL A"
             str.b <- "CELL B"
           },
           "NOISE"={
             y1a <- df.data()$noisa_1
             y1b <- df.data()$noisb_1
             y2a <- df.data()$noisa_2
             y2b <- df.data()$noisb_2
             str.a <- "CELL A"
             str.b <- "CELL B"
           },
           "FLOW"={
             y1a <- df.data()$flowa_1
             y1b <- df.data()$flowb_1
             y2a <- df.data()$flowa_2
             y2b <- df.data()$flowb_2
             str.a <- "CELL A"
             str.b <- "CELL B"
           },
           "PRESSURE"={
             y1a <- df.data()$pres_1
             y1b <- rep(0, az)
             y2a <- df.data()$pres_2
             y2b <- rep(0, az)
             str.a <- "PRESSURE"
             str.b <- ""
           })
    ## plot diagnostic variables
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
  }, height=800, width=900)

}  # --- end server --- #

## ------------------------------------------------------------------ ##
## RUN app

shinyApp(ui=ui, server=server)

## ------------------------------------------------------------------ ##
