library(shiny)
library(dplyr)


dd2 <- read.csv2("Dekompo_values_all.csv")
dd2$Region <- ifelse(dd2$Country=="DE",
                     paste("DE", dd2$Region, sep=""),
                     dd2$Region)

men <- arrange(filter(dd2, Sex=="Men"), Country, Region)
women <- arrange(filter(dd2, Sex=="Women"), Country, Region)

###
ui <- fluidPage(
  tags$head(tags$style(".rightAlign{float:right;}")),
  titlePanel("Change in sex gap between 1996-1998 and 2017-2019"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "Region", 
        label = "Select region", 
        choices = unique(dd2$Region), 
        selected = "AT11",
        multiple = FALSE
      ),
      p("Source: Data obtained from statistical offices (own calculations)."),
      p("Description: The barplot depicts the age- and cause-specific
        contributions to the change in the sex gap in life expectancy at birth.
        Summing the contributions gives the total change in the sex gap.
        The total change in the sex gap for each region is shown in the map tab.")
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Barplot, Decomposition", plotOutput(outputId = "p")),
                  tabPanel("Map, Change in sex gap",  tags$image(src="Map_Change_Sex_Gap.png",
                                              height=600, width=600))
      ))
  ))


## function for server
server <- function(input, output, ...) {
  output$p <- renderPlot({
    region.dd.m <- as.matrix(filter(men, Region==input$Region)[,c("Cancer",
                                                                  "CVD",
                                                                  "External",
                                                                  "LungC",
                                                                  "Rest")])
    region.dd.f <- as.matrix(filter(women, Region==input$Region)[,c("Cancer",
                                                                    "CVD",
                                                                    "External",
                                                                    "LungC",
                                                                    "Rest")])
    
    max.men <- max(region.dd.m)
    min.men <- min(region.dd.m)
    
    max.women <- max(region.dd.f)
    min.women <- min(region.dd.f)
    
    highest <- ceiling(
      max(c(abs(max.men), 
            abs(min.men),
            abs(max.women),
            abs(max.women)))
    )
    if (highest==2) {
      y.value <- -3
    }
    if (highest==3) {
      y.value <- -4.5
    }
    if (!(highest %in% c(2,3))) {
      y.value <- -highest-1
    }
    labs <- rep(c(unique(dd2$Age)), 5)
    Gap.change <- sum(region.dd.m)+sum(region.dd.f)
    na.matrix <- matrix(NA, nrow=nrow(region.dd.f), ncol=ncol(region.dd.f))
    
    par(mar=c(6,4,2,1))
    b <- barplot(na.matrix, ylim=c(-highest, highest),
                 beside=TRUE,names.arg=labs, las=2,
                 ylab="Contribution to change in sex gap"
    )
    barplot(region.dd.m, beside=TRUE, add=TRUE, col=adjustcolor("skyblue", alpha.f = 0.5),
            names.arg=labs, las=2)
    barplot(region.dd.f, beside=TRUE, add=TRUE, col=adjustcolor("orangered", alpha.f = 0.5),
            names.arg=labs, las=2)
    legend("topright", legend=c("Women", "Men"),
           fill= c("orangered", "skyblue"), bty="n")
    text(x=b[2,4],-highest+highest/5, labels=c(paste("Total change in sex gap: ",
                                                     round(Gap.change,2)),
                                               sep=""), xpd=NA)
    text(x=b[2,],y=y.value, labels=c("Cancer", "CVD", "External",
                                     "Lung C.", "Rest"), xpd=NA)
    title(paste("Age- & cause-specific decomposition, ", input$Region, sep=""))
  })
  }

shinyApp(ui = ui, server = server)