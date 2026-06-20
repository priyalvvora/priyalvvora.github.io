library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(shinythemes)
library(ggforce)
library(tidyverse)
library(lpSolveAPI)
library(lpSolve)
library(shinydashboard)
library(caret)
library(gbm)
library(pROC)
library(shiny)
library(gapminder)


palette(c("#ADF7B6","#FEE3C6","#FFBEC2",
          "#85E3FF","#97A2FF",
          "#C1867B","#A7BEB6","#F8DF81",
          "#FFABAB"))

setwd("./")
my_data <- read_xlsx("Travel_VF.xlsx", sheet=1, col_names=T)


factor_col <- c('CustomerID','ProdTaken','TypeofContact','CityTier','Occupation','Gender','ProductPitched',
                'PreferredPropertyStar','MaritalStatus','Passport','PitchSatisfactionScore','OwnCar','Designation')
my_data[factor_col] <- lapply(my_data[factor_col],factor)

num_col <- my_data[,c('Age','DurationOfPitch','NumberOfPersonVisiting','NumberOfFollowups','NumberOfTrips',
                      'NumberOfChildrenVisiting','MonthlyIncome','ProdTaken_and_PreferredPropertyStar',
                      'ProdTaken_and_MaritalStatus','ProdTaken_and_Designation','CityTier_and_Occupation',
                      'CityTier_and_PreferredPropertyStar','CityTier_and_MaritalStatus','CityTier_and_Designation',
                      'Occupation_and_PreferredPropertyStar','Occupation_and_MaritalStatus','Occupation_and_Designation',
                      'PreferredPropertyStar_and_MaritalStatus','PreferredPropertyStar_and_Passport',
                      'PreferredPropertyStar_and_Designation','MaritalStatus_and_Passport','MaritalStatus_and_Designation',
                      'Passport_and_Designation')]

my_data <- as.data.frame(my_data)

##Replace NAs with mean; Outlier Treatment
my_data$Age [is.na(my_data$Age)] <-mean(my_data$Age, na.rm=TRUE)

my_data$DurationOfPitch [is.na(my_data$DurationOfPitch)] <-mean(my_data$DurationOfPitch, na.rm=TRUE)
Duration_max = mean(my_data$DurationOfPitch, na.rm=TRUE) + 3*sd(my_data$DurationOfPitch, na.rm=TRUE)
my_data$DurationOfPitch [(my_data$DurationOfPitch > Duration_max)] <-Duration_max

my_data$NumberOfFollowups [is.na(my_data$NumberOfFollowups)] <-mean(my_data$NumberOfFollowups, na.rm=TRUE)
Follow_max = mean(my_data$NumberOfFollowups, na.rm=TRUE) + 3*sd(my_data$NumberOfFollowups, na.rm=TRUE)
my_data$NumberOfFollowups [(my_data$NumberOfFollowups > Follow_max)] <-Follow_max

my_data$NumberOfChildrenVisiting [is.na(my_data$NumberOfChildrenVisiting)] <-mean(my_data$NumberOfChildrenVisiting, na.rm=TRUE)
Children_max = mean(my_data$NumberOfChildrenVisiting, na.rm=TRUE) + 3*sd(my_data$NumberOfChildrenVisiting, na.rm=TRUE)
my_data$NumberOfChildrenVisiting [(my_data$NumberOfChildrenVisiting > Children_max)] <-Children_max

my_data$MonthlyIncome [is.na(my_data$MonthlyIncome)] <-mean(my_data$MonthlyIncome, na.rm=TRUE)
Income_max = mean(my_data$MonthlyIncome, na.rm=TRUE) + 3*sd(my_data$MonthlyIncome, na.rm=TRUE)
my_data$MonthlyIncome [(my_data$MonthlyIncome > Income_max)] <-Income_max

my_data$TypeofContact [is.na(my_data$TypeofContact)] <-NA

Trip_max = mean(my_data$NumberOfTrips, na.rm=TRUE) + 3*sd(my_data$NumberOfTrips, na.rm=TRUE)
my_data$NumberOfTrips [(my_data$NumberOfTrips > Trip_max)] <-Trip_max

my_data$Gender [(my_data$Gender == "Fe_Male")] <-"Female"

clust_cols <- as.data.frame(my_data[, c('Age','DurationOfPitch','MonthlyIncome','CityTier_and_MaritalStatus',
                                        'CityTier_and_Designation','Occupation_and_MaritalStatus','MaritalStatus_and_Designation',
                                        'Passport_and_Designation')])
str(my_data)


#####################SHINY BEGINS HERE##########################
##################### R-Shiny App UI ###########################

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Welcome To Wellness Tourism")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Home", tabName = "Home", icon = icon("home")),
  menuItem("Key Takeaways", tabName = "Takeaway", icon = icon("book")),
  # menuItem("Customer Data Summary", tabName = "customerdata"),
  # menuItem("Current Product User Summary", tabName = "productuser"),
  # menuItem("Potential Product Buyer Summary", tabName = "potentialproduct"),
  menuItem("Customer Data Summary",    tabName = "EDA",    icon = icon("calendar")),
  menuItem("Current Users Summary", tabName = "EDA2", icon = icon("bar-chart-o")),
  menuItem("Potential Buyers Summary", tabName = "EDA3", icon = icon("list-alt")),
  menuItem("Clustering Analytics", tabName = "clustering", icon = icon("table"))
  
  ))

tbs <- tabItems(
  
  tabItem(
    tabName = "Home",
    headerPanel('Home'),
    
    fluidRow(box(width = 10 , tags$img(src="wellnesstourism.jpg", height=250, width=350),
                 br(),
                 br(),
                 h4(em("Problem Statement")),
                 h5("To analyze customers' data for 'Trips & Travel.Com' company to provide consulting on segmentation and bolster strategic marketing for a new travel package, 'Wellness Tourism', through a viable business model."),
                 h4(em("Dataset")),
                 h5("It consists of 4888 records and provides information of the customers, such as demographic information, type of contact, product pitched, duration of pitch, no of persons visited etc."),
                 h4(em("Approach")),
                 h5("A.   Conduct descriptive analysis of the customer data and the variation in the variables based on the tourism package segments"),
                 h5("B.   Use this analysis to understand the factors that drive preference for new product"),
                 h5("C.   Conduct clustering analytics to predict the segments based on the factors that are drive preference for new product"),
                 h5("D.   Create a sophisticated shiny app to build an interactive web application that would glean effective data insights for decision making")))
  ),
  tabItem(
    tabName = "Takeaway",
    headerPanel('Key Takeaways'),
    fluidRow(box(width = 6 , h4(em("1. Factors Affecting Product Selection")),
                 tags$img(src="productdiff.webp", height=100, width=100),
                 br(),
                 br(),
                 h5("The 5 product packages - Basic, Standard, Deluxe, Super Deluxe and King - 
                                            chosen by customer groups who are different in their product and pitch preference, 
                                            and demographics. Their preference is driven by their designation and income."),
                 br(),
                 h4(em("2. Customer Segmentation")),
                 tags$img(src="customerseg.jpg", height=150, width=150),
                 br(),
                 br(),
                 h5("The current product buyers are the ones who belong to the following category:"),
                 h6("a. age group of 15-30"),
                 h6("b. single/unmarried males,"),
                 h6("c. more willing to adopt a product based off sales pitch,"),
                 h6("d. are contacted by company,"),
                 h6("e. belong to tier 2 and 3 cities"),
                 h6("f. working as Executive with a $15-30K salary"),
                 h6("g. prefer 5-star hotels."),
                 br(),
                 h4(em("3. Exploratory Research")),
                 tags$img(src="dataanalysis.png", height=150, width=200),
                 br(),
                 br(),
                 h5("Along with sophisticated techniques such as random forest and XGBoost, 
                                            basic exploratory analysis also provides the most appropriate picture of real 
                                            driving factors of a data. We created a prediction model with 10 independent variables with an accuracy of ~90%"),
                 br(),
    )),
  ),
  
  ##EDA tab content
  tabItem(
    tabName = "EDA",
    fluidRow(
      box(
        title = "Age"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("age", height = "300px")
      ) ,
      box(
        title = "Monthly Income"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("monthlyincome", height = "300px")
      )
    ) #End of Fluid Row 1
    ,
    fluidRow(
      box(
        title = "Pitch Satisfaction Score"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("pitchsscore", height = "300px")
      )
      ,
      box(
        title = "Marital Status"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("maritalstatus", height = "300px")
      )
      
    ) # End of fluid row 2
    ,
    fluidRow(
      box(
        title = "City Tier"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("citytier", height = "300px")
      ),
      box(
        title = "Gender"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("gender", height = "300px")
      )
    ) # End of fluid Row 3
    ,
    fluidRow(
      box(
        title = "Type of Contact"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("typecontact", height = "300px")
      ),
      box(
        width = 6,
        title = "Designation"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("designation", height = "300px")
      )
    )
  ), #End of fluid row 4
  ######################################################
  #Product User Summary tab content
  ######################################################
  tabItem(
    tabName = "EDA2",
    fluidRow(
      box(
        title = "Which segment is most willing to take the new product"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("userplot1", height = "300px")
      ),
      box(
        title = "How satisfied is each segment with the sales pitch"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("userplot2", height = "300px")
      )
    )
    , #End of Fluid Row 1
    
    fluidRow(
      box(
        width = 6,
        title = "Designation"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("userplot8", height = "300px")
      ),
      box(
        width = 6,
        title = "Income level of User Products",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("userplot9", height = "300px")
      ),
      
      
    ) # End of fluid row 2
    ,
    fluidRow(
      
      box(
        title = "Type of Contact"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("userplot3", height = "300px")
      )
      ,
      box(
        title = "Gender Categorization"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("userplot4", height = "300px")
      )
      
    ),
    fluidRow(
      box(
        title = "City Tier"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("userplot5", height = "300px")
      ),
      box(
        title = "Relationship Status of Users"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("userplot6", height = "300px")
      )
    ) # End of fluid Row 3
 
  ),
  
  #######################################################
  # Potential Product Buyer Summary tab content
  #######################################################
  
  tabItem(
    tabName = "EDA3",
    fluidRow(
      box(
        title = "Age group of potential buyers"       ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot1", height = "300px")
      )
      ,
      box(
        title = "What is the income level of the current product buyers"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot2", height = "300px")
      )
    ) #End of Fluid Row 1
    ,
    fluidRow(
      box(
        title = "How are the product buyers reacting to sales pitch?"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot3", height = "300px")
      )
      ,
      box(
        title = "Means of contacting buyers"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot4", height = "300px")
      ),
      box(
        title = "City Tier of Buyers"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot5", height = "300px")
      )
      
    ) # End of fluid row 2
    ,
    fluidRow(
      box(
        title = "Buyer Occupation"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot6", height = "300px")
      ),
      box(
        title = "Gender Share of Buyers"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot7", height = "300px")
      ),
      box(
        title = "Relationship status of Buyers"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot8", height = "300px")
      )) ,
    # End of fluid Row 3
    fluidRow(
      box(
        width = 12,
        title = "Do current buyers have passport?"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot9", height = "300px")
      ),
      box(
        width = 6,
        title = "Designation of Buyers"        ,
        status = "primary"        ,
        solidHeader = TRUE        ,
        collapsible = TRUE        ,
        plotOutput("buyerplot10", height = "300px")
      ),
      box(
        width = 6,
        title = "Hotel Rating preference of Buyers",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("buyerplot11", height = "300px")
      )
    )
  ),
    
  
  tabItem(
    tabName = "clustering",
    headerPanel('Clustering Analytics'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(clust_cols)),
      selectInput('ycol', 'Y Variable', names(clust_cols),
                  selected = names(clust_cols)[[2]]),
      numericInput('clusters', 'Cluster count', 5,
                   min = 1, max = 9),
      actionButton(inputId = "lmgo", label = "Get Linear Model"),
      actionButton(inputId = "Optgo", label = "Get Optimization result"),
      br(),
      br(),
      h2("Parameter Optimization"),
      tableOutput('opt1')
    ),
    mainPanel(
      h2("Clustering Plot"),
      plotOutput('plot1'),
      h2("Linear Model"),
      verbatimTextOutput("sumlm"),
      
    ))
  
)



# combine the two fluid rows to make the body
body <- dashboardBody(tbs,
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ))

#completing the ui part with dashboardPage
ui <-
  dashboardPage(title = 'This is my Page title', header, sidebar, body, skin =
                  'green')

# create the server functions for the dashboard
server <- function(input, output) {
  
  ################################ EDA ##########################################
  
  ######################
  
  output$age <- renderPlot({
    ggplot(my_data, aes(Age, fill = "#06466e")) + 
      geom_density(stat="density", alpha = 0.6)+
      scale_fill_manual(values = c("#06466e"))
    #"#a0816c","#e1c699", "#d8b370"
    ## holi - "#00bcd4", "#8bc34a", "#fff000","#d54799","#faa31b"
    
  })
  
  output$monthlyincome <- renderPlot({ 
    ggplot(my_data, aes(MonthlyIncome, fill = "#06466e")) + 
      geom_density(stat="density", alpha = 0.6)+
      scale_fill_manual(values = c("#06466e"))
  })
  
  output$pitchsscore <- renderPlot({
    ggplot(my_data, aes(PitchSatisfactionScore, fill = PitchSatisfactionScore)) + 
      geom_bar( alpha = 0.9, width = 0.5)+
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd"))
    
  })
  
  output$typecontact <- renderPlot({
    ggplot(my_data, aes(TypeofContact, fill = TypeofContact)) + 
      geom_bar( alpha = 0.9, width = 0.5)+
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd"))  
    
  })
  
  output$citytier <- renderPlot({
    ggplot(my_data, aes(CityTier, fill = CityTier)) + 
      geom_bar( alpha = 0.9, width = 0.5)+
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd"))
  })
  
  output$gender <- renderPlot({
    ggplot(my_data, aes(Gender, fill = Gender)) + 
      geom_bar( alpha = 0.9, width = 0.5)+
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd"))
    
  })
  
  output$maritalstatus <- renderPlot({
    df <- data.frame(
      Group = c("Single", "Married", "Unmarried", "Divorced"),
      value = c(916, 2340, 682, 950)
    )
    bp<- ggplot(df, aes(x="", y=value, fill=Group))+
      geom_bar(width = 1, stat = "identity")
    
    pie <- bp + coord_polar("y", start=0) +
      
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd"))
    pie
    
  }) 
  
  output$designation <- renderPlot({
    ggplot(my_data, aes(Designation, fill = Designation)) + 
      geom_bar( alpha = 0.9, width = 0.5)+
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd"))
    
  })
  
  ##################5 SEGMENT SOLUTION  ########################################
  ############################################################################
  
  output$userplot1 <- renderPlot({
    ggplot(my_data, aes(ProductPitched)) + geom_bar(aes(fill = ProductPitched)) +
      facet_grid( ~ ProdTaken) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$userplot2 <- renderPlot({
    ggplot(my_data, aes(ProductPitched)) + geom_bar(aes(fill = ProductPitched)) +
      facet_grid( ~ PitchSatisfactionScore) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$userplot3 <- renderPlot({
    ggplot(my_data, aes(ProductPitched)) + geom_bar(aes(fill = ProductPitched)) +
      facet_grid( ~ TypeofContact) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$userplot4 <- renderPlot({
    ggplot(my_data, aes(ProductPitched)) + geom_bar(aes(fill = ProductPitched)) +
      facet_grid( ~ Gender) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$userplot5 <- renderPlot({
    ggplot(my_data, aes(ProductPitched)) + geom_bar(aes(fill = ProductPitched)) +
      facet_grid( ~ CityTier) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$userplot6 <- renderPlot({
    ggplot(my_data, aes(ProductPitched)) + geom_bar(aes(fill = ProductPitched)) +
      facet_grid( ~ MaritalStatus) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$userplot8 <- renderPlot({
    ggplot(my_data, aes(ProductPitched)) + geom_bar(aes(fill = ProductPitched)) +
      facet_grid( ~ Designation) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#151e3d", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$userplot9 <- renderPlot({
    ggplot(my_data, aes(MonthlyIncome, fill = factor(ProductPitched))) + 
      geom_density(stat="density", alpha = 0.6)+
      scale_fill_manual(values = c("#06466e", "#ab846c","#8bc34a","#a4dbe8", "#d4acc4"))
  })
  
  
  ################## 2 SEGMENT SOLUTION  ########################################
  ############################################################################
  
  
  output$buyerplot1 <- renderPlot({
    ggplot(my_data, aes(Age, fill = factor(ProdTaken))) + 
      geom_density(stat="density", alpha = 0.6)+
      scale_fill_manual(values = c("#06466e", "#ab846c","#8bc34a","#a4dbe8", "#d4acc4"))
  })
  
  output$buyerplot2 <- renderPlot({
    ggplot(my_data, aes(MonthlyIncome, fill = factor(ProdTaken))) + 
      geom_density(stat="density", alpha = 0.6)+
      scale_fill_manual(values = c("#06466e", "#ab846c","#8bc34a","#a4dbe8", "#d4acc4"))
  })
  
  
  output$buyerplot3 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ PitchSatisfactionScore) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$buyerplot4 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ TypeofContact) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$buyerplot5 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ CityTier) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$buyerplot6 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ Occupation) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  
  output$buyerplot7 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ Gender) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c","#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  

  
  output$buyerplot8 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ MaritalStatus) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  
  output$buyerplot9 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ Passport) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  
  output$buyerplot10 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ Designation) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  output$buyerplot11 <- renderPlot({
    ggplot(my_data, aes(ProdTaken)) + geom_bar(aes(fill = ProdTaken)) +
      facet_grid( ~ PreferredPropertyStar) + coord_flip() + labs(y = "Segment") +
      scale_fill_manual(values = c("#06466e", "#ab846c", "#1f456e", "#59788e", "#52b2bf", "#63c5da", "#f7fbfd" ))
    
  })
  
  
  ##################CLUSTERING CODING ########################################
  ############################################################################
  selectedData <- reactive({
    clust_cols[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(bg= "#FFFAF1", mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
  
  observeEvent(input$lmgo, {
    bt <- lm(ProdTaken~ DurationOfPitch+NumberOfPersonVisiting+MonthlyIncome+
               NumberOfFollowups+NumberOfTrips+Age+NumberOfChildrenVisiting
             ,data=my_data)
    output$sumlm <- renderPrint(bt$coefficients)  
  })
  
  observeEvent(input$Optgo, {
    output$opt1 <- renderTable({
      lps.model <- make.lp(nrow=0, ncol=7)
      
      set.type(lps.model, columns=1, type="real") #DurationOfPitch
      set.type(lps.model, columns=2, type="real") #NumberOfPersonVisiting
      set.type(lps.model, columns=3, type="real") #MonthlyIncome
      set.type(lps.model, columns=4, type="real") #PreferredPropertyStar
      set.type(lps.model, columns=5, type="real") #NumberOfTrips
      set.type(lps.model, columns=6, type="real") #PitchSatisfactionScore
      set.type(lps.model, columns=7, type="real") #OwnCar
      
      get.type(lps.model)
      
      name.lp(lps.model, name="Travel Product Taken")
      
      lp.control(lps.model, sense="max")
      # use parameter as temperary, the obj should be the result from 
      # set.objfn(lps.model, obj=parameters)
      set.objfn(lps.model, obj=c(3.974798e-03,-2.044222e-02,-7.929973e-06,5.103920e-02,8.187509e-03,-4.932945e-03,-1.740629e-03))
      
      # constraint X1 to X9 
      add.constraint(lps.model, c(1,0,0,0,0,0,0), "<=", max(my_data$DurationOfPitch,na.rm = TRUE))
      add.constraint(lps.model, c(1,0,0,0,0,0,0), ">=", min(my_data$DurationOfPitch,na.rm = TRUE))
      
      add.constraint(lps.model, c(0,1,0,0,0,0,0), "<=", max(my_data$NumberOfPersonVisiting,na.rm = TRUE))
      add.constraint(lps.model, c(0,1,0,0,0,0,0), ">=", min(my_data$NumberOfPersonVisiting,na.rm = TRUE))
      
      add.constraint(lps.model, c(0,0,1,0,0,0,0), "<=", max(my_data$MonthlyIncome,na.rm = TRUE))
      add.constraint(lps.model, c(0,0,1,0,0,0,0), ">=", min(my_data$MonthlyIncome,na.rm = TRUE))
      
      add.constraint(lps.model, c(0,0,0,1,0,0,0), "<=", max(my_data$`NumberOfFollowups`,na.rm = TRUE))
      add.constraint(lps.model, c(0,0,0,1,0,0,0), ">=", min(my_data$`NumberOfFollowups`,na.rm = TRUE))
      
      add.constraint(lps.model, c(0,0,0,0,1,0,0), "<=", max(my_data$`NumberOfTrips`,na.rm = TRUE))
      add.constraint(lps.model, c(0,0,0,0,1,0,0), ">=", min(my_data$`NumberOfTrips`,na.rm = TRUE))
      
      add.constraint(lps.model, c(0,0,0,0,0,1,0), "<=", max(my_data$`Age`,na.rm = TRUE))
      add.constraint(lps.model, c(0,0,0,0,0,1,0), ">=", min(my_data$`Age`,na.rm = TRUE))
      
      add.constraint(lps.model, c(0,0,0,0,0,0,1), "<=", max(my_data$`NumberOfChildrenVisiting`,na.rm = TRUE))
      add.constraint(lps.model, c(0,0,0,0,0,0,1), ">=", min(my_data$`NumberOfChildrenVisiting`,na.rm = TRUE))
      
      
      
      # run the model
      lps.model
      
      # see the result of model
      solve(lps.model)
      
      get.objective(lps.model) # optimal obj. value (i.e. our maximum profit)
      get.variables(lps.model) # optimal solution of d.v.'s (i.e. our decisions to make)
      
      opt_d1 <- cbind(c('DurationOfPitch','NumberOfPersonVisiting','MonthlyIncome',
                        'NumberOfFollowups','NumberOfTrips','Age','NumberOfChildrenVisiting'),data.frame(get.variables(lps.model))
      )
      colnames(opt_d1)<-c("Important Variable","Optimization Value")
      # ggplot(data=opt_d1, aes(x=Variable, y=Optimization.Value)) +
      #   geom_bar(stat="identity", width=0.1)+
      #   geom_text(aes(label=Optimization.Value), position=position_dodge(width=0.9), vjust=-0.5)
      
      opt_d1
      
      
    })
  })
  
}

shinyApp(ui = ui, server = server)


