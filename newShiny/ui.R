library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  tags$head(HTML(
    "<script>
    (function(i,s,o,g,r,a,m){
    i['GoogleAnalyticsObject']=r;i[r]=i[r]||
    function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
    a=s.createElement(o), m=s.getElementsByTagName(o)[0];
    a.async=1;
    a.src=g;m.parentNode.insertBefore(a,m)
    })
    (window, document, 'script',
    '//www.google-analytics.com/analytics.js','ga');
    ga('create', 'UA-109758097-1', 'auto');
    ga('send', 'pageview');
    </script>"
  )),
  theme=shinytheme('flatly'),
  navbarPage("Group X whatsApp analysis"),
  tabsetPanel(
    # Tab 1
    tabPanel("Data table",
                 h3("Welcome to Group X whatsApp analysis"),
                 p("Click on the tabs above to see different analyses of your whatsApp chat data!"),
                 DT::DTOutput('contents')
    ),
    
    # Tab 2
    tabPanel("Post Count",
             #pageWithSidebar(
               headerPanel('Number of posts per user'),
                #sidebarPanel(),
               mainPanel(
                 plotOutput('postCount')
              )
            # )
    ),
    
    # Tab 3
    tabPanel("Word Frequency",
             pageWithSidebar(
               headerPanel('Most common words'),
               sidebarPanel(
                 sliderInput("wlength", "Minimum word length",
                             min = 2, max = 10, "")
               ),
               mainPanel(
                 plotOutput('wordCount')
               )
             )
    ),
    # Tab 4
    tabPanel("Word Cloud",
             pageWithSidebar(
               headerPanel('Word cloud of chat'),
               sidebarPanel(
                 selectInput('user', 'Sender', ""),
                 sliderInput("cwlength", "Minimum word length",
                             min = 2, max = 10, "")
               ),
               mainPanel(
                 plotOutput('wCloud')
               )
             )
    ),
    # Tab 5
    tabPanel("Time of day",
             pageWithSidebar(
               headerPanel('When are messages sent?'),
               sidebarPanel(
                 selectInput('Tuser', 'Sender', "")
               ),
               mainPanel(
                 plotOutput('timePlot')
               )
             )
    ),
    # Tab 6
    tabPanel("Date",
             pageWithSidebar(
               headerPanel('Chat history'),
               sidebarPanel(
                 selectInput('Duser', 'Sender', ""),
                 selectInput('Dyear', 'Year', "")
               ),
               mainPanel(
                 plotOutput('datePlot')
               )
             )
    ),
    # Tab 3
    tabPanel("Sentiments",
             pageWithSidebar(
               headerPanel('Sentiment Analysis of messages'),
               sidebarPanel(
                 selectInput('method1', 'Method', ""),
                 sliderInput("top_sender", "Top n senders",
                             min = 1, max = 10, "")
               ),
               mainPanel(
                 plotOutput('sentiments')
               )
             )
    )
  )
  )
)