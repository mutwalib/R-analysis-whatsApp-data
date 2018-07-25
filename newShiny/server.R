library(tm)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(xfun) #for file extension
library(wordcloud)
library(RColorBrewer)
library(forcats)
library(stringi)
library(syuzhet)
library(lubridate)
library(stringr)
library(tidyr)
setwd("~/Desktop/newShiny")
shinyServer(function(input, output, session) {
dat <- readLines("WhatsApp Chat with Person.txt")
#joinedData <- rep(NA, length(dat))
empty <- grepl("^s* ", dat)
joinedData <- dat[!empty]
for(i in 1:length(dat))
{
  startline <- grepl("^\\[?\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}", dat[i])  
  gr <- 1
  if (startline) {
    joinedData[gr] <- dat[i]
    gr <- gr + 1
  }
  if (!startline) {
    # if doesn't start with timestamp, append to previous (ss)
    ss <- gr - 1
    joinedData[ss] <- paste(joinedData[ss], dat[i])
  }
}
joinedData <- joinedData[!is.na(joinedData)]
joinedData <- as.data.frame(joinedData,row.names = NULL, optional = FALSE )
colnames(joinedData)<-'V1'
#get rid of square brackets around datetime
if(grepl("^\\[", joinedData$V1[5], perl = TRUE)){
  joinedData$V1 <- sub("^\\[.*?", "", joinedData$V1)
  joinedData$V1 <- sub("\\].*?", ":", joinedData$V1)
}
sepData<-suppressWarnings(tidyr::separate(joinedData, V1, c("datetime", "message"), sep = ": ", extra = "merge"))
sepData<-suppressWarnings(tidyr::separate(sepData, datetime, c("datetime", "sender"), sep = "- ", extra = "merge"))
sepData$message<- stringi::stri_trans_general(sepData$message, "latin-ascii")
sepData$message <- trimws(sepData$message)
filtData <- sepData %>%
  group_by(sender) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  filter(!str_detect(sender, 'changed|left|added|created|joined|removed')) %>%
  filter(!str_detect(message, '<Media omitted>|image|video')) %>%
  #filter(!grepl('\\+', sender)) %>%
  filter(!is.na(message)) %>%
  mutate(sender = as.factor(sender)) %>%
  droplevels()
cleanData<-tidyr::separate(filtData, datetime, c("date", "time"), sep = " ", remove =TRUE)
print(cleanData)

observe({
  # inFile <- input$file
  # if(is.null(inFile)) return(NULL)
  # d <- parseR(in_file=inFile$datapath)
  # tabpanel 1 - data table
  output$contents <- DT::renderDT(cleanData)
  # tabPanel 2 - Number of messages
  output$postCount <-renderPlot({
    senderPosts(chatdf = cleanData)
  })
   # tabPanel 3 - Top words
   observe({
     updateSelectInput(session, inputId = 'wlength', label = 'Minimum word length',
                       choices = c(3:5), selected = 3)
   })
   allWords <- makeCorpus(chatdf = cleanData) 
   output$wordCount <-renderPlot({
     wordFreq(corpus=allWords, wordlength=input$wlength)
   })
   # tabPanel 4 - Word cloud
   observe({
     updateSelectInput(session, inputId = 'user', label = 'Sender',
                       choices = c("All", levels(cleanData$sender)), selected = 'All')
     updateSelectInput(session, inputId = 'cwlength', label = 'Minimum word length',
                       choices = c(3:5), selected = 3)
   })
   output$wCloud <-renderPlot({
     chatCloud(chatdf = cleanData, wordlength=input$cwlength, user=input$user)
   })
   # tabPanel 5 - Messages throughout the day
   observe({
     updateSelectInput(session, inputId = 'Tuser', label = 'Sender',
                       choices = c("All", levels(cleanData$sender)), selected = 'All')
   })
   output$timePlot <-renderPlot({
     senderTime(chatdf = cleanData, user=input$Tuser)
   })
   # # tabPanel 6 - Messages throughout years
   # observe({
   #   updateSelectInput(session, inputId = 'Duser', label = 'Sender',
   #                    choices = c("All", levels(cleanData$sender)), selected = 'All')
   #   updateSelectInput(session, inputId = 'Dyear', label = 'Year',
   #                     choices = c("All", levels(factor(year(cleanData$date)))), selected = 'All')
   # })
   # output$datePlot <-renderPlot({
   #   senderDate(chatdf = cleanData, user=input$Duser,filtYear=input$Dyear)
   # })
   # tabPanel 7 - Sentiments
   # observe({
   #   updateSelectInput(session, inputId = 'method1', label = 'Method',
   #                     choices = c("nrc", 'bing', 'loughran'), selected = 'loughran')
   #   updateSelectInput(session, inputId = 'top_sender', label = 'Top n senders',
   #                     choices = c(1:10), selected = 5)
   # })
   # output$sentiments <-renderPlot({
   #   chatSentiments(chatdf = cleanData, top_sender = input$top_sender, method = input$method1)
   # })
})
#sender post
senderPosts <- function(chatdf){
  # if(length(file_in)>0)
  #   chatdf <- parseR(in_file=file_in, user=user)
  postCount <- chatdf %>%
    group_by(sender) %>%
    tally() %>%
    arrange(-n)
  division <- plyr::round_any(ceiling(max(postCount$n)/10), 10, f = ceiling)
  if(max(postCount$n)>=100){
    division <- plyr::round_any(ceiling(max(postCount$n)/10), 50, f = ceiling)
  }
  if(max(postCount$n)>=500){
    division <- plyr::round_any(ceiling(max(postCount$n)/10), 100, f = ceiling)
  }
  p <- ggplot(postCount)
  p <- p + geom_bar(aes(fct_reorder(sender, n), n, fill = "deepskyblue1"),stat='identity')
  p <- p + scale_y_continuous("Number of posts", breaks=seq(0,max(postCount$n),by=division),expand = c(0.01,0.05))
  p <- p + cleanTheme() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y = element_text(size=20),
      axis.text.x = element_text(size=15),
      panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted")
    )
  p <- p + scale_fill_identity()
  p <- p + coord_flip()
  p
}
#make corpus
makeCorpus <- function(chatdf){
  excludedWords <- c("omitted", "image", 'video', 'media')
  docs <- Corpus(VectorSource(chatdf$message)) %>%
    tm_map(content_transformer(htmlStrip)) %>%  # removing email ids
    tm_map(content_transformer(RemoveEmail)) %>%  # removing email ids
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower))  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, excludedWords) %>%
    tm_map(stripWhitespace)
  # dataframe of terms
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  all <- data.frame(word = names(v),freq=v)
  return(all)
}
#word frequence
wordFreq <- function(wordlength=3, corpus){
  all <- corpus %>%
    filter(nchar(as.character(word))>=wordlength)
  d <- all[1:15,]
  d  <- transform(d , word = reorder(word, freq))
  division <- plyr::round_any(ceiling(max(d$freq)/10), 10, f = ceiling)
  if(max(d$freq)>=100){
    division <- plyr::round_any(ceiling(max(d$freq)/10), 50, f = ceiling)
  }
  if(max(d$freq)>=500){
    division <- plyr::round_any(ceiling(max(d$freq)/10), 100, f = ceiling)
  }
  p <- ggplot(d)
  p <- p + geom_bar(aes(word, freq, fill="springgreen3"),stat='identity')
  p <- p + scale_y_continuous("Word frequency", breaks=seq(0,max(d$freq),by=division),expand=c(0.01,0))
  p <- p + scale_x_discrete("Word", expand = c(0.01,0.01))
  p <- p + cleanTheme() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y = element_text(size=20),
      axis.text.x = element_text(size=15),
      panel.grid.major.x = element_line(color="grey80", size = 0.5, linetype = "dotted")
    )
  p <- p + scale_fill_identity()
  p <- p + coord_flip()
  p
}
#remove Emails
RemoveEmail <- function(x) {
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
}
#htmlStrip
htmlStrip <- function(y) {
  return(gsub("<.*?>", "", y))
}
#clean theme
cleanTheme <- function(base_size = 12){
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    axis.text = element_text(size=15),
    axis.title.x=element_text(size=15),
    axis.title.y=element_text(size=15),
    strip.text = element_text(size=15)
  )
}
#chat cloud
chatCloud <- function(chatdf, user='All', wordlength=3){
  userFilt <- 1
  if(user == "All") userFilt <- 0
  if(userFilt)
    chatdf <- filter(chatdf, sender==user)
  all <- makeCorpus(chatdf)
  all <- all %>%
    filter(nchar(as.character(word))>=wordlength)
  options(warn=-1)
  wordcloud(words = all$word, freq = all$freq, min.freq = 1,
            max.words=50, random.order=FALSE,
            rot.per=0.35,
            colors=brewer.pal(6, "Paired")
  )
  options(warn=0)
}
#sender time
senderTime <- function (user='All', chatdf=FALSE) {
  userFilt <- 1
  yearFilt <- 1
  # if(user == "All") userFilt <- 0
  # if(length(file_in)>0)
  #   chatdf <- parseR(in_file=file_in, user=user)
  allData <- chatdf
  allData$time <- hm(allData$time)
  allData$hour<-lubridate::hour(allData$time)
  maxPosts<-max(table(allData$hour))
  if(userFilt)
    chatdf <- filter(chatdf, sender==user)
  chatdf$time <- hms(chatdf$time)
  chatdf$hour<-lubridate::hour(chatdf$time)
  labs<-c("12am", "1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "")
  p <- ggplot(chatdf, aes(hour, fill=sender))
  p <- p + geom_area(aes(group = sender, colour = sender), stat='bin',position="stack",binwidth=1, alpha = 0.5)
  if(userFilt){
    p <- p + scale_y_continuous("Number of posts", limits=c(0, maxPosts))
  } else{
    p <- p + scale_y_continuous("Number of posts")
  }
  p <- p + scale_x_continuous("Time", breaks=seq(0,23, by=1), labels=labs)
  # p <- p + scale_x_date(date_breaks="1 month", date_labels="%B")
  p <- p + cleanTheme() +
    theme(axis.text.x = element_text(angle = 90, hjust=1),
          panel.grid.major.y = element_line(color="grey80", size = 0.5, linetype = "dotted"),
          axis.title.x=element_blank(),
          legend.position="bottom"
    )
  p
}
# #sender date
# senderDate <- function(user='All', filtYear = 'All', chatdf = FALSE){
#   allData <- chatdf
#   allData$date <- mdy_hm(allData$date)
#   allData$year <- year(allData$date)
#   allData$month <- month(allData$date, label = TRUE, abbr = TRUE)
#   maxPosts <- max(table(week(allData$date),allData$year))
#   n <- length(levels(allData$sender))
#   cols <- gg_colour_hue(n)
#   if(userFilt==1){
#     chatdf <- filter(chatdf, sender==user)
#   }
#   data$date <- mdy_hm(data$date)
#   data$year <- year(data$date)
#   if(yearFilt==1){
#     data <- filter(d, year == filtYear)
#   }
#   data$month<-month(d$date, label = TRUE, abbr = TRUE)
#   months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
#   p <- ggplot(data, aes(as.Date(date), fill=sender))
#   p <- p + geom_area(aes(group = sender, colour = sender), stat='bin',position="stack",binwidth=14, alpha = 0.5)
#   if(userFilt==1){
#     p <- p + scale_y_continuous("Number of posts", limits=c(0, maxPosts))
#   } else{
#     p <- p + scale_y_continuous("Number of posts")
#   }
#   p <- p + scale_x_date(date_breaks="1 month", date_labels="%B", expand=c(0,0))
#   p <- p + cleanTheme() +
#     theme(axis.text.x = element_text(angle = 90, hjust=1,vjust = 0.5),
#           panel.grid.major.y = element_line(color="grey80", size = 0.5, linetype = "dotted"),
#           axis.title.x=element_blank(),
#           legend.position="bottom"
#     )
#   p
# }
# #gg_colour_hue
# gg_colour_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }


})#shiny server ends here