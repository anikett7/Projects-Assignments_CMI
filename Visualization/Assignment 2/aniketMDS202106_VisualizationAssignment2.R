library(shiny)
library(shinydashboard)
library(ggplot2)
gcdtdata=read.csv("german_credit_data.csv",header=TRUE)
gcdtdata$Job=as.factor(gcdtdata$Job)
ui<-dashboardPage(title="Visualization Assignment 2",
  dashboardHeader(title="Assignment 2"),
  dashboardSidebar(sidebarMenu(
    menuItem("Checking Central Limit Theorem",tabName="menu1"),
    menuItem("Data Visualization",tabName="menu2")
  )),
  dashboardBody(
    tabItems(
      tabItem("menu1",
                titlePanel("Simulator For Checking Central Limit Theorem"),
                fluidRow(column(width=3,numericInput("simsize","Simulation Size",value=1000)),
                         column(width=3,numericInput("smplsize","Sample Size",value=30)),
                         column(width=3,numericInput("p_mean","Mean Of The Population",value=50)),
                         column(width=3,numericInput("p_sd","Standard Deviation Of The Population",value=25)),
              tabsetPanel(
                tabPanel("Gamma Distribution",
                                  fluidPage(fluidRow(column(width=4,offset=4,actionButton("view1","Show/Update Histogram & Q-Q Plot Of Sample Means and Kolmogorov-Smirnov Normality Test"))),
                                  fluidRow(plotOutput("plot1"),plotOutput("plot2"),verbatimTextOutput("print3")))),
                         tabPanel("Uniform Distribution",
                                  fluidPage(fluidRow(column(width=4,offset=4,actionButton("view2","Show/Update Histogram & Q-Q Plot Of Sample Means and Kolmogorov-Smirnov Normality Test"))),
                                  fluidRow(plotOutput("plot4"),plotOutput("plot5"),verbatimTextOutput("print6"))))))),
      tabItem("menu2",
                titlePanel("Data Visualization"),
              tabsetPanel(
                tabPanel("Pie Chart And Bar Diagram",fluidRow(column(width=4,offset=3,selectInput("catvbl","Select Any Categorical Variable",choices=c("Sex","Job","Housing","Saving Account","Checking Account","Purpose")))),
                         fluidRow(plotOutput("plot7"),plotOutput("plot8"))),
                tabPanel("Side BY Side Boxplot",fluidRow(column(width=4,offset=2,selectInput("numervar","Select Any Numeric Variable",choices=c("Age", "Credit Amount", "Duration"))),
                                                         column(width=4,selectInput("catervar","Select Any Categorical Variable", choices=c("Sex","Job","Housing","Saving Account","Checking Account","Purpose")))),
                         fluidRow(plotOutput("plot9")))))
    )
  )
  
)

server<-function(input,output){
  g_smeans=reactive({
    pmean=input$p_mean
    psd=input$p_sd
    pvar=(psd)^2
    gscale=pvar/pmean
    gshape=pmean/gscale
    g_smeans=c()
    for (k in 1:input$simsize){
      gsamples=rgamma(n=input$smplsize,shape=gshape,scale=gshape)
      g_smeans[k]=sum(gsamples)/(input$smplsize)}
    g_smeans})
  u_smeans=reactive({
    pmean=input$p_mean
    psd=input$p_sd
    umin=pmean-psd*sqrt(3)
    umax=pmean+psd*sqrt(3)
    u_smeans=c()
    for (k in 1:input$simsize){
      usamples=runif(n=input$smplsize,min=umin,max=umax)
      u_smeans[k]=sum(usamples)/(input$smplsize)}
    u_smeans})
  
  observeEvent(input$view1,{output$plot1=renderPlot({
    isolate(hist(g_smeans(),freq=FALSE,main="Histogram of Sample Means",xlab="Gamma Sample Means"))})
  output$plot2=renderPlot({isolate(qqnorm(g_smeans(),main="Q-Q Plot of Sample Means",xlab="Normal Quantiles",ylab="Sample Quantiles"))
    isolate(qqline(g_smeans(),col="blue"))})
  output$print3=renderPrint({
    isolate(ks.test(g_smeans(),"pnorm",mean=isolate(mean(g_smeans())),sd=isolate(sd(g_smeans()))))})
  })
  observeEvent(input$view2,{output$plot4=renderPlot({
    isolate(hist(u_smeans(),freq=FALSE, main="Histogram of Sample Means",xlab="Uniform Sample Means"))})
  output$plot5=renderPlot({
    isolate(qqnorm(u_smeans(),main="Q-Q Plot of Sample Means",xlab="Normal Quantiles",ylab="Sample Quantiles"))
    isolate(qqline(u_smeans(),col="blue"))})
  output$print6=renderPrint({
    isolate(ks.test(u_smeans(),"pnorm",mean=isolate(mean(u_smeans())),sd=isolate(sd(u_smeans()))))})
  })
  ctvbldata=reactive({switch(input$catvbl,
                             "Sex"=gcdtdata$Sex,
                             "Job"=gcdtdata$Job,
                             "Housing"=gcdtdata$Housing,
                             "Saving Account"=gcdtdata$Saving.accounts,
                             "Checking Account"=gcdtdata$Checking.account,
                             "Purpose"=gcdtdata$Purpose)})
  df1=reactive({data.frame(ctvbldata())})
  tb1=reactive({table(ctvbldata())})
  output$plot7=renderPlot({
    p1=ggplot(df1(),aes(x="",fill=ctvbldata()))+geom_bar(color="white")+coord_polar("y",start=0)+labs(title="Pie Chart",fill=input$catvbl)+
      theme(plot.title=element_text(hjust=0.5,size=15,face="bold"),axis.title=element_blank(),axis.text=element_blank(),panel.grid=element_blank(),
            legend.text=element_text(size=15),legend.title=element_text(size=18),legend.key.size=unit(1,"cm"))
    p1})
  output$plot8=renderPlot({
    p2=ggplot(df1())+geom_bar(aes(x=ctvbldata(),fill=ctvbldata()))+
      labs(x=input$catvbl,title="Bar Plot",fill=input$catvbl)+theme(plot.title=element_text(hjust=0.5,size=15,face="bold"),
                                                                    axis.title=element_text(size=14),axis.text=element_text(size=12),                                                           
                                                                    legend.text=element_text(size=15),legend.title=element_text(size=18),legend.key.size=unit(1,"cm"))
    p2})
  numerdata=reactive({switch(input$numervar,
                             "Age"=gcdtdata$Age,
                             "Credit Amount"=gcdtdata$Credit.amount,
                             "Duration"=gcdtdata$Duration)})
  caterdata=reactive({switch(input$catervar,
                             "Sex"=gcdtdata$Sex,
                             "Job"=gcdtdata$Job,
                             "Housing"=gcdtdata$Housing,
                             "Saving Account"=gcdtdata$Saving.accounts,
                             "Checking Account"=gcdtdata$Checking.account,
                             "Purpose"=gcdtdata$Purpose)})
  df141=reactive({data.frame(cbind(numerdata(),caterdata()))})
  output$plot9=renderPlot({
    p3=ggplot(df141())+geom_boxplot(aes(x=caterdata(),y=numerdata(),fill=caterdata()))+
      labs(x=input$catervar,y=input$numervar,title="Side By Side Boxplot",fill=input$catervar)+
      theme(plot.title=element_text(hjust=0.5,size=15,face="bold"),
            axis.title=element_text(size=14),axis.text=element_text(size=12),
            legend.text=element_text(size=15),legend.title=element_text(size=18),legend.key.size=unit(1,"cm"))
    p3
  })
}
shinyApp(ui,server)
