library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(grid)
library(scales)
library(Cairo)
library(gtable)

is.linear.polar2 <<- function(x) TRUE
coord_polar2 <<-   coord_polar(theta="x", start = pi/12/2, direction=1) 
class(coord_polar2) <<- c("polar2", class(coord_polar2))


server <- function(input, output) {
  observe({
    
    output$CoolLabel1 <- renderUI({
      tags$table(
        tags$tr(tags$td(tags$strong("Total Harm Averter"))),
        tags$tr(tags$td(tags$h1("128")))
      )
    })
    output$CoolLabel2 <- renderUI({
      tags$table(
        tags$tr(tags$td(tags$strong("Total Severe Harm Averter"))),
        tags$tr(tags$td(tags$h1("46")))
      )
    })
    
    output$CoolLabel4 <- renderUI({
      tags$table(
        tags$tr(tags$td(tags$strong("Total Alerts"))),
        tags$tr(tags$td(tags$h1("6%"))),
        tags$tr(tags$td(tags$small("Down from average")))
      )
    })
    output$CoolLabel5 <- renderUI({
      tags$table(
        tags$tr(tags$td(tags$strong("Most Impr. Area"))),
        tags$tr(tags$td(tags$h1("NICU"))),
        tags$tr(tags$td(tags$small("22% оver average")))
      )
    })
    output$CoolLabel6 <- renderUI({
      tags$table(
        tags$tr(tags$td(tags$strong("Hightes Compliance"))),
        tags$tr(tags$b(tags$td(tags$h1("Pediatrio")))),
        tags$tr(tags$td(tags$small("2.3% оver target")))
      )
    })
    
    output$CoolLabel99 <- renderUI({
      tags$table(
        tags$tr(tags$td(tags$strong("НОВЫЙ РАЗДЕЛ")))
      )
    })
    ####################################################################################
    # Кольцевой график на черном фоне который    
    ####################################################################################
    # Input the ad data
    ad <- data.frame(
      type = c("9999CC", "Billboard", "Bus"),
      n = c(74, 14, 12)
      # n = c(input$n1, input$n2, input$n3)
      
    )
    ad$fraction  <-  ad$n / sum(ad$n)
    ad$ymax  <-  cumsum(ad$fraction)
    ad$ymin  <-  c(0, head(ad$ymax, n = -1))
    
    CairoPNG("MY_cairopng.png", bg = "transparent", height = 100, width = 100)
    p <-  
      ggplot(data = ad, aes(fill = type, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3), height=280) +
      geom_rect(show_guide = FALSE)  +
      coord_polar(theta = "y") +
      xlim(c(1.5, 4)) +
      theme_bw() +
      theme(
        panel.grid=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank()
        # , plot.margin=unit(c(-3,-5,-10,-10),"mm")
        , plot.margin=unit(c(-12,-12,-37,-37),"points")
      )  +
      geom_text(x=1.5,y=4, label = "4'657", fontface="bold",colour='white', size=7) +
      geom_text(x=0.8,y=4, label = "Alerts",colour='white', size=4) +
      xlab("") +
      ylab("")+
      scale_fill_manual(values=c("#718795", "#3C596A", "#D04649"))
    print(p)
    dev.off()
    
    output$plot1 <- renderImage({ 
      list(src = 'MY_cairopng.png',
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    ####################################################################################
    # График роза ветров на белом фоне
    ####################################################################################
    example <- data.frame(c(1, 2,3,4,5,6,7, 8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                          c(0,4,11,7,5,6,7,4,11,4,10, 6, 7, 5,10, 6, 8, 4, 5, 3, 4, 4, 7, 4))
    
    colnames(example) <- c("r", "theta")
    connect_line <- example[c(1,nrow(example)),]
    
    is.linear.polar2 <- function(x) TRUE
    coord_polar2 <-   coord_polar(theta="x", start = pi/12/2, direction=1) 
    class(coord_polar2) <- c("polar2", class(coord_polar2))
    
    text_y_axe <- data.frame(x=rep(24,6), y=c(2,4,6,8,10,12))
    CairoPNG("rose_cairopng.png", bg = "transparent", height = 350, width = 350)
    p <-  
      ggplot(example, aes(r, theta))+
      geom_area(colour=NA, fill="#8FAEC2", alpha=.6)+  
      coord_polar2+
      scale_x_discrete()+
      scale_y_continuous(limits=c(0,12), breaks=c(2,4,6,8,10,12))+
      # ylim(0,12)+
      geom_text(aes(x=x,y=y,label=y),data=text_y_axe, size=3)+
      theme_bw()+
      ggtitle("24 Hour Fluid Alerts") +
      theme(plot.title=element_text(hjust =0, colour = '#999999', face='bold'))+
      theme(
        panel.border=element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major=element_line(colour = 'lightgrey'),
        axis.text.y=element_blank(),
        
        axis.line=element_blank(),
        axis.line=element_blank(),        
        
        axis.text.x=element_text(vjust=0, face='bold'),
        axis.ticks.y=element_blank(),
        axis.title=element_blank()
      )
    
    print(p)
    dev.off()
    output$plot2 <- renderImage({ 
      list(src = 'rose_cairopng.png',
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    ####################################################################################
    # Кольцевой график на белом фоне
    ####################################################################################
    firstCircle.df <- data.frame( num=c('1','2','3','4','5','6'),
                                  lab=c('    Overrides','     Heparine','    HYDROMophine',
                                        '    Anticoag Citrate','    Vassopressin',''),
                                  val=c(100,23,24,25,26,0)
    )
    
    firstCircle.df$fraction  <-  firstCircle.df$val / sum(firstCircle.df$val)
    firstCircle.df$ymax  <-  cumsum(firstCircle.df$fraction)
    firstCircle.df$ymin  <-  c(0, head(firstCircle.df$ymax, n = -1))
    lab_data <- data.frame(x=c(0,0),y=c(1,0.5) )
    
    CairoPNG("round_white.png", bg = "transparent", height = 300, width = 600)
    p <-  
      ggplot() +
      geom_rect(show_guide = TRUE, data = firstCircle.df, aes(fill = num, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3))  +
      scale_fill_manual(values=c("#D3D8D8","#8FB7D2", "#73A4CC", "#5097C3",  "#417091",'#ECEFF4'),
                        labels=firstCircle.df$lab, name="TOP Overrides\n")+
      xlim(c(0, 4))+ 
      coord_polar(theta = "y") +
      theme_bw() +
      labs(fill="")+
      theme(
        panel.grid=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        legend.position="left",
        legend.title=element_text(size = 14, face='bold', color='#999999'),
        legend.background = element_blank(), #element_rect(colour = "ECEFF4"), 
        legend.text=element_text(size=14, face='bold'),
        legend.key=element_rect(size=8,color="#ECEFF4")
      )+
      guides(fill=guide_legend(
        keywidth=0.5,
        keyheight=0.5,
        default.unit="inch")
      )+
      xlab("") +
      ylab("") +
      geom_text(x=0.5, y=0, label = "50%", fontface="bold",colour='black', size=18, data=lab_data[1,]) +
      geom_text(x=-1,y=0, label = "of 2'646",colour='black', size=7, data=lab_data[2,]) 
    
    print(p)
    dev.off()
    output$plot3 <- renderImage({ 
      list(src = 'round_white.png',
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    ####################################################################################
    # Правый верхний график
    ####################################################################################
    dat <- read.csv(file='data1.csv',sep=';',header = TRUE, stringsAsFactors = FALSE)
    dat$DAY <- dmy(dat$DAY)
    dat$alpha <- as.factor(dat$alpha)
    dat$DAY <- as.Date(dat$DAY)
    
    CairoPNG("right_top.png", bg = "transparent", height = 300, width = 800)
    p <-  
      ggplot(dat, aes(x=DAY, y=line1, fill=color, group=color))+
      geom_ribbon(aes(ymin=0,ymax=line1+10), alpha=0.6)+
      theme_bw()+
      scale_fill_manual(values = c("#B7D0DC", "#80B5D5"), labels=c("Severe", "Many"))+
      ylim(c(0, 160)) +
      ylab('')+xlab('')+
      geom_hline(yintercept=c(50,100,150), linetype='dotted', alpha=0.3)+
      geom_hline(yintercept=c(90), linetype='dashed', alpha=0.65)+
      theme(
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.margin = unit(0, "lines"),
        plot.background = element_blank(),
        legend.title= element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,1),
        legend.key.size=unit(10,"points"),
        legend.direction="horizontal",
        legend.key=element_blank(),
        legend.text=element_text(colour='#999999')
      )+
      ggtitle("Harms Averted Trends") +
      theme(plot.title=element_text(hjust =-0.08, vjust = 2.5, colour = '#999999', face='bold'))
    
    print(p)
    dev.off()
    output$plot4 <- renderImage({ 
      list(src = 'right_top.png',
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    ####################################################################################
    # Правый нижний график
    ####################################################################################
    dat <- read.csv(file='data2.csv',sep=';',header = TRUE, stringsAsFactors = TRUE)
    
    CairoPNG("right_bottom.png", bg = "transparent", height = 350, width = 830)
    p <-  
      ggplot(dat, aes(x=type, y=val, fill=var)) +
      geom_bar(stat="identity", width=.55)+
      theme_bw()+
      scale_fill_manual(values = c("#346178", "#4D7EA0","#80BDE0","grey"))+
      ylim(c(0, 300))+
      ylab('')+xlab('')+
      theme(
        panel.grid=element_blank(),
        # axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.margin = unit(0, "lines"),
        plot.background = element_blank(),
        legend.title= element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,1),
        legend.key.size=unit(10,"points"),
        legend.direction="horizontal",
        legend.key=element_blank(),
        legend.text=element_text(colour='#999999')
      )+
      ggtitle("Action by Care Area") +
      theme(plot.title=element_text(hjust =-0.08, vjust = 2.5, colour = '#999999', face='bold'))
    
    print(p)
    dev.off()
    output$plot5 <- renderImage({ 
      list(src = 'right_bottom.png',
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    
  })  
}
shinyServer(server)
