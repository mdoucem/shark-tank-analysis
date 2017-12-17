###
# This script takes as input a cvs file containing observations from each Shark Tank episode.
# With the data, pie charts, stacked bar charts and radar charts are plotted to visualize the gender distributions in the show.
# Source of data is from here - bit.ly/STankData

# Author: Myriam Munezero
###

# Needed libraries
library(ggplot2)
library(plyr) # allows the use of the count function
library(scales) # for the plot grids

#Radar charts require the following packages
library(varhandle)
library(fmsb)

# Load the data into a DataFrame
tank_vec<-function(){
  shark_data<-read.csv("shark-tank-part1.csv")
  shark_data<-sapply(shark_data,gsub,pattern=";",replacement=",")
  len<-nrow(shark_data)
  row_data<-c()
  result<-data.frame()
  for (i in 1:len){
    row<-shark_data[[i]]
    row_vec<-c(strsplit(row,","))
    row_data[[i]]<-rapply(row_vec, c)
    result<-rbind(result, row_data[[i]], stringsAsFactors = FALSE)
  }
  colnames(result)<-c("Season", "Series", "Episode", "Company", "Deal",
                      "Industry", "Gender")
  result

}

tank_data<-tank_vec()

# Subsetting the data - only extract the necessary columns for the analysis
subset_data<-data.frame(tank_data$Season, tank_data$Gender, tank_data$Deal, tank_data$Industry)
colnames(subset_data)<-c("Season", "Gender", "Deal", "Industry")
subset_data$Season<-paste("s", subset_data$Season, sep = "")



##================================ Plots begin ================================##

##------------------------- Pie Graph-----------------------##

#===Pie Chart - plots the gender distribution among the eight seasons===##


pie_graph<-function(vec){
  vec<-table(vec$Gender)
  vec<-as.data.frame(vec)
  Category<-vec[,1]
  count<-vec[,2]

  data<-data.frame(Category, count)

  data$fraction = data$count / sum(data$count)
  data = data[order(data$fraction), ]
  data$ymax = cumsum(data$fraction)
  data$ymin = c(0, head(data$ymax, n=-1))
  # # Make the plot


  p1 = ggplot(data, aes(fill=Category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(0, 4)) +
    theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    annotate("text", x = 0, y = 0, label = "Gender distribution among 8 seasons") +
    labs(title="") + 
    scale_fill_brewer(palette = "Dark2")
    
    
  p1
}

pie_graph(subset_data) 

##------------------------- Stacked Bar Charts -----------------------##

#===1. Stacked Bar Chart - plots the percentage gender distribution in each of the eight seasons===## 

stacked_bar<-function(vec){
  Season<-c(rep("S1", 3), rep("S2", 3), rep("S3", 3), rep("S4", 3), rep("S5", 3),
            rep("S6", 3), rep("S7", 3), rep("S8", 3))
  Category<-rep(c("Female", "Male", "Mixed Teams"), 8)
  
  vec<-tapply(vec$Gender, vec$Season, count)
  
  Total<-c()
  for (i in 1:8){
    var_seas<-vec[i]
    var<-as.data.frame(var_seas)
    value<-var[,2]
    Total<-c(Total, value)
  }  
  
  data<-data.frame(Season, Category, Total)

  # Grouped chart
  bar_grouped<-ggplot(data, aes(fill=Category, y=Total, x=Season)) + 
    geom_bar(position="dodge", stat="identity") + 
    ggtitle("Gender distribution season after season") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Stacked chart
  bar_stacked<-ggplot(data, aes(fill=Category, y=Total, x=Season)) + 
    geom_bar( stat="identity") + 
    ggtitle("Gender distribution season after season") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Stacked Percent chart
  bar_stacked_perc<-ggplot(data, aes(fill=Category, y=Total, x=Season)) + 
    geom_bar(stat="identity", position="fill") +
    scale_y_continuous(labels = percent_format()) + 
    ggtitle("Gender distribution season after season") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Stacked Percent chart displaying the percentage labels
  bar_stacked_perc_showing<-function(df){
    df$Category = factor(df$Category, levels = c("Mixed Teams", "Male", "Female"))
    df<-arrange(df, Season, desc(Category))
    df<-ddply(df, .(Season), transform, percent = Total/sum(Total)*100)
    df$label<-paste0(sprintf("%.0f", df$percent), "%")
    ggplot(df, aes(x = Season, y=Total, fill=Category))+
      geom_bar(stat="identity", width = .7, position = position_fill()) +
      geom_text(aes(label=label), position = position_fill(vjust=0.5), size=2) + 
      scale_y_continuous(labels = percent) + 
      ggtitle("Gender distribution season after season") +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
    
  bar_grouped
  
  ## Uncomment to see the total presenters in each season 
  # total_seas<-tapply(data$Total, data$Season, sum)
  # total_seas
}

stacked_bar(subset_data)

##===2. Stacked Bar Chart - plots the percentage gender/deal distribution in each of the eight seasons===##

stacked_bar_deal<-function(vec){
  Season<-c(rep("S1", 3), rep("S2", 3), rep("S3", 3), rep("S4", 3), rep("S5", 3),
            rep("S6", 3), rep("S7", 3), rep("S8", 3))
  Category<-rep(c("Female", "Male", "Mixed Teams"), 8)
  
  #get only those who got the deal
  deal_yes<-subset(vec, Deal=='Yes')
  deal_vec<-tapply(deal_yes$Gender, deal_yes$Season, count)
  
  Total<-c()
  for (i in 1:8){
    var_seas<-deal_vec[i]
    var<-as.data.frame(var_seas)
    value<-var[,2]
    Total<-c(Total, value)
  }  
  
  data<-data.frame(Season, Category, Total)
  
  # Grouped chart
  bar_grouped<-ggplot(data, aes(fill=Category, y=Total, x=Season)) + 
    geom_bar(position="dodge", stat="identity") + 
    scale_y_continuous(labels = percent_format()) + 
    ggtitle("Gender distribution season after season of those that got deals") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Stacked chart
  bar_stacked<-ggplot(data, aes(fill=Category, y=Total, x=Season)) + 
    geom_bar( stat="identity") + 
    scale_y_continuous(labels = percent_format()) + 
    ggtitle("Gender distribution season after season of those that got deals") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Stacked Percent chart
  bar_stacked_perc<-ggplot(data, aes(fill=Category, y=Total, x=Season)) + 
    geom_bar(stat="identity", position="fill") + 
    scale_y_continuous(labels = percent_format()) + 
    ggtitle("Gender distribution season after season of those that got deals") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Stacked Percent chart displaying the percentage labels  
  bar_stacked_perc_showing<-function(df){
    df$Category = factor(df$Category, levels = c("Mixed Teams", "Male", "Female"))
    df<-arrange(df, Season, desc(Category))
    df<-ddply(df, .(Season), transform, percent = Total/sum(Total)*100)
    df$label<-paste0(sprintf("%.0f", df$percent), "%")
    # head(df)
    ggplot(df, aes(x = Season, y=Total, fill=Category))+
      geom_bar(stat="identity", width = .7, position = position_fill()) +
      geom_text(aes(label=label), position = position_fill(vjust=0.5), size=4) +
      scale_y_continuous(labels = percent) + 
      # scale_y_continuous(labels = percent_format()) + 
      ggtitle("Gender distribution season after season of those that got deals") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  bar_stacked_perc_showing(data)
 
}

stacked_bar_deal(subset_data)


##--------------------------- Radar Charts ----------------------------##


##===1. Radar Chart - Distribution of the Industry across all 8 seasons===##
radar_industry<-function(vec){
  industry<-unique(vec$Industry)
  ind_vec<-tapply(vec$Industry, vec$Industry, count)
  
  data<-as.data.frame(matrix(nrow=2, ncol=14))
  values<-c()
  for (i in 1:length(ind_vec)){
    var_seas<-ind_vec[i]
    var<-as.data.frame(var_seas)
    industry<-names(ind_vec[i])
    freq<-var[1,2]
    values<-c(industry, freq)
    data<-cbind(data, values)
  }
  # return only the needed columns
  data<-data[15:28]
  column_names<-as.character(unlist(data[1,]))
  data<-data[-1,]
  colnames(data)<-column_names
  # data=as.data.frame(matrix(nrow=3, ncol=16))
  data<-unfactor(data)
  data<-apply(data, 2, function(x) {as.integer(x/sum(data)*100)})
  max<-c(rep(25,14))
  min<-c(rep(0,14))
  data<-rbind(max, min, data)
  data<-as.data.frame(data)

  radarchart(data, axistype = 0,
             #customize the polygon
             pcol = rgb(0.2,0.5,0.5,0.9), pfcol = rgb(0.2,0.5,0.5,0.5), plwd=3, seg=5,
             #customize the grid
             cglcol="grey", cglty=1, axislabcol="grey", #caxislabels=seq(0,20,2)
             #custom labels
             vlcex=1, title="Industry Distribution Among 8 Seasons")
}

radar_industry(subset_data)



##===2. Radar Chart - Distribution of the Gender/Industry across all 8 seasons===##
data_industry_gender<-function(vec){
  industry<-unique(vec$Industry)
  ind_gend_vec<-tapply(vec$Gender, vec$Industry, count)
  
  data_single<-function(ind_gend_vec){
    data<-as.data.frame(matrix(nrow=2, ncol=14))
    values<-c()
    
    for (i in 1:14) {
      ind_name<-names(ind_gend_vec[i])
      var_seas<-ind_gend_vec[i]
      var<-as.data.frame(var_seas)
      value<-var[,1]
      
      if ("Female" %in% value){
        ind<-which(var[,1]=="Female")
        fem<-var[ind,2]
      } else {
        fem<-NA
      }
      
      if ("Male" %in% value){
        ind<-which(var[,1]=="Male")
        mal<-var[ind,2]
      } else {
        mal<-NA
      }
      
      if ("Mixed Team" %in% value){
        ind<-which(var[,1]=="Mixed Team")
        mix<-var[ind,2]
      } else {
        mix<-NA
      }
      
      values<-c(ind_name, fem, mal, mix)
      data<-cbind(values, data)
    }
    
    # return only the needed columns
    data<-data[1:14]
    colnames(data)<-as.character(unlist(data[1,]))
    data<-data[-1,]
    data

  }
  
  data_single<-data_single(ind_gend_vec)
  data_single
  
}
radar_data_single<-data_industry_gender(subset_data)

#fuction for creating radar chart
radar_industry_gender<-function(radar_df){
  radar_df<-unfactor(radar_df)
  rownames(radar_df)<-c("Female", "Male", "Mixed Team")
  max<-c(rep(90,14))
  min<-c(rep(0,14))
  data<-rbind(max, min, radar_df)
  # Plot1: The default radar chart proposed by the library:
  radarchart(data)

  #Plot2: Same plot with custom features
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  radarchart( data  , axistype=0 ,
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1, seg=10,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
              #custom labels
              vlcex=0.8, title="Industry Gender Distribution Among 8 Seasons")
  legend(x=1.05, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in,
         text.col = "grey", cex=1.2, pt.cex=3)
  
}
radar_industry_gender(radar_data_single)



##===3 Radar Chart - Distribution of the Gender/Industry/Deal across all 8 seasons===##

data_industry_gender_deal<-function(vec){
  #industry<-unique(vec$Industry)
  
  #get those who got the deal
  deal_yes<-subset(vec, Deal=='Yes')
  ind_gend_deal_vec<-tapply(deal_yes$Gender, deal_yes$Industry, count)
  
  data_multiple<-function(ind_gend_deal_vec){
    data<-as.data.frame(matrix(nrow=4, ncol=14))
    values<-c()
    
    for (i in 1:14) {
      ind_name<-names(ind_gend_deal_vec[i])
      var_seas<-ind_gend_deal_vec[i]
      var<-as.data.frame(var_seas)
      value<-var[,1]
      
      if ("Female" %in% value){
        ind<-which(var[,1]=="Female")
        fem<-var[ind,2]
      } else {
        fem<-NA
      }
      
      if ("Male" %in% value){
        ind<-which(var[,1]=="Male")
        mal<-var[ind,2]
      } else {
        mal<-NA
      }
      
      if ("Mixed Team" %in% value){
        ind<-which(var[,1]=="Mixed Team")
        mix<-var[ind,2]
      } else {
        mix<-NA
      }
      
      values<-c(ind_name, fem, mal, mix)
      data<-cbind(values, data)
      
    }
    # return only the needed columns
    data<-data[1:14]
    colnames(data)<-as.character(unlist(data[1,]))
    data<-data[-1,]
    data
  }
  
  data_multiple<-data_multiple(ind_gend_deal_vec)
  data_multiple
  
}
#get the data in the right format for the radar chart
radar_data_multiple<-data_industry_gender_deal(subset_data) 

#plot the radar chart

radar_industry_gender_deal<-function(radar_df){
  radar_df<-unfactor(radar_df)
  rownames(radar_df)<-c("Female", "Male", "Mixed Team")
  max<-c(rep(50,14))
  min<-c(rep(0,14))
  data<-rbind(max, min, radar_df)
  
  # Plot1: The default radar chart proposed by the library:
  radarchart(data)
  
  #Plot2: Same plot with custom features
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  radarchart( data  , axistype=0 , 
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1, seg=9,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
              #custom labels
              vlcex=0.8, title="Industry Gender Deal Distribution Among 8 Seasons")
  legend(x=1.05, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in,
         text.col = "grey", cex=1.2, pt.cex=3)
  data
}

radar_industry_gender_deal(radar_data_multiple)
