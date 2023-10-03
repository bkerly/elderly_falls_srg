library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(arrow)
library(missForest)

#Preliminary Preparation: Importing Data and Creating Data Frames
 
 my_data <- read.csv("C:/Users/sje0303/Downloads/primary_data.csv")
  kw<-read.csv("C:/Users/sje0303/Downloads/keyword import2.csv")
  topics<-c("Fall.Energy", "Intoxication", "Fragility", "Unfamiliar.Activity",
               "Cognition", "Environment", "Consciousness")
  my_data500<-my_data[1:500,]%>%mutate(narrative=gsub("[^[:alnum:][:space:]]","",narrative))
  df<-data.frame(V1=numeric(500))
  md<-list()
  
  
#Part 1: Estimating Thematic Saturation to ensure adequate coding
        
  SatEst<-function(Topic){
    kwlist<-kw[,Topic]
    kwlist2<-kwlist[nzchar(kwlist)]
    #Loop to find first occurrence of each keyword
      for  (i in 1:length(kwlist2)) {
        df[,i]<-str_detect(my_data500$narrative, kwlist2[i])
        md[i]<-min(which(df[,i]==TRUE))
     }
    #Saturation Model Model, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7200005/
       #Part 1A: Estimate of Overall Saturation
          N<-500
          T1<-sum(df[1:500,])/500
          NTerms<-length(kwlist2)
          A<-((N-1)*T1*NTerms)/(N*T1-NTerms)
          Est<-tail(round(NTerms/A*100),1)
       #Part 1B: Theoretical Model of Saturation
           b<-(NTerms-N*T1)/((1-N)*NTerms)
           N<-seq(1:500)
           Tn<-A*b*N/(1+b*(N-1))
           sim<-data.frame(V1=seq(1:500),cumsum=Tn,Label="Simulated")
       #Part 1C: Identifying When Each Term First Occurred (For Saturation Plot)    
             md1<-do.call(rbind.data.frame, md)%>%mutate(ind=1)%>%rename(V1=1)
             md2<-data.frame(V1=seq(1,500,1))%>%left_join(md1)%>%
               mutate(ind=coalesce(ind,0))%>%
               group_by(V1)%>%
               summarize(ind=sum(ind))%>%
               ungroup()%>%
               mutate(cumsum=cumsum(ind))
             real<-md2%>%select(V1,cumsum)%>%mutate(Label="Observed")   
      
        #Part 1D: Bringing Together Simulated And Observed Data for Plot
           output<-rbind(real,sim)%>%mutate(Estimate=Est, Topic=Topic)
          return(output)
      }
  
  #Print Estimates of Saturation
    ests<-bind_rows(lapply(topics,SatEst))
    ests%>%select(Topic,Estimate)%>%distinct()

  #Print Plots of Saturation
    for (i in 1:7){
      ests2<-ests%>%filter(Topic==topics[i])
      plot<-ggplot(data=ests2)+
        geom_line(aes(x=V1,y=cumsum,group=Label, color=Label))+
        theme(axis.ticks = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5))+ 
          labs(y = "Themes Identified", x = "Records Reviewed", colour = "", title=topics[i])
        print(plot)
      }


#Part 2: Machine Learning Imputation Using Text Embeddings   
    
  #Part 2A: Creating Training Data for Imputation Model (500 first narratives which were coded) 
    #Function to Create Indicators for The Presence of Each Theme)
      lps<-function(ind){
            kwlist<-unique(kw[ind])
            kwlist<-kwlist[(kwlist=='')==FALSE]
            for  (i in 1:length(kwlist)) {
              df[,i]<-str_detect(my_data500$narrative, kwlist[i])
            }
            col<-apply(df, 1, max, na.rm=TRUE)
            return(col)
          }
    vars<-as.data.frame(lapply(c(1:7),lps))
    colnames(vars)<-topics
    vars$cpsc_case_number<-my_data500$cpsc_case_number
    
    #Retreiving Text Embeddings
      embed<-read_parquet("C:/Users/sje0303/Downloads/openai_embeddings_primary_narratives.parquet.gzip")
      embed2<-as.data.frame(t(as.data.frame(lapply(as.matrix(embed[,2]),unlist))))
      embed3<-cbind(embed[,1],embed2[,1:100])
      
  #Part 2B Merging Training Data with Text Embeddings and finalizing formatting
    embed4<-vars%>%left_join(embed3)%>%
    mutate(Fall.Energy=as.factor(Fall.Energy),
       Intoxication=as.factor(Intoxication),
       Fragility=as.factor(Fragility),
       Unfamiliar.Activity=as.factor(Unfamiliar.Activity),
       Cognition=as.factor(Cognition),
       Environment=as.factor(Environment),
       Consciousness=as.factor(Consciousness))
   
  #Part 2C: Multiple Imputation to Predict Themes in Narratives that Weren't Cocded
    embed5<-missForest(embed4)$ximp

  
#Part4; Negative Binomial Regression To Identify Strongest Risk Factors  
  #Adding Outcome Indicator to 
    Outcome<-my_data%>%mutate(Outcome=ifelse(disposition>3,1,0))%>%select(cpsc_case_number, Outcome, age, sex)
    embed6<-embed5%>%left_join(Outcome)
  #Negative Binomial Model
    exp(glm(Outcome~Fall.Energy+Intoxication+Fragility+Unfamiliar.Activity+Cognition+Environment+Consciousness+age+sex,embed6,
      family='binomial')$coefficients)

    
    