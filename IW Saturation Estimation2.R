library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(episensr)

#Import Data
  my_data <- read.csv("C:/Users/sje0303/Downloads/primary_data.csv")
  kw<-read.csv("C:/Users/sje0303/Downloads/keyword import2.csv")
  topics<-c("Fall.Energy", "Intoxication", "Fragility", "Unfamiliar.Activity",
               "Cognition", "Environment", "Consciousness")

  
#Part 1: Estimating Thematic Saturation  
        
      SatEst<-function(Topic){
      kwlist<-kw[,Topic]
      kwlist2<-kwlist[nzchar(kwlist)]
        
      #Loop to search each note for each keywords.  Outputs True/False matrix with one row per note and one column per keyword
         md<-list()
         df<-data.frame(V1=numeric(nrow(my_data)))
          for  (i in 1:length(kwlist2)) {
            
              df[,i]<-str_detect(my_data$narrative, kwlist2[i])
              md[i]<-min(which(df[,i]==TRUE))
              print(i)
          }
        
          
      #Mathematical Model, https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7200005/
         #Part 1: Estimate of Overall Saturation
         
         N<-500
         T1<-sum(df[1:500,])/500
         NTerms<-length(kwlist2)
         A<-((N-1)*T1*NTerms)/(N*T1-NTerms)
         Est<-tail(round(NTerms/A*100),1)
      
         #Part 2: Theoretical Model of Saturation
         b<-(NTerms-N*T1)/((1-N)*NTerms)
         N<-seq(1:500)
         Tn<-A*b*N/(1+b*(N-1))
         sim<-data.frame(V1=seq(1:500),cumsum=Tn,Label="Simulated")
         
      #Plot of observed data vs model      
         #Dataframe indicating at which note each keyword was first observed 
           md1<-do.call(rbind.data.frame, md)%>%mutate(ind=1)%>%rename(V1=1)
           md2<-data.frame(V1=seq(1,500,1))%>%left_join(md1)%>%
             mutate(ind=coalesce(ind,0))%>%
             group_by(V1)%>%
             summarize(ind=sum(ind))%>%
             ungroup()%>%
             mutate(cumsum=cumsum(ind))
           real<-md2%>%select(V1,cumsum)%>%mutate(Label="Observed")   
      
        #Bringing Together Simulated And Observed Data for Plot
        full<-rbind(real,sim)  
        plot<-ggplot(data=full)+
           geom_line(aes(x=V1,y=cumsum,group=Label, color=Label))+
           theme(axis.ticks = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.background = element_blank(),
                 plot.title = element_text(hjust = 0.5)
           )+ 
           labs(y = "Keywords Identified", x = "Records Reviewed", colour = "", title=Topic)
         
        
      output<-list(Topic,Est,plot)  
      return(output)
      }
  

v1<-SatEst("Fall.Energy")
v2<-SatEst("Intoxication")
v3<-SatEst("Fragility")
v4<-SatEst("Unfamiliar.Activity")
v5<-SatEst("Cognition")
v6<-SatEst("Environment")
v7<-SatEst("Consciousness")

#Print Saturation
data.frame(Topic=topics, Saturation=c(v1[[2]],v2[[2]],v3[[2]],v4[[2]],
                                      v5[[2]], v6[[2]],v7[[2]]))
#Print Plots
grid.arrange(v1[[3]], v2[[3]], v3[[3]], v4[[3]],
             v7[[3]], v6[[3]], v7[[3]],nrow = 7)




#Part 2: Epi #Regression Model

  #Function to Create Indicators for The Presence of Each Keyword
  lps<-function(ind){
      kwlist<-unique(kw[ind])
      kwlist<-kwlist[(kwlist=='')==FALSE]
      md<-list()
      df<-data.frame(V1=numeric(nrow(my_data)))
 
              for  (i in 1:length(kwlist)) {
          
          df[,i]<-str_detect(my_data$narrative, kwlist[i])
          md[i]<-min(which(df[,i]==TRUE))
          print(i)
          
        }
    
      col<-apply(df, 1, max, na.rm=TRUE)
      return(col)
  }


Fall.Energy<-lps(1)
Intoxication<-lps(2)
Fragility<-lps(3)
Unfamiliar.Activity<-lps(4)
Cognition<-lps(5)
Environment<-lps(6)
Consciousness<-lps(7)

#Outcome Indicator
Outcome<-ifelse(my_data$disposition>3,1,0)

m1 <- glm.nb(Outcome ~ Fall.Energy+Intoxication+Fragility+Unfamiliar.Activity+
                               Cognition+Environment+Consciousness+NLength)

df<-data.frame(Fall.Energy,Intoxication,Fragility,Unfamiliar.Activity,
                Cognition,Environment,Consciousness,Outcome)



QBA<-function(i,Exposure,Label){
  
sens<-i[[2]]/100
Case_Exposed<-df[Exposure==1 & Outcome==1,]%>%nrow()
Case_Unexposed<-df[Exposure==0 & Outcome==1,]%>%nrow()
Control_Exposed<-df[Exposure==1 & Outcome==0,]%>%nrow()
Control_Unexposed<-df[Exposure==0 & Outcome==0,]%>%nrow()


g<-episensr::misclassification(matrix(c(Case_Exposed, Case_Unexposed, Control_Exposed, Control_Unexposed),
                         dimnames = list(c("Case", "Control"),
                                         c("Exposure +", "Exposure - ")),
                         nrow = 2, byrow = TRUE),
                  type = "exposure",
                  bias_parms = c(sens, sens, 0.97, 0.97))

obs<-t(g$obs.measures[1,])
adj<-t(g$adj.measures[2,])

output2<-cbind(Label,obs,adj)
colnames(output2)<-c("Label", "UnadjustedRR", "2.5%" ,"97.5%", "AdjustedRR", "2.5%" ,"97.5%")
return(output2)
}

QBA(v1,Fall.Energy,"Fall.Energy")
QBA(v2,Fall.Energy,"Intoxication")
QBA(v3,Fragility,"Fragility")
QBA(v4,Unfamiliar.Activity,"Unfamiliar.Activity")
QBA(v5,Cognition,"Cognition")
QBA(v6,Environment,"Environment")
QBA(v7,Consciousness,"Consciousness")
