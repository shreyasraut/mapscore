getwd()
setwd('/Users/shreyas/Desktop/SARBayes')
library(XLConnect)


cleanSnowData<-function(Snow){
  Snow<-ifelse(Snow %in% c("1","3.05","blizzard","Down to 200mts","Dusting of snow on tops","flurries","light","Light","lightly","occasional", "Occasional","occasional to heavy","occasional-light","occasional/light","occassional,heavy","on gound","on ground","On ground","ON GROUND","on mountain tops","ON MOUNTIANS","on the tops","On the tops above 1200m","on tops","On tops","OVERNIGHT","periodically","Posibility","Predicted","Present","Recent deposit of fresh snow 15cm","Sleet","Sno9w on ground","snow","Snow","snow and sleet","Snow from previous few days", "Snow on ground, hard frost","Snowing","some","Some","some snow/sleet","Spring snow on the ground"        
                           ,"Sw","thick covering","To 1200m","up high","Varied","Very little", "Was falling but no settling","Windblown sleet","y", "Y" ,"yes","Yes" , "YES","Yes Deep in Alpine areas","yes, higher up mountain"  ), "Yes", Snow)
  
  Snow<-ifelse(Snow %in% c("N?A","na","NA","Ni","nil","NIL" ,"NIL AT OPERATION HEIGHT","nil`","NK","no","nO","No","NO","non","none","None","NOT AT THIS LEVEL/ SOME HAIL","Not Known" ,"not quite","Not where subject Located"), "No", Snow)
  return(Snow)
}
removeData<- function (data){
  newData<-subset(data, !Subject.Category %in% c('Aircraft-nonpowered','Aircraft','Abduction' ) )
  newData$Super.Category<-
    ifelse( newData$Subject.Category %in% c('Fisher','Boater','boater','Swimmer','Diver'), 'Water', 
            
            ifelse(newData$Subject.Category %in%  c('Vehicle','Vehicle-4wd','Motorcycle','Vehicle-Left','Mountain Biker','ATV'), 'Vehicle',
                   
                   ifelse(  newData$Subject.Category %in% c('Skier-Alpine','Snowboarder','Skier-Nordic','Snowmobile','Skier','Snowshoer','Ice Skater'), 'Snow',
                            
                            ifelse(newData$Subject.Category =='Child', 'Child',
                                   
                                   ifelse(newData$Subject.Category =='Despondent', 'Despondent',
                                          
                                          ifelse(newData$Subject.Category %in%  c('Mental Retardation','Autistic','Substance Abuse','Psychotic','Dementia'), 'Other Mental issues',
                                                 
                                                 ifelse(newData$Subject.Category =='Dementia', 'Dementia', 
                                                        
                                                        ifelse(is.na(newData$Subject.Category) , 'uncategorized', 'outDoor')
                                                 )
                                          )
                                   )
                            )
                   )
            )
    )
  
  
  
  #identifying Group
  newData$Group<-ifelse(newData$Number.Lost>1|newData$Group.Type %in% c("AC","MM","FF","MF"),'Yes','No')
  newData$Group<-ifelse(newData$Group.Type %in% c("F","M"),newData$Group,"Yes")
  #newData$Group<-ifelse(newData$Number.Lost>1,'Yes','No')
  class(newData$Age)<-"numeric"
  class(newData$Total.hours)
  newData$Total.hours<-round(newData$Total.hours, digits = 2)
  newData$Total.hours<-newData$Total.hours*24
  
  newData$Snow<-ifelse(newData$Weather %in% c('Sleet','Snow','Hail'),'Yes',newData$Snow)
  newData$Snow<-cleanSnowData(newData$Snow)
  newData$Rain<-ifelse(newData$Weather %in% c('Rain','Showers','Thunderstrom'),'Yes',newData$Rain)
  newData$Rain<-ifelse(newData$Weather =='Drizzle','No',newData$Rain)
  newData$Rain<-ifelse(newData$Rain %in% c("1","2","3","68.4","Extreme","Hail","Heavy","Intermittent","Medium","Sleet"),'Yes',newData$Rain)
  newData$Rain<-ifelse(newData$Rain %in% c("Drizzle","Light","Showers","Scattered"),'No',newData$Rain)
  #newData$Total.hours<-format(newData$Total.hours, format = "%H:%M:%S")
  
  
  refinedData <- newData
  
  refinedData <- refinedData[, -which(names(refinedData) %in% 
                                        c("Key.","Incident.Date","Data.Source","Key.  ","Mission..","City",
                                          "County","EcoRegion.Division",
                                          "Subject.Sub.Category","Clothing","Survival.training","Notify.hours",
                                          "Search.hours","LKP.Type","LKP.Coord...N.S.","LKP.Coord...E.W.",
                                          "Destination.Coord...N.S.","Destination.Coord...E.W.",
                                          "Investigative.Find","Suspended","Medical","Subject.Found.feature",
                                          "Found.Secondary","Find.Resource	Detection","Responsivenss",
                                          "Lost.Strategy","Elevation.Change..ft.","Mobile..hours.",
                                          "Distance.IPP..km.","Distance.IPP..miles.","Distance.Invest...km.",
                                          "Distance.Invest...miles.","Track.Offset..m.","Find.Coord..N.S.",
                                          "Find.Coord..E.W.","Dispersion.Angle..ÃŽ..Ã‚.","Mission.Close",
                                          "Mission.Cause","Rescue.Method","Total.Air.Hours","Total.Dog.Hours",
                                          "Total.Personnel","Total.Man.Hours","Total.Cost","Mission.Contact",
                                          "Mission.Contact..","Comments","Subject.Activity",
                                          "Number.Lost","Search.Outcome","Find.Resource","Detection",
                                          "Mobility","Subject.Category","Dispersion.Angle..Î..Â.","Sex",
                                          "Weather","Scenario","Population.Density"))]
  
  
 # kept  ,"Snow"  ,"Rain" ,"Wind..kph."
  #build can be removed
  refinedData<-subset(refinedData, Incident.Type %in% c('Search','Water')|is.na(Incident.Type))
  #refinedData<-subset(refinedData, Incident.Type %in% c('Search','Water'))
  
  
  refinedData$Subject.Status<-gsub("No Trace","DOA", refinedData$Subject.Status)
  #following statement may not be needed
  refinedData$Subject.Status<-gsub("Well","Alive", refinedData$Subject.Status)
  #changed at 11 jun 3:35pm, following statement may not be needed
  refinedData$Subject.Status<-gsub("Injured","Alive", refinedData$Subject.Status)
  #gsub("Injured", "Alive", refinedData$Subject.Status, ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE)
  
  
  keep<-refinedData$Subject.Status %in% c('DOA','Alive') | is.na(refinedData$Subject.Status)
  summary(keep)
  refinedData<-data.frame(lapply(refinedData, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
  #
  
  
  refinedData$Experience<-gsub("EXPERIENCE","GOOD", refinedData$Experience)
  refinedData$Personality<-gsub("SUICIDAL","WITHDRAWN", refinedData$Personality)
  
  
  groupDOA<-grepl("DOA", refinedData$Subject.Status, ignore.case = TRUE,fixed = FALSE, useBytes = FALSE)
  groupAlive<-grepl("ALIVE", refinedData$Subject.Status, ignore.case = TRUE,fixed = FALSE, useBytes = FALSE)
  refinedData$Subject.Status<-ifelse(groupDOA,"DOA",ifelse(groupAlive,"ALIVE",NA))
  refinedData$Group= factor(refinedData$Group)
  summary(refinedData$Group)
  
  xy<-is.na(refinedData$Subject.Status)
  refinedData$Subject.Status<-ifelse(xy,"ALIVE",refinedData$Subject.Status)
  
  
  

  refinedData$Subject.Status= factor(refinedData$Subject.Status)
  class(refinedData$Age)<-"numeric"
  class(refinedData$Total.hours)<-"numeric"
  refinedData$EcoRegion.Domain=factor(refinedData$EcoRegion.Domain)
  refinedData$Terrain= factor(refinedData$Terrain)
#  refinedData$Subject.Category= factor(refinedData$Subject.Category)
  refinedData$Incident.Type=  factor(refinedData$Incident.Type)
 
 
 #(,"Population.Density") and  scenario (,"Scenario")
 # refinedData$Population.Density= factor(refinedData$Population.Density)
  #refinedData$Scenario=  factor(refinedData$Scenario)
 
  refinedData$Group.Type = factor(refinedData$Group.Type)
  refinedData$Build = factor(refinedData$Build)
  refinedData$Physical.Fitness = factor(refinedData$Physical.Fitness)
  refinedData$Mental.Fitness = factor(refinedData$Mental.Fitness)
  refinedData$Experience = factor(refinedData$Experience)
  refinedData$Equipment = factor(refinedData$Equipment)
  refinedData$Personality = factor(refinedData$Personality)
return(refinedData)
}






Data <- readWorksheet(loadWorkbook("ISRIDclean.xls"),sheet=1)
summary(Data)
str(Data)
finalData<-removeData(Data)

library(foreign)
write.csv(finalData, file = "ISRIDfinalData.csv")
write.arff(finalData, "finalData.arff", eol = "\n", relation = deparse(substitute(finalData)))
