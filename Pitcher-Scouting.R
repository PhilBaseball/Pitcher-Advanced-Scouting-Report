library(dplyr)
library(tidyverse)
library(stringr)
library(baseballr)
#install.packages("discordr")
#library(discordr)
#install.packages("emojifont")
#library(emojifont)
library(janitor)
library(tidyr)
library(ggplot2)
library(tools)
library(pandoc)
library(tinytex)
library(latexpdf)
library(RColorBrewer)
library(readr)


#Set Directory
setwd("C:/RStudioProjects/Frontier/Pitcher Report")

#Files from the CSV folder
list_of_files <- list.files(path = "C:/RStudioProjects/Frontier/Hitter Advanced Scouting App/AIGLES_HITTERS/CSV",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

#Reading the files
df <- readr::read_csv(list_of_files)
yak_23<- df


#Import my XSLUG model !
XSLUG<- readRDS("XSLUG.rds")

#Subset the dataframe to keep only the rows without any important missing value (So the models can run)!
yak_23<- subset(yak_23, !is.na(PlateLocSide) & !is.na(PlateLocHeight)&
                  #!is.na(TaggedPitchType) & 
                  !is.na(RelSpeed)& !is.na(HorzBreak)& !is.na(InducedVertBreak)& !is.na(PitcherThrows)& !is.na(SpinAxis)& !is.na(SpinRate) & PitcherThrows %in% c("Right", "Left") & RelSpeed <105 & SpinRate < 3000)

#yak_23<- subset(yak_23, (PitcherThrows == "Left" & RelSide < 1.1) | (PitcherThrows == "Right" & RelSide > -1.10))

### PITCH MODEL ####

#Read the Pitch Classifing Model ##
Pitch_Classification_AAA<- readRDS("Pitch_ClassifierAAA.rds")

#Predict the Pitch Type (Fastballs come from the Tagger himself)
yak_23$AutoPitchType<- predict(Pitch_Classification_AAA, yak_23, "raw")
yak_23$AutoPitchType<- as.character(yak_23$AutoPitchType)
yak_23$AutoPitchType<- ifelse(yak_23$AutoPitchType == "Sweeper" , "Slider",
                                    ifelse(yak_23$AutoPitchType ==  "Knuckle Curve", "Curveball", 
                        ifelse(yak_23$AutoPitchType == "Fastball", "Four-Seam Fastball", yak_23$AutoPitchType )))
yak_23$TaggedPitchType<- ifelse(yak_23$TaggedPitchType %in% c("Fastball", "Sinker"), "Four-Seam Fastball", yak_23$AutoPitchType)


#Number of pitches
pitch<- unique(yak_23$TaggedPitchType)


### Creating Variables to help with everything ###

#Adjusting Batter Side#
yak_23$Side<- ifelse(yak_23$BatterSide == "Switch" & yak_23$PitcherThrows == "Right" , "Left",
                     ifelse(yak_23$BatterSide == "Switch" & yak_23$PitcherThrows == "Left" , "Right" ,
                            yak_23$BatterSide))
yak_23["Sww"]<- ifelse(yak_23$PitcherThrows == "Right", "RHP", ifelse(yak_23$PitcherThrows == "Left", "LHP", NA))
yak_23["Pitcher"]<- paste(yak_23$Pitcher , yak_23$Sww, sep = " - ")
yak_23<- subset(yak_23, !is.na(PlateLocSide) & !is.na(PlateLocHeight)& !is.na(TaggedPitchType) & !is.na(RelSpeed)& !is.na(HorzBreak)& !is.na(InducedVertBreak)& !is.na(PitcherThrows)& !is.na(SpinAxis)& !is.na(SpinRate))
#Pitch_Class <- readRDS("Pitch_Class.rds")
yak_23["inzone"]<- ifelse(yak_23$yt_PlateLocSide > -0.95 & yak_23$yt_PlateLocSide < 0.95 & yak_23$yt_PlateLocHeight > 1.6 & yak_23$yt_PlateLocHeight<3.5,1,0)
#yak_23["Offspeed"]<- ifelse(yak_23$TaggedPitchType =="Changeup" |yak_23$TaggedPitchType =="Splitter", 1, 0)
#yak_23["Breaking_Ball"]<- ifelse(yak_23$TaggedPitchType == "Curveball"|yak_23$TaggedPitchType == "Slider",1,0)
#yak_23["Fastball"]<- ifelse(yak_23$TaggedPitchType == "Fastball"|yak_23$TaggedPitchType == "Cutter"|yak_23$TaggedPitchType == "Sinker",1,0)
yak_23["Count"]<- paste(yak_23$Balls , yak_23$Strikes, sep = "-")
yak_23["InR"]<- ifelse(yak_23$PlateLocSide >= 0.5,1,0)
yak_23["InL"]<- ifelse(yak_23$PlateLocSide <= -0.5,1,0)
yak_23["Hit"]<- ifelse(yak_23$PlayResult == "Single"|yak_23$PlayResult == "Double"| yak_23$PlayResult == "Triple"|yak_23$PlayResult == "HomeRun",1,0)
yak_23["Out"]<- ifelse(yak_23$PlayResult == "Out"| yak_23$PlayResult == "FieldersChoice"|yak_23$PlayResult == "StrikeoutSwinging"|yak_23$PlayResult == "StrikeoutLooking"|yak_23$PlayResult == "Error",1,0)
yak_23["inplay"]<- ifelse(yak_23$PitchCall == "InPlay",1,0)
yak_23["right"]<- ifelse(yak_23$BatterSide == "Right",1,0)
yak_23["left"]<- ifelse(yak_23$BatterSide == "Left",1,0)
yak_23["hitl"]<-  ifelse(yak_23$left == 1 & (yak_23$PlayResult == "Single"|yak_23$PlayResult == "Double"| yak_23$PlayResult == "Triple"|yak_23$PlayResult == "HomeRun"),1,0)
yak_23["hitr"]<-  ifelse(yak_23$right == 1 & (yak_23$PlayResult == "Single"|yak_23$PlayResult == "Double"| yak_23$PlayResult == "Triple"|yak_23$PlayResult == "HomeRun"),1,0)
yak_23["inplayr"]<-ifelse(yak_23$right == 1 & yak_23$PitchCall == "InPlay",1,0)
yak_23["inplayl"]<-ifelse(yak_23$left == 1 & yak_23$PitchCall == "InPlay",1,0)
yak_23["outcome"]<- ifelse(yak_23$inplay == 1 | yak_23$PlayResult == "StrikeoutSwinging" | yak_23$PlayResult == "StrikeoutLooking" | yak_23$PlayResult == "Walk",1,0 )
yak_23["Walk"]<- ifelse(yak_23$PlayResult == "Walk",1,0)
yak_23["K"]<- ifelse(yak_23$PlayResult == "StrikeoutSwinging" | yak_23$PlayResult == "StrikeoutLooking",1,0)
yak_23["contact"]<- ifelse(yak_23$PitchCall == "Foul" | yak_23$PitchCall == "InPlay",1,0)
yak_23["Swing"]<- ifelse(yak_23$PitchCall== "StrikeSwinging"| yak_23$PitchCall== "Foul" | yak_23$PitchCall== "InPlay",1,0)
yak_23["Chase"]<- ifelse((yak_23$inzone == 0 & yak_23$PitchCall == "StrikeSwinging")| (yak_23$inzone == 0 & yak_23$PitchCall == "Foul") | (yak_23$inzone == 0 & yak_23$PitchCall == "InPlay"),1,0)
yak_23["Take"]<- ifelse(yak_23$PitchCall== "StrikeCalled"| yak_23$PitchCall == "BallCalled",1,0)
yak_23["firstpitch"]<- ifelse(yak_23$Strikes == 0 & yak_23$Balls == 0,1,0)
yak_23["firststrike"]<- ifelse(yak_23$firstpitch == 1 & yak_23$inzone == 1,1,0)
yak_23["inzone"]<- ifelse(yak_23$yt_PlateLocSide > -0.95 & yak_23$yt_PlateLocSide < 0.95 & yak_23$yt_PlateLocHeight > 1.6 & yak_23$yt_PlateLocHeight<3.5,1,0)
yak_23["outzone"]<- ifelse(yak_23$inzone == 0,1,0)
yak_23["BeforeTwoS"]<- ifelse(yak_23$Strikes %in% c(0,1),1,0)
yak_23["Swing"]<- ifelse(yak_23$PitchCall== "StrikeSwinging"| yak_23$PitchCall== "Foul" | yak_23$PitchCall== "InPlay",1,0)
yak_23["Swingbtwostrike"]<- ifelse(yak_23$Strikes %in% c(0,1) & yak_23$Swing == 1,1,0)
yak_23["Chase"]<- ifelse((yak_23$inzone == 0 & yak_23$PitchCall == "StrikeSwinging")| (yak_23$inzone == 0 & yak_23$PitchCall == "Foul") | (yak_23$inzone == 0 & yak_23$PitchCall == "InPlay"),1,0)
yak_23["Take"]<- ifelse(yak_23$PitchCall== "StrikeCalled"| yak_23$PitchCall == "BallCalled",1,0)
yak_23["Offspeed"]<- ifelse(yak_23$TaggedPitchType =="Changeup" |yak_23$TaggedPitchType =="Splitter", 1, 0)
yak_23["Breaking_Ball"]<- ifelse(yak_23$TaggedPitchType == "Curveball"|yak_23$TaggedPitchType == "Slider",1,0)
yak_23["Fastball"]<- ifelse(yak_23$TaggedPitchType == "Fastball"|yak_23$TaggedPitchType == "Cutter"|yak_23$TaggedPitchType == "Sinker",1,0)
yak_23["Pitch"]<- ifelse(yak_23$Fastball == 1, "Fastballs" , 
                         ifelse(yak_23$Offspeed == 1 , "OffSpeed",
                                "Breaking Ball"))
yak_23["Hardhit"]<- ifelse(yak_23$ExitSpeed >= 90,1,0)
yak_23["Barrel"]<- ifelse(yak_23$ExitSpeed >= 95 & yak_23$Angle >= 25 & yak_23$Angle <= 30,1,0)
yak_23["Count"]<- paste(yak_23$Balls , yak_23$Strikes, sep = "-")
yak_23["Hit"]<- ifelse(yak_23$PlayResult == "Single"|yak_23$PlayResult == "Double"| yak_23$PlayResult == "Triple"|yak_23$PlayResult == "HomeRun",1,0)
yak_23["Out"]<- ifelse(yak_23$PlayResult == "Out"| yak_23$PlayResult == "FieldersChoice"|yak_23$PlayResult == "StrikeoutSwinging"|yak_23$PlayResult == "StrikeoutLooking"|yak_23$PlayResult == "Error",1,0)
yak_23["inplay"]<- ifelse(yak_23$PitchCall == "InPlay",1,0)
yak_23["right"]<- ifelse(yak_23$PitcherThrows == "Right",1,0)
yak_23["left"]<- ifelse(yak_23$PitcherThrows == "Left",1,0)
yak_23["hitl"]<-  ifelse(yak_23$left == 1 & (yak_23$PlayResult == "Single"|yak_23$PlayResult == "Double"| yak_23$PlayResult == "Triple"|yak_23$PlayResult == "HomeRun"),1,0)
yak_23["hitr"]<-  ifelse(yak_23$right == 1 & (yak_23$PlayResult == "Single"|yak_23$PlayResult == "Double"| yak_23$PlayResult == "Triple"|yak_23$PlayResult == "HomeRun"),1,0)
yak_23["inplayr"]<-ifelse(yak_23$right == 1 & yak_23$PitchCall == "InPlay",1,0)
yak_23["inplayl"]<-ifelse(yak_23$left == 1 & yak_23$PitchCall == "InPlay",1,0)
yak_23["outcome"]<- ifelse(yak_23$inplay == 1 | yak_23$PlayResult == "StrikeoutSwinging" | yak_23$PlayResult == "StrikeoutLooking" | yak_23$PlayResult == "Walk",1,0 )
yak_23["Walk"]<- ifelse(yak_23$PlayResult == "Walk",1,0)
yak_23["K"]<- ifelse(yak_23$PlayResult == "StrikeoutSwinging" | yak_23$PlayResult == "StrikeoutLooking",1,0)
yak_23["contact"]<- ifelse(yak_23$PitchCall == "Foul" | yak_23$PitchCall == "InPlay",1,0)
yak_23["FirstPitch"]<- ifelse(yak_23$Strikes == 0 & yak_23$Balls == 0,1,0)
yak_23["firstswinging"]<- ifelse(yak_23$FirstPitch == 1 & yak_23$Swing == 1, 1,0)
yak_23["FirstPSwing"]<- ifelse(yak_23$FirstPitch == 1 & yak_23$Swing == 1,1,0)
yak_23["firststrike"]<- ifelse(yak_23$Strikes == 0 & yak_23$inzone == 1, 1,0)
yak_23["firststrikeswing"]<- ifelse(yak_23$firststrike == 1 & yak_23$Swing == 1,1,0) 
yak_23["ZContact"]<- ifelse(yak_23$contact == 1 & yak_23$inzone == 1,1,0)
yak_23["OContact"]<- ifelse(yak_23$contact == 1 & yak_23$inzone == 0,1,0)
yak_23["ZSwing"]<- ifelse(yak_23$inzone == 1 & (yak_23$PitchCall== "StrikeSwinging"|yak_23$PitchCall== "Foul" | yak_23$PitchCall== "InPlay"),1,0)
yak_23["AB"]<- ifelse(yak_23$PlayResult %in% c("FieldersChoice", "Triple" ,"StrikeoutLooking","HomeRun", "Error" ,"StrikeoutSwinging", "Double","Out","Single"),1,0)
yak_23["SLUG"]<- ifelse(yak_23$PlayResult == "Single", 1,
                        ifelse(yak_23$PlayResult == "Double",2,
                               ifelse(yak_23$PlayResult == "Triple",3,
                                      ifelse(yak_23$PlayResult == "HomeRun",4,0))))
yak_23["SwingF"]<- ifelse(yak_23$Swing == 1 , "Swing" , "Take")
yak_23["Sww"]<- ifelse(yak_23$BatterSide == "Right", "RHB", ifelse(yak_23$BatterSide == "Left", "LHB", "Switch"))
yak_23["Batter"]<- paste(yak_23$Batter , yak_23$Sww, sep = " - ")
yak_23$XSLUG<- ifelse(yak_23$PitchCall == "InPlay" &(is.na(yak_23$SLUG) | is.na(yak_23$ExitSpeed) | is.na(yak_23$Angle) | is.na(yak_23$HitSpinRate)),predict(XSLUG, yak_23), NA)
yak_23["HardHit"]<- ifelse(yak_23$PitchCall == "InPlay" & yak_23$ExitSpeed >= 90,1,0)
yak_23["Barrel"]<- ifelse(yak_23$ExitSpeed >= 95 & yak_23$Angle >= 25 & yak_23$Angle <= 30,1,0)
#yak_23["Tilt"]<- as.factor(yak_23$Tilt)
yak_23["Count"]<- paste(yak_23$Balls , yak_23$Strikes, sep = "-")
yak_23['FB']<- ifelse(yak_23$TaggedPitchType %in% c("Four-Seam Fastball", "Fastball"), 1,0 )
yak_23['SNK']<- ifelse(yak_23$TaggedPitchType == "Sinker", 1,0 )
yak_23['CT']<- ifelse(yak_23$TaggedPitchType == "Cutter", 1,0 )
yak_23['CB']<- ifelse(yak_23$TaggedPitchType == "Curveball", 1,0 )
yak_23['CH']<- ifelse(yak_23$TaggedPitchType == "Changeup", 1,0 )
yak_23['SL']<- ifelse(yak_23$TaggedPitchType == "Slider", 1,0 )
yak_23['SPL']<- ifelse(yak_23$TaggedPitchType == "Splitter", 1,0 )
yak_23['SLV']<- ifelse(yak_23$TaggedPitchType == "Slurve", 1,0 )
yak_23['SWP']<- ifelse(yak_23$TaggedPitchType == "Sweeper", 1,0 )


# StrikeZone Dimensions #
Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

# This is to Make a 3x3 Strike Zone (Vertical and Horizontal Lines in Zone)
Width <- (Right - Left) / 3
Height <- (Top - Bottom) / 3
#Colors for the HeatMaps
heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1))(16)

#View the Teams
unique(yak_23$PitcherTeam)

#Changing mispelled names
yak_23$Pitcher<- ifelse(yak_23$Pitcher %in% c("Brandon Backmon - RHP","Brandon Backman - RHP", "Brendon Backman - RHP"), "Brandon Backman - RHP",
                 ifelse(yak_23$Pitcher %in% c("Aaron Dana - RHP" , "Aaron Dona - RHP"), "Aaron Dona - RHP",
                 ifelse(yak_23$Pitcher %in% c("Frank Moscatiello - RHP",  "Frank Mascatiello - RHP"), "Frank Moscatiello - RHP",
                 ifelse(yak_23$Pitcher %in% c( "Abdiel  Saldana - RHP",   "Abdiel Saldana - RHP"),  "Abdiel Saldana - RHP",
                 ifelse(yak_23$Pitcher %in% c("Jackson  Sigman - RHP"),  "Jackson Sigman - RHP",
                 ifelse(yak_23$Pitcher %in% c("Alfredo Ruiz - RHP", "Alfredo Ruiz - LHP"), "Alfredo Ruiz - LHP",
                 ifelse(yak_23$Pitcher %in% c("Yuhi Sako - RHP" , "Yuhi Sake - RHP" ), "Yuhi Sako - RHP" ,
                 ifelse(yak_23$Pitcher %in% c("Jalon Long - RHP", "Jalon Tyson-long - RHP" ), "Jalon Tyson-Long - RHP",
                 ifelse(yak_23$Pitcher %in% c("Cole  Roland - RHP"), "Cole Roland - RHP", 
                 ifelse(yak_23$Pitcher %in% c("Christopher Mormile - RHP"), "Chris Mormile - RHP",
                 ifelse(yak_23$Pitcher %in% c("Zack Morris - LHP"), "Zach Morris - LHP",
                 ifelse(yak_23$Pitcher %in% c("Charlie  Neuweiler - RHP"), "Charlie Neuweiler - RHP",
                        yak_23$Pitcher))))))))))))


# rename the dataset and filter for the team we want
yakker <- yak_23 %>%
  # FILTER
  filter(PitcherTeam %in% c("2024 sussex county miners", "Sussex county miners" ))
  # CHANGE DATE WHEN NECESSARY


# ------------------------
# set a factor to manually order the pitch types
pitch
yakker$TaggedPitchType <- factor(yakker$TaggedPitchType, levels = c("Four-Seam Fastball","Fastball" ,"Sinker", "Cutter","Curveball", "Slider", "Changeup", "Splitter", 'Slurve', 'Sweeper'))

#each pitcher that pitched for this team
pitchers <- unique(yakker$Pitcher)
#Remove pitchers if needed
pitchers<- pitchers[c(30)]
pitchers

# LOOP ---------

# For each pitcher in the dataset, run through the following code
for (pitcher in pitchers) {
  # Filter the data for the current pitcher
  pitcher_data <- yak_23[yak_23$Pitcher == pitcher, ]
  

  # Generate the summary / pitch characteristics table
  game_summary_table <- 
    pitcher_data %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") )%>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                     'Usage' = n(),
                     'Usage%' = n(),
                     'Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
                     'VeloMax' = round(max(RelSpeed, na.rm = TRUE),1),
                     'Tilt' = round(mean(SpinAxis, na.rm = TRUE),0),
                     'Time' = sapply(`Tilt`, function(x) if (is.na(x)){return(NA)}
                                     else if(x > 180 & x <= 360){(x/30)-6}
                                     else if(x == 180){12}
                                     else{(x/30)+6}),
                     'HH' = as.integer(Time),
                     'HH' = sapply(HH, function(x) if (is.na(x)){return(NA)}
                                   else if(x == 0){x+12}
                                   else if(x > 12){x-12}
                                   else{x+0}),
                     "MM" = formatC(round((Time%%1)*60, digits = 0), width = 2, flag = "0"),
                     #'Tilt' = paste0(HH,":", MM),
                     'Spin' = round(mean(SpinRate, na.rm = TRUE),0),
                     #'SpinEff%' = round(mean(yt_Efficiency, na.rm= TRUE),0),
                     'Whiff% ' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                          sum(PitchCall%in%c("StrikeSwinging","Foul", 
                                                             "InPlay")),3)*100,
                     'O-Swing%' = round(sum(Chase, na.rm=T)/sum(outzone),2)*100,
                     "Z-Swing%" = round(sum(ZSwing, na.rm=T)/sum(inzone),2)*100,
                   
                    # "O-Contact%" = round(sum(OContact,na.rm=TRUE)/
                                            # sum(Chase,na.rm=T),2)*100,
                     "Z-Contact%" = round(sum(ZContact, na.rm =TRUE)/
                                             sum(ZSwing, na.rm=T),2)*100,
                     'Swing%' = round(sum(Swing, na.rm=T)/n(),2)*100,
                     "Chase%" = round(sum(Chase, na.rm=T)/n(),2)*100,
                    
                     #"Batting AVG" = round(sum(Hit,na.rm=TRUE)/
                                             #sum(AB,na.rm=T),3),
                    #'Ext' = round(mean(Extension, na.rm = TRUE),1)
    ) %>%
    mutate(`Usage%` = round(`Usage%`/sum(`Usage%`),3)*100) %>%
    dplyr::select(-Usage,-Time,-HH,-MM)
  
  game_summary_table <- game_summary_table[order(game_summary_table$'No.',decreasing = TRUE),]
  
  
  # Generate the pitch usage table
  pitch_usage_table <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") )%>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('#' = n(),
                     'Use%' = n(),
                     "1P" = sum(PitchofPA == 1, na.rm = TRUE),
                     "1P%" = sum(`1P`, na.rm = TRUE),
                     '2K' = sum(Strikes == 2, na.rm = TRUE),
                     '2K%' = sum(`2K`, na.rm = TRUE),
                     'Strk%' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"), na.rm = TRUE)/n(),3)*100,
                     'Whiff%' = round(sum(PitchCall %in% c("StrikeSwinging"), na.rm = TRUE)/
                                        sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay"), na.rm = TRUE),3)*100 
    ) %>%
    mutate(`Use%` = round(`Use%`/sum(`Use%`),3)*100,
           `1P%` = round(`1P%`/sum(`1P%`),3)*100,
           `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
    dplyr::select(-`1P`, -`2K`) 
  
  # Generate the stat table
  game_stats <- 
    pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") )%>%
    
    summarise('IP' = round( (sum(OutsOnPlay, na.rm = TRUE))/3, 1),
              'BF' = n_distinct(Inning, Batter, PAofInning),
              'K' = sum(KorBB =="Strikeout"),
              'BB' = sum(KorBB =="Walk"),
              'HBP' = sum(PlayResult == 'HitByPitch'),
              'BIP' = sum(PitchCall == 'InPlay') ,
              'H' = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
              'XBH' = sum(PlayResult %in% c('Double','Triple','HomeRun')),
              'R' = sum(RunsScored, na.rm = TRUE)
    )

  # PITCH USAGE VS RHH
  usage_r <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") )%>%
    filter(Side == 'Right') %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                     'Usage %' = n(),
                     "1P" = sum(PitchofPA == 1),
                     "1P%" = sum(`1P`),
                     '2K' = sum(Strikes == 2),
                     '2K%' = sum(`2K`),
                     'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                     'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                         sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay")),3)*100 
    ) %>%
    mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100,
           `1P%` = round(`1P%`/sum(`1P%`),3)*100,
           `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
    dplyr::select(-`1P`, -`2K`)
  
  usage_r <- usage_r[order(usage_r$'No.',decreasing = TRUE),]
  

  # USAGE VS LHH
  usage_l <- pitcher_data  %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") )%>%
    filter(Side == 'Left') %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                     'Usage %' = n(),
                     "1P" = sum(PitchofPA == 1),
                     "1P%" = sum(`1P`),
                     '2K' = sum(Strikes == 2),
                     '2K%' = sum(`2K`),
                     'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                     'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                         sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay")),3)*100 
    ) %>%
    mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100,
           `1P%` = round(`1P%`/sum(`1P%`),3)*100,
           `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
    dplyr::select(-`1P`, -`2K`)
  
  usage_l <- usage_l[order(usage_l$'No.',decreasing = TRUE),]
  

  

  
  # Generate the pitch movement plot
  pitch_movement_plot <- 
    ggplot(data =  
             pitcher_data %>%
             dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                                     Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") ),
           
           
           aes(x = -1 * HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
    labs(title = "Pitch Movement" , subtitle = "Catcher's View" ,color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" ) + 
    xlim(-25, 25) + ylim(-25, 25) +
    geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
    geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
    geom_point(size =4, alpha = .75) +
    # we manually set the pitch colors so that they are uniform across each plot and tables
    scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SNK' = '#f47b20',  'SL'='cornflowerblue',
                                  'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', "SLV" = "pink", "SWP" = "cyan", "SCRW"= "green", "FRK"= "darkgrey", "2S" = "darkred")) +  
    theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_text(size = 8))
  
  

  
  pvp_game_plot <- 
    ggplot(data = pitcher_data %>%
             dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                                     Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") ),
           aes(x = RelSide*(-1), y = RelHeight, color = TaggedPitchType)) +
    xlim(-5,5) + ylim(0,10) + labs(color = "", title = "Pitch Release Catcher View") +
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
    #geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
    #geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
    #geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
    #geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
    #geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
    geom_point(alpha = 2, na.rm = TRUE) +
    scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SNK' = '#f47b20',  'SL'='cornflowerblue',
                                  'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', "SLV" = "pink", "SWP" = "cyan", "SCRW"= "green", "FRK"= "darkgrey", "2S" = "darkred")) +
    theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", legend.text = element_text(size = 12), axis.title = element_blank())

  

  
  # Pitch location plot vs rhh with a facet wrap on one of the created columns
  right<-  pitcher_data %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") ) %>% filter(Side== "Right" )
  if (nrow(right) > 5) {
    plp_rhh <- ggplot(data = right, 
           aes(x = PlateLocSide * -1 , y = PlateLocHeight)) +
    xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Frequency VS RHB" ), subtitle = "Catcher's View" )+
      #stat_density_2d(aes(fill = ..level..), geom = "polygon")+
      #scale_fill_distiller(palette="RdBu", direction=-1)+
      stat_density2d_filled(contour_var = "ndensity")  +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      geom_point(size = 0.25)+
      #stat_density2d()+
    geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
    geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
    geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
    geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
    
    # Horizontal Lines (Bottom Inner, Top Inner)
    geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
    geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
    geom_plate()+
    # Vertical Lines (Left Inner, Right Inner)
    geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
    geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
    #scale_color_gradient2(midpoint=0.5,low="blue", mid="white",high="red")+
    theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", axis.title = element_blank())  +
    theme(strip.text = element_text(size = 7, face = 'bold'),
          axis.text.x=element_blank(), #remove x axis labels
          axis.text.y=element_blank(),  #remove y axis labels
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + facet_wrap(~factor(TaggedPitchType, levels=c('FB', 'SNK', 'CT', 'SL',"CB","CH","SPL", "SWP", "SLV")))
  } else{
    plp_rhh <- ggplot(data = right, 
                      aes(x = PlateLocSide * -1 , y = PlateLocHeight)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Frequency VS RHB" ), subtitle = "Catcher's View" )+
      #stat_density_2d(aes(fill = ..level..), geom = "polygon")+
      #scale_fill_distiller(palette="RdBu", direction=-1)+
      #stat_density2d_filled(contour_var = "ndensity")  +
      #scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      geom_point(size = 1)+
      geom_plate()+
      
      #stat_density2d()+
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      #scale_color_gradient2(midpoint=0.5,low="blue", mid="white",high="red")+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) + facet_wrap(~factor(TaggedPitchType, levels=c('FB', 'SNK', 'CT', 'SL',"CB","CH","SPL", "SWP", "SLV")))
  }
  
  # Pitch location plot vs lhh with a facet wrap on one of the created columns
  
  
  left<-  pitcher_data %>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") ) %>%
  filter(Side== "Left" )
  
  #plp_lhh <- 
    
    if (nrow(left) > 5) {
    plp_lhh<- ggplot(data = left,
           aes(x = PlateLocSide * -1 , y = PlateLocHeight)) +
    stat_density2d_filled(contour_var = "ndensity")  +
    scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
    #stat_density_2d(aes(fill = ..level..), geom = "polygon")+
    #scale_fill_distiller(palette="RdBu", direction=-1)+
    geom_point(size = 0.25)+
    #stat_density2d()+
    geom_plate()+
    xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Frequency VS LHB" ) , subtitle = "Catcher's View" )+
    geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
    geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
    geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
    geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
    
    # Horizontal Lines (Bottom Inner, Top Inner)
    geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
    geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
    
    # Vertical Lines (Left Inner, Right Inner)
    geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
    geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
    
  
    #scale_fill_distiller(palette="RdBu", direction=-1)+
    #geom_density_2d(colour = "black")+
    #scale_color_gradient2(midpoint=0.5,low="blue", mid="white",high="red")+
    theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none",axis.title = element_blank())  +
    theme( strip.text = element_text(size = 7, face = 'bold'),
          axis.text.x=element_blank(), #remove x axis labels
          axis.text.y=element_blank(),  #remove y axis labels
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + facet_wrap(~ factor(TaggedPitchType, levels=c('FB', 'SNK', 'CT', 'SL',"CB","CH","SPL", "SWP", "SLV")))
  
    } else {
    plp_lhh<- ggplot(data = left,
           aes(x = PlateLocSide * -1 , y = PlateLocHeight)) +
      #stat_density2d_filled(contour_var = "ndensity")  +
      #scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      #stat_density_2d(aes(fill = ..level..), geom = "polygon")+
      #scale_fill_distiller(palette="RdBu", direction=-1)+
      geom_point(size = 1)+
      #stat_density2d()+
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Frequency VS LHB" ) , subtitle = "Catcher's View" )+
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      geom_plate()+
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      
      
      #scale_fill_distiller(palette="RdBu", direction=-1)+
      #geom_density_2d(colour = "black")+
      #scale_color_gradient2(midpoint=0.5,low="blue", mid="white",high="red")+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none",axis.title = element_blank())  +
      theme( strip.text = element_text(size = 7, face = 'bold'),
             axis.text.x=element_blank(), #remove x axis labels
             axis.text.y=element_blank(),  #remove y axis labels
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank()) + facet_wrap(~ factor(TaggedPitchType, levels=c('FB', 'SNK', 'CT', 'SL',"CB","CH","SPL", "SWP", "SLV")))
    
  }
  
  
  
  
  
  # CREATES ANOTHER USAGE BREAKDOWN
  
  bd <- pitcher_data%>% filter( !is.na(Side))%>%
    dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, 'Four-Seam Fastball' = "FB", Curveball = 'CB', Sinker = 'SNK', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Slurve = "SLV", Sweeper = "SWP", Screwball = "SCRW", Forkball = "FRK", "Two-Seam Fastball" = "FB", Fastball = "FB") ) %>%
    mutate(Count = paste0(Balls,"-",Strikes),
           filter_2 = ifelse(Count == "0-0", 'FirstPitch',
                      ifelse(Strikes == 2, '2 Strikes',
                      ifelse(Count %in% c('1-0','2-0','3-0','2-1','3-1'), 'PitcherBehind',
                      ifelse(Strikes == 2 & Balls == 3, "FullCount", 
                      ifelse(Count %in% c('0-1','0-2', '1-2' ), 'PitcherAhead', ''
                                           ))  )) )   
    )%>%
    filter(filter_2 != '') %>%
    group_by(TaggedPitchType, filter_2, Side) %>%
    summarise(P = n() ) %>%
    group_by(filter_2, Side) %>%
    mutate(percentage = round(P / sum(P),3)*100) %>%
    mutate(Side = gsub("Left",'LHH',Side),
           Side = gsub('Right','RHH',Side))
  
  
  
  breakdown<-ggplot(bd %>%
                      mutate(filter_2 = factor(filter_2, levels = c('FirstPitch', '2 Strikes', 'PitcherAhead', 'PitcherBehind', "FullCount") )), 
                    aes(x = "", y = percentage, fill = TaggedPitchType)) +
    geom_col(color = "black") +
    geom_text(aes(label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), fontface = 'bold', color = 'white', size = 3)+
    theme_void()+
    theme(strip.text = element_text(size = 11, face = 'bold'))+
    facet_wrap(Side~filter_2, nrow=2) + 
    #theme(legend.position="none")+
    scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SNK' = '#f47b20',  'SL'='cornflowerblue',
                                 'CT' = 'gold',  'CH'='violet', 'SPL' = 'black', "SLV" = "pink", "SWP" = "cyan"))

  
  Pitches<- as.data.frame(pitch)
  
  
  CountR<- pitcher_data %>% filter(Side == "Right") %>%  group_by('Count' = Count) %>%
  dplyr ::  summarize("No."= n(),
                      "FB %" = round(sum(FB)/n(),3)*100,
                     # "SNK %" = round(sum(SNK)/n(),3)*100,
                      "CT %" = round(sum(CT)/n(),3)*100,
                      "CB %" = round(sum(CB)/n(),3)*100,
                      "CH %" = round(sum(CH)/n(),3)*100,
                      "SL %" = round(sum(SL)/n(),3)*100,
                      "SPL %" = round(sum(SPL)/n(),3)*100,
                      "SLV %" = round(sum(SLV)/n(),3)*100,
 #                     "SWP %" = round(sum(SWP)/n(),3)*100
                      
  ) 
  
  
  CountL<- pitcher_data %>% filter(Side == "Left") %>%  group_by('Count' = Count) %>%
  dplyr ::  summarize("No."= n(),
                      "FB %" = round(sum(FB)/n(),3)*100,
                    #  "SNK %" = round(sum(SNK)/n(),3)*100,
                      "CT %" = round(sum(CT)/n(),3)*100,
                      "CB %" = round(sum(CB)/n(),3)*100,
                      "CH %" = round(sum(CH)/n(),3)*100,
                      "SL %" = round(sum(SL)/n(),3)*100,
                      "SPL %" = round(sum(SPL)/n(),3)*100,
                      "SLV %" = round(sum(SLV)/n(),3)*100,
                     # "SWP %" = round(sum(SWP)/n(),3)*100
                      
  ) 
  
  
  # SET THE PARAMETERS FOR THE R MARKDOWN FILE
  params <- list(
    game_summary_table = game_summary_table,
    pitch_usage_table = pitch_usage_table,
    pitch_movement_plot = pitch_movement_plot,
    pitcher = pitcher,
    game_stats = game_stats,
    usage_r = usage_r,
    #stats_vs_r = stats_vs_r,
    usage_l = usage_l,
    #stats_vs_l= stats_vs_l,
    #date = game_stats$Date[1],
    opponent = pitcher_data$BatterTeam[1],
    pvp_game_plot = pvp_game_plot,
    Pitches = Pitches,
    plp_lhh = plp_lhh,
    plp_rhh = plp_rhh,
    breakdown =breakdown,
    CountR = CountR,
    CountL = CountL
    
  )
  
  # SETS THE DATE FOR THE FILE NAME
 # file_date <- format(as.Date(pitcher_data$Date[1]), "%m-%d")
  
  # Knit the R Markdown file to PDF
  rmarkdown::render(input = "C:/RStudioProjects/Frontier/Pitcher Report/Pitcher_Advanced_Scouting.Rmd",
                    output_file = paste0("C:/RStudioProjects/Frontier/Pitcher Report/PDFScoutingReports/",pitcher, " report",".pdf"),
                    params = params)
}


