library(plyr)
library(reshape)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(binhf)


#set working directory
#setwd("~/Desktop/Repro_Methods_Final_Pipeline/Repro_Methods_Project/")
#baseDir<-"~/Desktop/Repro_Methods_Final_Pipeline/Repro_Methods_Project/"
baseDir<-getwd()
print(baseDir)
setwd(baseDir)

#############################################################
################# OPEN AND COMPILE FILES ####################
#############################################################

#get a list of subject folders
setwd(paste0(baseDir,'/data/subs'))
subs<-list.files()
#loop through and read in data
for (s in subs){
  setwd(paste0(baseDir,'/data/subs/',s))
  Pred_Files<-list.files(pattern=c("Prediction"))
  if(s==subs[1]){
    pred_data<-read.csv(Pred_Files)
  }
  else{
    temp<-read.csv(Pred_Files)
    pred_data<-rbind(pred_data, temp)
  }
}

#####################################################
################## PRED DATA ########################
#####################################################

#rename columns
colnames(pred_data)[30:32]<- c("resp","acc","rt")

#overall acc
acc<-cast(pred_data, participant~., mean, value="acc", na.rm=T)
print(acc)
colnames(acc)[colnames(acc)=="(all)"] <- "mean_acc"
mean(acc$mean_acc)
sd(acc$mean_acc)
acc_plot<-plot(acc, col="green", pch = 19, ylab = "acc")
acc$participant<-as.factor(acc$participant)

##gg plot
acc_plot <- ggplot() +
  geom_point(data=acc, aes(y=mean_acc, x=participant, size = 2), show.legend = FALSE) +
  ylim(c(0.4,1)) + ylab("accuracy") + xlab("participant") +
  theme_grey(base_size = 22) +
  xlab("Participant") +
  ylab("Accuracy (%)")
  #theme_classic()
acc_plot


#check missed responses
missed_resp<-cast(pred_data, participant~., length, value = "resp", subset = (resp == "None"))
print(missed_resp)


#acc by map
acc_map<-cast(pred_data, participant~map, mean, value="acc", na.rm=T)
acc_map$V1<-NULL
acc_map$participant<-as.factor(acc_map$participant)
print(acc_map)
mean(acc_map$A)
sd(acc_map$A)
mean(acc_map$B)
sd(acc_map$B)
t.test(acc_map$A, acc_map$B)

acc_map_plot<-cast(pred_data, participant + map~., mean, value="acc", na.rm=T, subset = (map == "A" | map == "B"))
colnames(acc_map_plot)[colnames(acc_map_plot)=="(all)"] <- "mean_acc"
print(acc_map_plot)
acc_map_plot$participant<-as.factor(acc_map_plot$participant)

#for the bars
map_mean<-cast(pred_data, map~., mean, value="acc", na.rm=T, subset = (map == "A" | map == "B"))
colnames(map_mean)[colnames(map_mean)=="(all)"] <- "mean_acc"
print(map_mean)

#where are there only 4 participants here?
plot_map <- ggplot() +
  geom_bar(data=map_mean, aes(y=mean_acc,x=map),fill  = "white", color = "black", stat = "identity", width = 0.5) + 
  geom_point(data=acc_map_plot, aes(y=mean_acc, x=map, color = factor(participant)), position = position_jitter(w = 0.2, h = 0)) +
  ylim(c(0,1.0)) + 
  theme_grey(base_size = 22)
plot_map

#acc by path
acc_path<-cast(pred_data, participant~path, mean, value="acc", na.rm=T)
print(acc_path)
colMeans(acc_path, na.rm = T)
sd(acc_path$blue)
sd(acc_path$green)
t.test(acc_path$blue, acc_path$green, paired = TRUE)

acc_path_plot<-cast(pred_data, participant + path~., mean, value="acc", na.rm=T,  subset = (path == "blue" | path == "green"))
colnames(acc_path_plot)[colnames(acc_path_plot)=="(all)"] <- "mean_acc"
print(acc_path_plot)
acc_path_plot$participant<-as.factor(acc_path_plot$participant)

#for the bars
path_mean<-cast(pred_data, path~., mean, value="acc", na.rm=T, subset = (map == "A" | map == "B"))
colnames(path_mean)[colnames(path_mean)=="(all)"] <- "mean_acc"
print(path_mean)

plot_path <- ggplot() +
  geom_bar(data=path_mean, aes(y=mean_acc,x=path),fill  = "white", color = "black", stat = "identity", width = 0.5) +
  geom_point(data=acc_path_plot, aes(y=mean_acc, x=path, color = factor(participant)), position = position_jitter(w = 0.2, h = 0)) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22) +
  theme(legend.position = "none") + 
  xlab('Path') + 
  ylab('Accuracy (%)')
plot_path

#acc by map and block
acc_map_block<-cast(pred_data, participant~map+path, mean, value="acc", na.rm=T)
print(acc_map_block)
colMeans(acc_map_block, na.rm = T)

pred_data$map<-as.character(pred_data$map)
pred_data$path<-as.character(pred_data$path)
pred_data$cond<-paste(pred_data$map, pred_data$path, sep="_")
pred_data$cond<-as.factor(pred_data$cond)

acc_cond_plot<-cast(pred_data, participant+cond~., mean, value = "acc", na.rm = T, subset = (cond != "_"))
colnames(acc_cond_plot)[colnames(acc_cond_plot)=="(all)"] <- "mean_acc"
print(acc_cond_plot, na.rm=T)

#for the bars
cond_mean<-cast(pred_data, cond~., mean, value="acc", na.rm=T, subset = (cond != "_"))
colnames(cond_mean)[colnames(cond_mean)=="(all)"] <- "mean_acc"
print(cond_mean)

plot_cond <- ggplot() +
  geom_bar(data=cond_mean, aes(y=mean_acc,x=cond),fill  = "white", color = "black", stat = "identity", width = 0.5) +
  geom_point(data=acc_cond_plot, aes(y=mean_acc, x=cond, color = factor(participant)), size = 2, alpha = 0.8, position = position_jitter(w = 0.2, h = 0)) +
  ylim(c(0,1)) +
  theme_grey(base_size = 22)
plot_cond


#add a time bin
pred_data$time_bin<-ifelse(pred_data$dur < 6.0, "5", ifelse(pred_data$dur < 7.0, "6", ifelse(pred_data$dur < 8.0, "7", "8")))
unique(pred_data$time_bin)

acc_pred_time<-cast(pred_data, participant~time_bin, mean, value="acc", na.rm=T)
print(acc_pred_time)
colMeans(acc_pred_time, na.rm = T)
sd(acc_pred_time$`8`)


#do ALL of this for rt

#overall acc
rt<-cast(pred_data, participant~., mean, value="rt", na.rm=T, subset = (acc == 1))
print(rt)
plot(rt, col = "blue", ylab="rt", pch = 19)

#acc by map
rt_map<-cast(pred_data, participant~map, mean, value="rt", na.rm=T)
print(rt_map)

#acc by map and block
rt_map_block<-cast(pred_data, participant~map+path, mean, value="rt", na.rm=T)
print(rt_map_block)
colMeans(rt_map_block, na.rm  = T)

#rt pred time
rt_pred_time<-cast(pred_data, participant~time_bin, mean, value="rt", na.rm=T)
print(rt_pred_time)
colMeans(rt_pred_time, na.rm=T)

#######get the distance between the predicted room and the correct answer
order_only<-subset(pred_data, !is.na(subject))
order_only$scene<-as.character(order_only$scene)
order_only<-subset(order_only, (scene != "18.png") & (scene != "19.png"))

pred_data$path<-as.character(pred_data$path)
pred_data$map<-as.character(pred_data$map)
pred_data$scene<-as.character(pred_data$scene)
pred_data$scene_a<-as.character(pred_data$scene_a)
pred_data$scene_b<-as.character(pred_data$scene_b)
pred_data$cor_distance<-NA

for (row in 1:nrow(pred_data)){
  if (pred_data$scene_a[row] != ""){
    pth<-pred_data$path[row]
    mp<-pred_data$map[row]
    p<-pred_data$participant[row]
    temp<-subset(order_only, subject == p & path == pth & map == mp)
    if (pred_data$cor_resp[row] == "j"){
      left<-pred_data$scene_a[row]
      pred<-pred_data$scene[row]
      ind_1<-which(temp$scene == pred)
      ind_2<-which(temp$scene == left)
      pred_data$cor_distance[row]<-ifelse(ind_2>ind_1, abs(ind_2-ind_1), abs(ind_2+8-ind_1))
    }
    else if (pred_data$cor_resp[row] == "k"){
      right<-pred_data$scene_b[row]
      pred<-pred_data$scene[row]
      ind_1<-which(temp$scene == pred)
      ind_2<-which(temp$scene == right)
      pred_data$cor_distance[row]<-ifelse(ind_2>ind_1, abs(ind_2-ind_1), abs(ind_2+8-ind_1))
    }
  }
}


acc_future<-cast(pred_data, participant~cor_distance, mean, value="acc", na.rm=T, subset = (participant != 2))
print(acc_future)
colMeans(acc_future, na.rm = T)
sd(acc_future$`4`)


#plot it
acc_distance_plot<-cast(pred_data, participant + cor_distance~., mean, value="acc", na.rm=T, subsest = (!is.na(cor_distance)))
colnames(acc_distance_plot)[colnames(acc_distance_plot)=="(all)"] <- "mean_acc"
acc_distance_plot<-subset(acc_distance_plot, !is.na(cor_distance))
print(acc_distance_plot)
acc_distance_plot$participant<-as.factor(acc_distance_plot$participant)
acc_distance_plot$cor_distance<-as.factor(acc_distance_plot$cor_distance)

#for the bars
distance_mean<-cast(pred_data, cor_distance~., mean, value="acc", na.rm=T, subset = (map == "A" | map == "B"))
distance_mean<-subset(distance_mean, !is.na(cor_distance))
colnames(distance_mean)[colnames(distance_mean)=="(all)"] <- "mean_acc"
print(distance_mean)
distance_mean$cor_distance<-as.factor(distance_mean$cor_distance)

plot_distance <- ggplot() +
  geom_bar(data=distance_mean, aes(y=mean_acc,x=cor_distance),fill  = "white", color = "black", stat = "identity", width = 0.5) +
  geom_point(data=acc_distance_plot, aes(y=mean_acc, x=cor_distance, color = factor(participant)), position = position_jitter(w=0.2, h=0)) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22) +
  theme(legend.position = "none") + 
  xlab('Number of Steps into Future') + 
  ylab('Accuracy (%)')
plot_distance


acc_future<-cast(pred_data, participant~cor_distance, mean, value="acc", na.rm=T, subset = (participant != 2))
print(acc_future)
#acc_future<-subset(acc_future, participant != "14")
colMeans(acc_future, na.rm = T)
sd(acc_future$`4`)

#plot it
#do this for inverse efficiency
acc_distance_plot<-cast(pred_data, participant + cor_distance~., mean, value="acc", na.rm=T, subsest = (!is.na(cor_distance)))
colnames(acc_distance_plot)[colnames(acc_distance_plot)=="(all)"] <- "mean_acc"
acc_distance_plot<-subset(acc_distance_plot, !is.na(cor_distance))
print(acc_distance_plot)
acc_distance_plot$participant<-as.factor(acc_distance_plot$participant)
acc_distance_plot$cor_distance<-as.factor(acc_distance_plot$cor_distance)

acc_distance_plot_rt<-cast(pred_data, participant + cor_distance~., mean, value="rt", na.rm=T, subsest = (!is.na(cor_distance)))
colnames(acc_distance_plot_rt)[colnames(acc_distance_plot_rt)=="(all)"] <- "mean_rt"
acc_distance_plot_rt<-subset(acc_distance_plot_rt, !is.na(cor_distance))

acc_distance_plot$inverse_efficiency<-acc_distance_plot_rt$mean_rt/acc_distance_plot$mean_acc

#for the bars
acc_distance_plot_rt<-as.data.frame(acc_distance_plot_rt)
distance_mean<-cast(acc_distance_plot, cor_distance~., mean, value="inverse_efficiency", na.rm=T)
distance_mean<-subset(distance_mean, !is.na(cor_distance))
colnames(distance_mean)[colnames(distance_mean)=="(all)"] <- "inverse_efficiency"
print(distance_mean)
distance_mean$cor_distance<-as.factor(distance_mean$cor_distance)
distance_mean$inverse_efficiency<-as.numeric(distance_mean$inverse_efficiency)
distance_mean<-as.data.frame(distance_mean)

plot_distance_inv<-ggplot()+
  geom_bar(data = distance_mean, aes(y=inverse_efficiency, x = cor_distance), fill = "white", color = "black", stat = "identity", width = 0.5)+
  geom_point(data = acc_distance_plot, aes(y=inverse_efficiency, x=cor_distance, color= factor(participant)),
             position = position_jitter(w=0.2, h= 0)) +
  theme_grey(base_size = 22)
plot_distance_inv


acc_future_time<-cast(pred_data, time_bin~cor_distance, mean, value="acc", na.rm=T)
print(acc_future_time)
colMeans(acc_future_time, na.rm=T)

acc_time_four<-cast(pred_data, participant~time_bin, mean, value="acc", na.rm=T, subset = (cor_distance == 4))
acc_time_four
sd(acc_time_four$`5`, na.rm=T)
sd(acc_time_four$`8`, na.rm=T)


#plot time X distance
#CHANGE ACK TO ACC!!!!!!!
distance_time<-cast(pred_data, time_bin + cor_distance ~ . , mean, value="acc", na.rm=T)
print(distance_time)
distance_time<-subset(distance_time, !is.na(cor_distance))
colnames(distance_time)[colnames(distance_time)=="(all)"] <- "mean_acc"
print(distance_time)
distance_time$time_bin<-as.numeric(distance_time$time_bin)
distance_time$cor_distance<-as.factor(distance_time$cor_distance)


plot_distance_time <- ggplot(data=distance_time, aes(y=mean_acc,x=time_bin, colour = cor_distance)) +
  geom_line(size = 1.5, se= FALSE)+
  ylim(c(0.5,1)) +
  theme_grey(base_size = 22) +
  labs(colour = "Steps into Future") +
  theme(legend.text = element_text(size = 10), legend.title=element_text(size=10)) +
  xlab('Amount of Prediction Time (s)') + 
  ylab('Accuracy (%)')
plot_distance_time


#plot time X distance reaction time
distance_time_rt<-cast(pred_data, time_bin + cor_distance ~ . , mean, value="rt", na.rm=T)
print(distance_time_rt)
distance_time_rt<-subset(distance_time_rt, !is.na(cor_distance))
colnames(distance_time_rt)[colnames(distance_time_rt)=="(all)"] <- "mean_rt"
print(distance_time_rt)
distance_time_rt$time_bin<-as.numeric(distance_time_rt$time_bin)
distance_time_rt$cor_distance<-as.factor(distance_time_rt$cor_distance)

#for the bars
time_mean<-cast(pred_data, time_bin~., mean, value="acc", na.rm=T, subset = (map == "A" | map == "B"))
time_mean<-subset(time_mean, !is.na(time_bin))
colnames(time_mean)[colnames(time_mean)=="(all)"] <- "mean_acc"
print(time_mean)
time_mean$time_bin<-as.factor(time_mean$time_bin)

plot_distance_time_rt <- ggplot(data=distance_time_rt, aes(y=mean_rt,x=time_bin, colour = cor_distance)) +
  geom_line(size = 1.5, se= FALSE)+
  ylim(c(1,2.5)) +
  theme_grey(base_size = 22)
plot_distance_time_rt


#it's using the average from participant 1...
#plot it
distance_time_plot<-cast(pred_data, participant + cor_distance~., mean, value="acc", na.rm=T, subsest = (!is.na(cor_distance)))
colnames(acc_distance_plot)[colnames(acc_distance_plot)=="(all)"] <- "mean_acc"
acc_distance_plot<-subset(acc_distance_plot, !is.na(cor_distance))
print(acc_distance_plot)
acc_distance_plot$participant<-as.factor(acc_distance_plot$participant)
acc_distance_plot$cor_distance<-as.factor(acc_distance_plot$cor_distance)

rt_future<-cast(pred_data, participant~cor_distance, mean, value="rt", na.rm=T)
print(rt_future)
colMeans(rt_future, na.rm=T)

rt_future_time<-cast(pred_data, time_bin~cor_distance, mean, value="rt", na.rm=T)
print(rt_future_time)

#make a column for inverse efficiency
pred_data$inverse_efficiency<-pred_data$acc/pred_data$rt
head(pred_data$inverse_efficiency)

inv_eff_cor_dist<-cast(pred_data, participant~cor_distance, mean, value="inverse_efficiency", na.rm = T)
inv_eff_cor_dist
colMeans(inv_eff_cor_dist)

inv_eff_pred_time<-cast(pred_data, participant~time_bin, mean, value="inverse_efficiency", na.rm = T)
inv_eff_pred_time
colMeans(inv_eff_pred_time)

########################################
############ TEMP INTEGRATION ##########
########################################

#get a list of subject folders
setwd(paste0(baseDir,'/data/subs'))
subs<-list.files()
print(subs)
#loop through and read in data
for (s in subs){
  print(s)
  setwd(paste0(baseDir,'/data/subs/',s))
  Int_Files<-list.files(pattern=c("Integration"))
  if(s==subs[1]){
    int_data<-read.csv(Int_Files)
  }
  else{
    temp<-read.csv(Int_Files)
    int_data<-rbind(int_data, temp)
  }
}

colnames(int_data)[23:25]<- c("resp","acc","rt")

#overall acc
acc_int<-cast(int_data, participant~., mean, value="acc", na.rm=T)
colnames(acc_int)[colnames(acc_int)=="(all)"] <- "mean_acc"
print(acc_int)
mean(acc_int$mean_acc)
sd(acc_int$mean_acc)
t.test(acc_int$mean_acc, mu = 0.5)

acc_int$participant<-as.factor(acc_int$participant)
acc_int_plot <- ggplot() +
  geom_point(data=acc_int, aes(y=mean_acc, x=participant, size = 2), show.legend = FALSE) +
  ylim(c(0.4,1)) + ylab("Accuracy (%)") + xlab("Participant") +
  theme_grey(base_size = 22)
acc_int_plot

#acc by map and block
acc_path_int<-cast(int_data, participant~path, mean, value="acc", na.rm=T)
print(acc_path_int)
colMeans(acc_path_int)

#plot
int_path_plot<-cast(int_data, participant + path~., mean, value="acc", na.rm=T, subset = (path == "green" | path == "blue"))
colnames(int_path_plot)[colnames(int_path_plot)=="(all)"] <- "mean_acc"
print(int_path_plot)
int_path_plot$participant<-as.factor(int_path_plot$participant)

#for the bars
int_path<-cast(int_data, path~., mean, value="acc", na.rm=T, subset = (path == "green" | path == "blue"))
colnames(int_path)[colnames(int_path)=="(all)"] <- "mean_acc"
print(int_path)

#where are there only 4 participants here?
plot_path_int <- ggplot() +
  geom_bar(data=int_path, aes(y=mean_acc,x=path),fill  = "white", color = "black", stat = "identity", width = 0.5) +
  geom_point(data=int_path_plot, aes(y=mean_acc, x=path, color = factor(participant))) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22)
plot_path_int

#acc by run
acc_run_int<-cast(int_data, participant ~ Int_All_Runs.thisN, mean, value  = "acc", na.rm=T)
print(acc_run_int)
colMeans(acc_run_int, na.rm=T)

#inverse efficiency by run
int_data$inverse_efficiency<-int_data$acc/int_data$rt
acc_run_int_eff<-cast(int_data, participant ~ Int_All_Runs.thisN, mean, value  = "inverse_efficiency", na.rm=T)
print(acc_run_int_eff)
colMeans(acc_run_int_eff, na.rm=T)



#plot
int_run_plot<-cast(int_data, participant + Int_All_Runs.thisN~., mean, value="acc", na.rm=T, subset = (!is.na(Int_All_Runs.thisN)))
colnames(int_run_plot)[colnames(int_run_plot)=="(all)"] <- "mean_acc"
print(int_run_plot)
int_run_plot$participant<-as.factor(int_run_plot$participant)


#for the bars
int_run<-cast(int_data, Int_All_Runs.thisN~., mean, value="acc", na.rm=T, subset = (!is.na(Int_All_Runs.thisN)))
colnames(int_run)[colnames(int_run)=="(all)"] <- "mean_acc"
print(int_run)

#where are there only 4 participants here? why are there only 2 bars?
plot_run_int <- ggplot() +
  geom_line(data=int_run_plot, aes(y=mean_acc,x=Int_All_Runs.thisN, col = participant), size = 1.3) +
  ylim(c(0.3,1.0)) +
  theme_grey(base_size = 22) +
  theme(legend.position = "none")+
  xlab('Run') +
  ylab('Accuracy (%)') 
plot_run_int

#acc by run and path
acc_run_path<-cast(int_data, participant ~ Int_All_Runs.thisN + path, mean, value  = "acc", na.rm=T)
print(acc_run_path)

#acc by int and no int
acc_int_noint<-cast(int_data, participant ~ cond, mean, value = "acc", na.rm=T)
print(acc_int_noint)
colMeans(acc_int_noint)
sd(acc_int_noint$Int)

#inv eff by int and no int
acc_int_noint_eff<-cast(int_data, participant ~ cond, mean, value = "inverse_efficiency", na.rm=T)
print(acc_int_noint_eff)
colMeans(acc_int_noint_eff)
sd(acc_int_noint_eff$Int)

#plot
int_noInt_plot<-cast(int_data, participant + cond ~., mean, value="acc", na.rm=T, subset= (cond != ""))
colnames(int_noInt_plot)[colnames(int_noInt_plot)=="(all)"] <- "mean_acc"
print(int_noInt_plot)
int_noInt_plot$participant<-as.factor(int_noInt_plot$participant)

#for the bars
int_noInt<-cast(int_data, cond~., mean, value="acc", na.rm=T, subset = (cond != ""))
colnames(int_noInt)[colnames(int_noInt)=="(all)"] <- "mean_acc"
print(int_noInt)

#where are there only 4 participants here? why are there only 2 bars?
plot_int_noInt <- ggplot() +
  geom_bar(data=int_noInt, aes(y=mean_acc,x=cond),fill  = "white", color = "black", stat = "identity", width = 0.5) +
  geom_point(data=int_noInt_plot, aes(y=mean_acc, x=cond, color = factor(participant)), position = position_jitter(w = 0.2, h = 0)) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22) +
  theme(legend.position = 'none')+
  xlab('Integration Condition')+
  ylab('Accuracy (%)')
plot_int_noInt

#acc by int and no int
acc_int_noint_run<-cast(int_data, participant ~ cond + Int_All_Runs.thisN, mean, value = "acc", na.rm=T, subset = (cond != "V1"))
print(acc_int_noint_run)
colMeans(acc_int_noint_run, na.rm=T)

#plot
int_noInt_run_plot<-cast(int_data, Int_All_Runs.thisN + cond ~ ., mean, value="acc", na.rm=T, subset= (cond != ""))
colnames(int_noInt_run_plot)[colnames(int_noInt_run_plot)=="(all)"] <- "mean_acc"
print(int_noInt_run_plot)
int_noInt_run_plot$Int_All_Runs.thisN<-as.numeric(int_noInt_run_plot$Int_All_Runs.thisN)
#int_noInt_run_plot$participant<-as.factor(int_noInt_run_plot$participant)

#where are there only 4 participants here? why are there only 2 bars?
plot_int_noInt_run <- ggplot(data=int_noInt_run_plot, aes(x=Int_All_Runs.thisN, y=mean_acc, colour = cond)) +
  geom_line(size = 1.5, se=FALSE) +
  ylim(c(0.3,1.0)) +
  theme_grey(base_size = 22)+
  labs(colour = "Integration Condition") +
  xlab("Run")+
  ylab("Accuracy (%)")
plot_int_noInt_run




######do the same for reaction time
#overall acc
acc_int_rt<-cast(int_data, participant~., mean, value="rt", na.rm=T)
print(acc_int_rt)
plot(acc_int_rt, ylab = "rt", pch = 19, col = "blue")
plot(rt, col = "blue", ylab="rt", pch = 19)

acc_cond_rt<-cast(int_data, participant~cond, mean, value="rt", na.rm=T)
print(acc_cond_rt)
colMeans(acc_cond_rt)


#acc by map and block
acc_path_int_rt<-cast(int_data, participant~path, mean, value="rt", na.rm=T)
print(acc_path_int_rt)

#acc by run
acc_run_int_rt<-cast(int_data, participant ~ Int_All_Runs.thisN, mean, value  = "rt", na.rm=T)
print(acc_run_int_rt)

#acc by run and path
acc_run_path_rt<-cast(int_data, participant ~ Int_All_Runs.thisN + path, mean, value  = "rt", na.rm=T)
print(acc_run_path_rt)


#rt by int and no int
rt_int_noint_run<-cast(int_data, participant ~ cond + Int_All_Runs.thisN, mean, value = "rt", na.rm=T, subset = (cond != "V1"))
print(rt_int_noint_run)
colMeans(rt_int_noint_run, na.rm=T)

#plot
int_noInt_run_plot_rt<-cast(int_data, Int_All_Runs.thisN + cond ~ ., mean, value="rt", na.rm=T, subset= (cond != ""))
colnames(int_noInt_run_plot_rt)[colnames(int_noInt_run_plot_rt)=="(all)"] <- "mean_rt"
print(int_noInt_run_plot_rt)
int_noInt_run_plot_rt$Int_All_Runs.thisN<-as.numeric(int_noInt_run_plot_rt$Int_All_Runs.thisN)
#int_noInt_run_plot$participant<-as.factor(int_noInt_run_plot$participant)

#where are there only 4 participants here? why are there only 2 bars?
plot_int_noInt_rt <- ggplot(data=int_noInt_run_plot_rt, aes(x=Int_All_Runs.thisN, y=mean_rt, colour = cond)) +
  geom_line(size = 1.5, se=FALSE) +
  ylim(c(1.5,2.0)) +
  theme_grey(base_size = 22)
plot_int_noInt_rt

####get the order of integration

#read in int_info per sub
#create an empty data frame for the full path
int_paths<-data.frame()
#create an empty data frame for just the int bridges, this will eventually get merged with int_full
int_info_data<-data.frame(Q = NA, A = NA, A_next = NA, path = NA, participant = NA)
#loop through subjects to fill in int_paths and int_info_data
for (sub in unique(order_only$subject)){
  temp<-subset(order_only, subject == sub)
  #this gets the room numbers and paths of the "int bridges" for each participant
  int_info<-read.csv(paste0(baseDir, "/data/", sub, "_IntInfo.csv"), header = TRUE)
  int_info$participant<-sub
  int_info_data<-rbind(int_info_data, int_info)
  #gets the path order for each of teh 4 original sequence for each participant
  g_a<-(temp$scene[1:8])
  bl_a<-(temp$scene[9:16])
  g_b<-(temp$scene[17:24])
  bl_b<-(temp$scene[25:32])
  #shifts the paths and binds them together for the green and blue path, based on which one gets integrated in each participant
  if (int_info$path[1] == "green"){
    int<-c(shift(g_a, places = abs(match(int_info$Q[1], g_a)-8), dir = "right"), shift(g_b, places = abs(match(int_info$A[1], g_b)-1), dir = "left"))
    int<-cbind(matrix(int), sub)
    int<-cbind(int, "green")
  }
  if (int_info$path[1] == "blue") {
    int<-c(shift(bl_a, places = abs(match(int_info$Q[1], bl_a)-8), dir = "right"), shift(bl_b, places = abs(match(int_info$A[1], bl_b)-1), dir = "left"))
    int<-cbind(matrix(int), sub)
    int<-cbind(int, "blue")
  }
  int_paths<-rbind(int_paths, int)
}

int_info_data<-int_info_data %>% filter(!is.na(participant))



order_only<-subset(pred_data, !is.na(subject))
order_only$scene<-as.character(order_only$scene)

#GET COR DISTANCE FOR EACH SUB FOR INT
#this is including no int cond...but will have to add a diff loop for that
int_paths$V1<-as.character(int_paths$V1)
int_data$cor_distance<-NA
for (row in 1:nrow(int_data)){
  if (int_data$scene_a[row] != ""){
    cond<-int_data$cond[row]
    p<-int_data$participant[row]
    temp<-subset(int_paths, sub == p)
    temp_path<-as.character(temp$V1)
    if (int_data$cor_resp[row] == "j"){
      left<-int_data$scene_a[row]
      pred<-int_data$scene[row]
      ind_1<-as.numeric(which(temp_path == pred))
      ind_2<-as.numeric(which(temp_path == left))
      int_data$cor_distance[row]<-ifelse(ind_2>ind_1, ind_2-ind_1, (ind_2+16)-ind_1)
    }
    else if (int_data$cor_resp[row] == "k"){
      right<-int_data$scene_b[row]
      pred<-int_data$scene[row]
      ind_1<-as.numeric(which(temp_path == pred))
      ind_2<-as.numeric(which(temp_path == right))
      int_data$cor_distance[row]<-ifelse(ind_2>ind_1, ind_2-ind_1, (ind_2+16)-ind_1)
    }
  }
}

int_test<-subset(int_data, cond == "Int")
int_test$cor_distance
int_distance<-cast(int_test, participant~cor_distance, mean, value="acc", na.rm=T)
colMeans(int_distance)

int_distance_run<-cast(int_test, participant~cor_distance + Int_All_Runs.thisN, mean, value="acc", na.rm=T)
colMeans(int_distance_run, na.rm=T)

#plot run X distance -- using int_Test for int only until I get the for loop right for cor dist in no int cond
distance_run_int<-cast(int_test, Int_All_Runs.thisN + cor_distance ~ . , mean, value="acc", na.rm=T)
print(distance_run_int)
colnames(distance_run_int)[colnames(distance_run_int)=="(all)"] <- "mean_acc"
print(distance_run_int)
distance_run_int$Int_All_Runs.thisN<-as.numeric(distance_run_int$Int_All_Runs.thisN)
distance_run_int$cor_distance<-as.factor(distance_run_int$cor_distance)

plot_distance_run_int <- ggplot(data=distance_run_int, aes(y=mean_acc,x=Int_All_Runs.thisN, colour = cor_distance)) +
  geom_line(size = 1.5, se= FALSE)+
  ylim(c(0.5,1.0)) +
  theme_grey(base_size = 22) +
  xlab("Run")+
  ylab ("Accuracy (%)")+
  labs(colour = "Steps into Future")
plot_distance_run_int

#plot it--using int_test again
acc_distance_int<-cast(int_test, participant + cor_distance~., mean, value="acc", na.rm=T, subsest = (!is.na(cor_distance)))
colnames(acc_distance_int)[colnames(acc_distance_int)=="(all)"] <- "mean_acc"
acc_distance_int<-subset(acc_distance_int, !is.na(cor_distance))
print(acc_distance_int)
acc_distance_int$participant<-as.factor(acc_distance_int$participant)
acc_distance_int$cor_distance<-as.factor(acc_distance_int$cor_distance)

#for the bars
distance_mean<-cast(int_test, cor_distance~., mean, value="acc", na.rm=T)
distance_mean<-subset(distance_mean, !is.na(cor_distance))
colnames(distance_mean)[colnames(distance_mean)=="(all)"] <- "mean_acc"
print(distance_mean)
distance_mean$cor_distance<-as.factor(distance_mean$cor_distance)

plot_distance_int <- ggplot() +
  geom_bar(data=distance_mean, aes(y=mean_acc,x=cor_distance),fill  = "white", color = "black", stat = "identity", width = 0.5) +
  geom_point(data=acc_distance_int, aes(y=mean_acc, x=cor_distance, color = factor(participant)), position = position_jitter(w=0.2, h=0)) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22) +
  theme(legend.position = "none")+
  ylab("Accuracy (%)") +
  xlab("Number of Steps into Future")
plot_distance_int


#where at least ONE answer is from the different map
int_full<-data.frame()
#get accuracy for integration around the bridges
for (sub in unique(int_data$participant)){
  temp<-subset(order_only, subject == sub)
  g_a<-(temp$scene[1:8])
  bl_a<-(temp$scene[9:16])
  g_b<-(temp$scene[17:24])
  bl_b<-(temp$scene[25:32])
  int_bridge<-subset(int_data, participant == sub)
  int_bridge$int_required<-ifelse(int_bridge$scene %in% g_a & int_bridge$scene_a %in% g_a & int_bridge$scene_b %in% g_a, "n",
                                  ifelse(int_bridge$scene %in% g_b & int_bridge$scene_a %in% g_b & int_bridge$scene_b %in% g_b, "n",
                                         ifelse(int_bridge$scene %in% bl_a & int_bridge$scene_a %in% bl_a & int_bridge$scene_b %in% bl_a, "n",
                                                ifelse(int_bridge$scene %in% bl_b & int_bridge$scene_a %in% bl_b & int_bridge$scene_b %in% bl_b, "n", "y"))))
  int_full<-rbind(int_full, int_bridge)
}


int_req<-cast(int_full, participant~int_required, mean, value = "acc", na.rm=T)
print(int_req)
colMeans(int_req)


int_req<-cast(int_full, participant~ int_required + cor_distance, mean, value = "acc", na.rm=T, subset = (cond=="Int"))
print(int_req)
colMeans(int_req, na.rm=T)
t.test(int_req$y_4, mu = 0.5)

int_req_run<-cast(int_full, participant~int_required + Int_All_Runs.thisN, mean, value = "acc", na.rm=T, subset = (cond == "Int"))
print(int_req_run)
colMeans(int_req_run, na.rm=T)
t.test(int_req_run$y_0, mu=.5)
t.test(int_req_run$y_1, mu=.5)


#plot
int_req_plot<-cast(int_full, participant + int_required~., mean, value="acc", na.rm=T)
colnames(int_req_plot)[colnames(int_req_plot)=="(all)"] <- "mean_acc"
print(int_req_plot)
int_req_plot$participant<-as.factor(int_req_plot$participant)

#for the bars
int_req<-cast(int_full, int_required~., mean, value="acc", na.rm=T)
colnames(int_req)[colnames(int_req)=="(all)"] <- "mean_acc"
print(int_req)

#where are there only 4 participants here?
plot_int <- ggplot() +
  geom_bar(data=int_req, aes(y=mean_acc,x=int_required),fill  = "white", color = "black", stat = "identity", width = 0.5) +
  #geom_errorbar(data=my_dat, aes(y=my_mean,x=cyl,ymin=my_mean-my_se,ymax=my_mean+my_se), width = 0.75) +
  geom_point(data=int_req_plot, aes(y=mean_acc, x=int_required, color = factor(participant)), position = position_jitter(w = 0.2, h = 0)) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22)
plot_int



#where BOTH answers are in the different map
int_full<-data.frame()
#get accuracy for integration around the bridges
for (sub in unique(int_data$participant)){
  temp<-subset(order_only, subject == sub)
  g_a<-(temp$scene[1:8])
  bl_a<-(temp$scene[9:16])
  g_b<-(temp$scene[17:24])
  bl_b<-(temp$scene[25:32])
  int_bridge<-subset(int_data, participant == sub)
  int_bridge$int_required<-ifelse(int_bridge$scene %in% g_a & int_bridge$scene_a %in% g_b & int_bridge$scene_b %in% g_b, "y",
                              ifelse(int_bridge$scene %in% g_b & int_bridge$scene_a %in% g_a & int_bridge$scene_b %in% g_a, "y",
                                      ifelse(int_bridge$scene %in% bl_a & int_bridge$scene_a %in% bl_b & int_bridge$scene_b %in% bl_b, "y",
                                            ifelse(int_bridge$scene %in% bl_b & int_bridge$scene_a %in% bl_a & int_bridge$scene_b %in% bl_a, "y", "n"))))
  int_full<-rbind(int_full, int_bridge)
  }


#break down the "n" category into no integration or one answer integrated
int_full_temp_y<-subset(int_full, int_required == "y")
int_full_temp_n<-subset(int_full, int_required == "n")

int_full_temp_n$int_required<-ifelse(int_full_temp_n$scene %in% g_a & int_full_temp_n$scene_a %in% g_a & int_full_temp_n$scene_b %in% g_a, "n",
                                                   ifelse(int_full_temp_n$scene %in% g_b & int_full_temp_n$scene_a %in% g_b & int_full_temp_n$scene_b %in% g_b, "n",
                                                          ifelse(int_full_temp_n$scene %in% bl_a & int_full_temp_n$scene_a %in% bl_a & int_full_temp_n$scene_b %in% bl_a, "n",
                                                                 ifelse(int_full_temp_n$scene %in% bl_b & int_full_temp_n$scene_a %in% bl_b & int_full_temp_n$scene_b %in% bl_b, "n", "s"))))

int_full<-rbind(int_full_temp_n, int_full_temp_y)

int_req<-cast(int_full, participant~int_required, mean, value = "acc", na.rm=T)
print(int_req)
colMeans(int_req)

int_req_run<-cast(int_full, participant~int_required + Int_All_Runs.thisN, mean, value = "acc", na.rm=T)
print(int_req_run)
colMeans(int_req_run, na.rm=T)
t.test(int_req_run$y_0, mu=.5)
t.test(int_req_run$y_0, int_req_run$y_3)
t.test(int_req_run$y_1, mu=.5)


#plot
int_req_plot<-cast(int_full, participant + int_required~., mean, value="acc", na.rm=T)
colnames(int_req_plot)[colnames(int_req_plot)=="(all)"] <- "mean_acc"
print(int_req_plot)
int_req_plot$participant<-as.factor(int_req_plot$participant)

#for the bars
int_req<-cast(int_full, int_required~., mean, value="acc", na.rm=T)
colnames(int_req)[colnames(int_req)=="(all)"] <- "mean_acc"
print(int_req)

#where are there only 4 participants here?
plot_int <- ggplot() +
  geom_bar(data=int_req, aes(y=mean_acc,x=int_required),fill  = "white", color = "black", stat = "identity", width = 0.5) +
  #geom_errorbar(data=my_dat, aes(y=my_mean,x=cyl,ymin=my_mean-my_se,ymax=my_mean+my_se), width = 0.75) +
  geom_point(data=int_req_plot, aes(y=mean_acc, x=int_required, color = factor(participant)), position = position_jitter(w = 0.2, h = 0)) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22)
plot_int

#int_required for cor_distance
int_req_distance<-cast(int_full, participant~int_required + cor_distance, mean, value = "acc", na.rm=T, subset = (cond == "Int"))
print(int_req_distance)
colMeans(int_req_distance, na.rm=T)


#int_required for cor_distance
int_req_distance_first<-cast(int_full, participant~int_required + cor_distance, mean, value = "acc", na.rm=T, subset = (cond == "Int" & Int_All_Runs.thisN == 0))
print(int_req_distance_first)
colMeans(int_req_distance_first, na.rm=T)

int_req_distance_first<-cast(int_full, participant~int_required + cor_distance, mean, value = "acc", na.rm=T, subset = (cond == "Int" & Int_All_Runs.thisN == 3))
print(int_req_distance_first)
colMeans(int_req_distance_first, na.rm=T)

#plot distance X required -- using int_Test for int only until I get the for loop right for cor dist in no int cond
distance_req_int<-cast(int_full, int_required + cor_distance ~ . , mean, value="acc", na.rm=T, subset = (cond == "Int"))
print(distance_req_int)
colnames(distance_req_int)[colnames(distance_req_int)=="(all)"] <- "mean_acc"
print(distance_req_int)
distance_req_int$int_required<-as.factor(distance_req_int$int_required)
distance_req_int$cor_distance<-as.numeric(distance_req_int$cor_distance)

#is this wrong or is the other cast function wrong???
plot_distance_req <- ggplot(data=distance_req_int, aes(y=mean_acc,x=cor_distance, colour = int_required)) +
  geom_line(size = 1.5)+
  ylim(c(0.5,1.0)) +
  theme_grey(base_size = 22)
plot_distance_req

################################################
###get the distance to the integration bridge###
################################################
#first get int_info_data into a readable format
#clean int_info_data, put it so that each participant is in one row, remove unnecessary columns, and rename ones that are left
int_AtoB<-int_info_data %>% slice(which(row_number() %% 2 == 1))
int_BtoA<-int_info_data %>% slice(which(row_number() %% 2 == 0))
int_info_data<-left_join(int_AtoB, int_BtoA, by = "participant")
int_info_data<-subset(int_info_data, select = -c(3,8,9))
colnames(int_info_data)[1:6]<- c("bridge_AtoB_1", "bridge_AtoB_2", "path", "participant", "bridge_BtoA_1", "bridge_BtoA_2")

#then merge the full data with the int bridge, so for each trial there is a column with the int bridge
int_full<-left_join(int_full, int_info_data, by = "participant")

#get distance to the integration bridge for the predicted scene and the correct answer
#index 8 and 16 of int_paths for each subject are the "int_bridges"--this loop gets the int bridges
#then take the cue scene and get its index from int paths, see if it's less than or greater than 8, then get the difference between its index and 8/16
int_full$bridge_distance<-NA
int_full$bridge_cor_dist<-NA

for (row in 1:nrow(int_full)){
  if (int_full$scene[row] != ""){
    temp<-int_paths %>% filter(sub == int_full$participant[row])
    int_bridge_1<-int_full$bridge_AtoB_1[row]
    int_bridge_2<-int_full$bridge_BtoA_1[row]
    pred_scene<-as.character(int_full$scene[row])
    bridge_1<-as.numeric(which(temp$V1 == int_bridge_1))
    bridge_2<-as.numeric(which(temp$V1 == int_bridge_2))
    bridge_dist<-as.numeric(which(temp$V1 == pred_scene))
    int_full$bridge_distance[row]<-ifelse(bridge_dist <= bridge_1, bridge_1 - bridge_dist, bridge_2 - bridge_dist)
    if (int_full$cor_resp[row] == "j"){
      left<-int_full$scene_a[row]
      bridge_dist<-as.numeric(which(temp$V1 == left))
      int_full$bridge_cor_dist[row]<-ifelse(bridge_dist <= bridge_1, bridge_1 - bridge_dist, bridge_2 - bridge_dist)
    }
    else if (int_full$cor_resp[row] == "k"){
      right<-int_full$scene_b[row]
      bridge_dist<-as.numeric(which(temp$V1 == right))
      int_full$bridge_cor_dist[row]<-ifelse(bridge_dist <= bridge_1, bridge_1 - bridge_dist, bridge_2 - bridge_dist)
    }
  }
}

#plot
bridge_dist_plot<-cast(int_full, participant + bridge_cor_dist~., mean, value="acc", na.rm=T, subset = (!is.na(bridge_cor_dist)) & cond == "Int")
colnames(bridge_dist_plot)[colnames(bridge_dist_plot)=="(all)"] <- "mean_acc"
print(bridge_dist_plot)
bridge_dist_plot$participant<-as.factor(bridge_dist_plot$participant)
bridge_dist_plot$bridge_cor_dist<-as.numeric(bridge_dist_plot$bridge_cor_dist)


#for the bars
int_run<-cast(int_data, Int_All_Runs.thisN~., mean, value="acc", na.rm=T, subset = (!is.na(Int_All_Runs.thisN)) & cond == "Int")
colnames(int_run)[colnames(int_run)=="(all)"] <- "mean_acc"
print(int_run)

#where are there only 4 participants here? why are there only 2 bars?
plot_bridge_dist <- ggplot() +
  geom_line(data=bridge_dist_plot, aes(y=mean_acc,x=bridge_cor_dist, col = participant), size = 1.3) +
  ylim(c(0.3,1.0)) +
  theme_grey(base_size = 22)
plot_bridge_dist


###make these for each run, then put in a plot matrix
#plot run 1
bridge_dist_plot_run_1<-cast(int_full, bridge_cor_dist~., mean, value="acc", na.rm=T, subset = (Int_All_Runs.thisN == 0 & cond == "Int"))
colnames(bridge_dist_plot_run_1)[colnames(bridge_dist_plot_run_1)=="(all)"] <- "mean_acc"
bridge_dist_plot_run_1<-subset(bridge_dist_plot_run_1, !is.na(bridge_cor_dist))
print(bridge_dist_plot_run_1)
bridge_dist_plot_run_1$bridge_cor_dist<-as.numeric(bridge_dist_plot_run_1$bridge_cor_dist)

#where are there only 4 participants here? why are there only 2 bars?
plot_bridge_dist_1 <- ggplot() +
  geom_line(data=bridge_dist_plot_run_1, aes(y=mean_acc,x=bridge_cor_dist), size = 1.3) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22)
plot_bridge_dist_1

#plot run 2
bridge_dist_plot_run_2<-cast(int_full, bridge_cor_dist~., mean, value="acc", na.rm=T, subset = (Int_All_Runs.thisN == 1 & cond == "Int"))
colnames(bridge_dist_plot_run_2)[colnames(bridge_dist_plot_run_2)=="(all)"] <- "mean_acc"
bridge_dist_plot_run_2<-subset(bridge_dist_plot_run_2, !is.na(bridge_cor_dist))
print(bridge_dist_plot_run_2)
bridge_dist_plot_run_2$bridge_cor_dist<-as.numeric(bridge_dist_plot_run_2$bridge_cor_dist)

#where are there only 4 participants here? why are there only 2 bars?
plot_bridge_dist_2 <- ggplot() +
  geom_line(data=bridge_dist_plot_run_2, aes(y=mean_acc,x=bridge_cor_dist), size = 1.3) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22)
plot_bridge_dist_2


#plot run 3
bridge_dist_plot_run_3<-cast(int_full, bridge_cor_dist~., mean, value="acc", na.rm=T, subset = (Int_All_Runs.thisN == 2 & cond == "Int"))
colnames(bridge_dist_plot_run_3)[colnames(bridge_dist_plot_run_3)=="(all)"] <- "mean_acc"
bridge_dist_plot_run_3<-subset(bridge_dist_plot_run_3, !is.na(bridge_cor_dist))
print(bridge_dist_plot_run_3)
bridge_dist_plot_run_3$bridge_cor_dist<-as.numeric(bridge_dist_plot_run_3$bridge_cor_dist)

#where are there only 4 participants here? why are there only 2 bars?
plot_bridge_dist_3 <- ggplot() +
  geom_line(data=bridge_dist_plot_run_3, aes(y=mean_acc,x=bridge_cor_dist), size = 1.3) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22)
plot_bridge_dist_3

#plot run 3
bridge_dist_plot_run_4<-cast(int_full, bridge_cor_dist~., mean, value="acc", na.rm=T, subset = (Int_All_Runs.thisN == 3 & cond == "Int"))
colnames(bridge_dist_plot_run_4)[colnames(bridge_dist_plot_run_4)=="(all)"] <- "mean_acc"
bridge_dist_plot_run_4<-subset(bridge_dist_plot_run_4, !is.na(bridge_cor_dist))
print(bridge_dist_plot_run_4)
bridge_dist_plot_run_4$bridge_cor_dist<-as.numeric(bridge_dist_plot_run_4$bridge_cor_dist)

#where are there only 4 participants here? why are there only 2 bars?
plot_bridge_dist_4 <- ggplot() +
  geom_line(data=bridge_dist_plot_run_4, aes(y=mean_acc,x=bridge_cor_dist), size = 1.3) +
  ylim(c(0,1.0)) +
  theme_grey(base_size = 22)
plot_bridge_dist_4

library(gridExtra)
grid_runXbridge<-grid.arrange(plot_bridge_dist_1, plot_bridge_dist_2,plot_bridge_dist_3, plot_bridge_dist_4, ncol = 2, nrow=2)

pred_acc_plots<-grid.arrange(acc_plot, plot_path, plot_distance, plot_distance_time, ncol = 2, nrow = 2)
acc_plot
plot_path
plot_distance
plot_distance_time