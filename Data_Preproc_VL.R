library(readr)

setwd("~/IDDO/Data")
data <- read_csv("VL_maindata.csv")
#VL main data was marked for all the columns that might be necessary fot hte analysis (110 selected out of 350+)
#Go change row number 1, put 1 above the column you need and run the script

#Following script selects the columns which were marked in the excel sheet

dcols <- (data[1,] == 1)
a<-c(unique(colnames(data)[dcols]))
a<-a[-2]
data1 <- data[,a]


#Creating a data dictionary
dict<-t(rbind(colnames(data1),data1[2,]))
rownames(dict)<-NULL
dict<-as.data.frame(dict)
#write.csv(dict,'dict.csv')
#write.csv(data1,'vldata_prep.csv')



df<-data1[c(-1,-2),]
df[is.na(df)] <- " "

#Cleaning and pre-processing

n<-max(as.numeric(df$pub_id))


new_df<-NULL

for (i in 1:n){
  tmp<-subset(df,df$pub_id==i)
  arm<-nrow(tmp)-1
  tmp1<-tmp[-1,]
  rowmain<-tmp[1,]
  tmp3<-NULL
  
  for (j in 1:arm){
    new_row<-paste0(tmp1[j,],rowmain)
    new_row[1]<-rowmain[1]
    tmp3<-rbind(tmp3,new_row)
  }
  
  tmp3<-as.data.frame(tmp3)
  new_df<-rbind(new_df,tmp3)
  
  }
colnames(new_df)<-colnames(df)
rownames(new_df)<- NULL
new_df<-as.data.frame(new_df)

# First coerce the data.frame to all-character
df2 = data.frame(lapply(new_df, as.character), stringsAsFactors=FALSE)
df2<-subset(df2, df2$st_status==" Published")
# write file
write.csv(df2,"vldata_clean_1125.csv")
#Manually cleaned for study #163, 166, 167, 181 (Unpublished single rows): 1 study arm
#Replaced NA with Blank
#Deleted additional row created
#Stored it as vldata_clean

#-------------------
