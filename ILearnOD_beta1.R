################ Housekeeping ########################

##Install required packages if they don't exist. Then load them
list.of.packages <- c('tidyr','dplyr','stringr','RecordLinkage','lubridate','readxl')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(RecordLinkage)
library(reshape2)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(readxl)

#Custom Functions
remove.all<-function(text,remove){
  for(r in remove) text<-str_replace_all(text,r,' ')
  return(text)
}

change.na<-function(vec,new=0){ifelse(is.na(vec),new,vec)}

##Set global options
options(stringsAsFactors=FALSE,comment.char = "#")

########################### Options  #################################
######### DO NOT CHANGE ANYTHING ABOVE THIS LINE  ####################

##Set the path of the directory where all files are located
setwd("/Users/aliabbasi/Box Sync/IDPH/opioids mortality/decode death cert/ILearnOD_502/")

##### Enter the names of the files we will be using 
death.file<-"od_deaths_example.xlsx"           ### Death records, can be csv, txt, xls, xlsx
drug.names.file<-"drugs.csv"                  ### File containing the drugs we are searching for, can be csv, txt, xls, xlsx


## Name of the column in the deathd data that uniquely identifies each record
idcol<-'State_File_Number'


#### Name of the columns that contain the text we want to analyze
wordfields<-c('COD_Immediate',
              'COD_Consequence1',
              'COD_Consequence2',
              'Injury_How_Occurred',
              'COD_Other_SigConditions')


################# Advanced options #####################

## Characters that should be removed from literals before analysis
bad_chars<-c('\\(','\\)','\\,','\\`','\\"','\\.','\\/','\\{','\\}','\\:','\\;')

##Cutoffs for text comparison. The lower the cutoff, the more "false" matches we will find 
score.cutoff.1 <-0.9 #For single word comparisons
score.cutoff.2<-0.95 #For two word comparisons


###################################### Analysis #############################################
########################## DO NOT CHANGE ANYTHING BELOW THIS LINE  ##########################

### Load death data
### Detect filetype
tmp<-str_split(death.file,"\\.")
filetype<-tmp[[1]][length(tmp[[1]])]

#Read in
if(filetype=='txt') od<-read.delim(death.file,stringsAsFactors = FALSE,header = TRUE,sep="\t",quote='')
if(filetype=='csv') od<-read.csv(death.file,stringsAsFactors = FALSE,header = TRUE)
if(filetype=='xls') od<-read_xls(death.file)
if(filetype=='xlsx') od<-read_xlsx(death.file)

## Make the fields we are analyzing to lower case
for(ifield in wordfields) od[[ifield]]<-change.na(tolower(as.character(od[[ifield]])),'')

##### Load Drug data
### Detect filetype
tmp<-str_split(drug.names.file,"\\.")
filetype<-tmp[[1]][length(tmp[[1]])]

### load death data
if(filetype=='txt') drugs<-read.delim(drug.names.file,stringsAsFactors = FALSE,header = TRUE,sep="\t",quote='')
if(filetype=='csv') drugs<-read.csv(drug.names.file,stringsAsFactors = FALSE,header = TRUE)
if(filetype=='xls') drugs<-read_xls(drug.names.file)
if(filetype=='xlsx') drugs<-read_xlsx(drug.names.file)

drugs<-drugs%>%filter(name!='')%>%arrange(class)

###### Step 1: Find all the words that could be drugs ############
#Pull out text fields
cause<-character()
for(iword in wordfields) cause<-c(cause,od[[iword]])

# Remove unwanted characters such as punctuation and replace with spaces, then split into individual words
words<-remove.all(cause,bad_chars)%>%
  str_split(' ',simplify=TRUE)%>%t()%>%
  as.vector()%>%str_replace_all(' ','')

#Remove empty and duplicated words and put the words into a data.frame format
words<-words[words!='']
wordsdf<-data.frame(name=unique(words))

## Now compare the words to the list of drugs using jarow-winkler text comparison
link1<-compare.linkage(wordsdf,select(drugs,name),strcmp = TRUE,strcmpfun = jarowinkler)

## Keep only the highest scoring drug for each word
pairs1<-link1$pairs%>%group_by(id1)%>%slice(which.max(name))%>%ungroup()
pairs1$score<-pairs1$name

## Put the original names back in so we can inspect the results
pairs1$word<-wordsdf$name[pairs1$id1]
pairs1$name<-drugs$name[pairs1$id2]
pairs1<-filter(pairs1,score>score.cutoff.1,score!=1.0)

## Now make combination of two words
words2<-c(paste0(words[seq(1,length(words),by=2)]," ",words[seq(2,length(words),by=2)]),
          paste0(words[seq(2,length(words),by=2)]," ",words[seq(3,length(words),by=2)]))

## Keep only unique 
uwords2<-unique(words2)

#Keep only 2 word combination where the words have at least 5 characters
uwords2<-uwords2[(uwords2%>%str_split(" ",simplify=T)%>% nchar() %>% apply(1,min) > 4)]
  
## Put 2 word combinations in data frame format
wordsdf2<-data.frame(name=uwords2,stringsAsFactors = FALSE)

## Compare 2 word combinations with drug names
link2<-compare.linkage(wordsdf2,dplyr::select(drugs,name),strcmp = TRUE,strcmpfun = jarowinkler)
## Keep only results with highest score
pairs2<-link2$pairs%>%group_by(id1)%>%slice(which.max(name))%>%ungroup()
pairs2$score<-pairs2$name

## Put names back in so we can inspect
pairs2$word<-wordsdf2$name[pairs2$id1]
pairs2$name<-drugs$name[pairs2$id2]

## Filter according to cutoff and exclude exact matches
pairs2<-filter(pairs2,score>score.cutoff.2,score!=1.0)


## Combine 1 and 2 word pairs and output
pairs<-bind_rows(pairs1,pairs2)%>%select(-id1,-id2,-is_match)%>%
  mutate(use=1)%>%filter(score!=1)

pairs<-left_join(pairs,select(drugs,name,consensus.name))%>%select(word,name,consensus.name,score,use)%>%arrange(desc(score))

write.csv(arrange(pairs,desc(score)),'drug_spellings_OUTPUT.csv',row.names=FALSE)



################# STEP 2 Find the drugs in the death certificates. ###############
## Read in corrected version
drug.spellings.file<-'drug_spellings_use.csv'
if(!file.exists(drug.spellings.file)) stop('No input drug spellings file specified.')
pairs<-read.csv('drug_spellings_use.csv')%>%filter(use==1)

### Make columns with the different drugs
drug.spellings<-pairs%>%left_join(select(drugs,name,class),by='name')%>%select(word,name,consensus.name,class)
drug.spellings<-bind_rows(drug.spellings,drugs%>%mutate(word=name))

## Put them in the right order for text search
drug.spellings<-arrange(drug.spellings,desc(nchar(word)))
drug.spellings<-bind_rows(filter(drug.spellings,class=='trash'),
                          filter(drug.spellings,class!='trash'))


#Make copy of od data
odcp<-od

## Initiate columns for each drug
for(iname in unique(drugs$consensus.name)) odcp[[iname]]<-0

#Now find the drugs
for(j in 1:nrow(drug.spellings)){
  word<-drug.spellings$word[j]
  consensus.name<-drug.spellings$consensus.name[j]
  #Check for each field, whether the drug is found, and if it is found delete
  for(icol in wordfields){
    odcp[[consensus.name]]<-odcp[[consensus.name]]+str_count(odcp[[icol]],word)
    odcp[[icol]]<-str_replace_all(odcp[[icol]],word,'#')
  }
}

## Make sure each value is a maximum of 1
for(iname in unique(drugs$consensus.name)) odcp[[iname]]<- as.numeric(odcp[[iname]]>0)

## Remove the column that is used to throw away certain terms, only keep columns we want
result<-select(odcp,-trash)
result<-result[,c(idcol,unique(filter(drugs,class!='trash')$consensus.name))]

## Summarize by class
for(iclass in unique(filter(drugs,class!='trash')$class)){
  result[[paste0('class.',iclass)]]<-apply(select(result,one_of(filter(drugs,class==iclass)$consensus.name)),1,sum)
}


## Output
write.csv(result,'result.csv',row.names=FALSE)


# ######## Part 3: OPTIONAL. Recode the ICD-10 codes ############
# 
# 
# 
# ### Definition of ICD-10 code groups, can add more here
# icd<-list(any.opioid=c('T400', 'T401', 'T402', 'T403', 'T404', 'T406'),
#           heroin=c('T401'),
#           other.opioid=c('T402'),
#           methadone=c('T403'),
#           synthetic=c('T404'),
#           cocaine=c('T405'),
#           opioid.analgesic=c('T402', 'T403', 'T404'),
#           alcohol=c('T510'))
# 
# 
# ### What are the names of the columns that contain the cause of death
# codefield<-c('SMICAR_AXIS_1',
#              'SMICAR_AXIS_2',
#              'SMICAR_AXIS_3',
#              'SMICAR_AXIS_4',
#              'SMICAR_AXIS_5',
#              'SMICAR_AXIS_6',
#              'SMICAR_AXIS_7',
#              'SMICAR_AXIS_8',
#              'SMICAR_AXIS_9',
#              'SMICAR_AXIS_10',
#              'SMICAR_AXIS_11',
#              'SMICAR_AXIS_12',
#              'SMICAR_AXIS_13',
#              'SMICAR_AXIS_14',
#              'SMICAR_AXIS_15',
#              'SMICAR_AXIS_16',
#              'SMICAR_AXIS_17',
#              'SMICAR_AXIS_18',
#              'SMICAR_AXIS_19',
#              'SMICAR_AXIS_20')
# 
# 
# 
# ###################### ICD-10 Analysis #######################
# ## Extract fields that contain ICD-10
# cod<-od[,c(idcol,codefield)]
# 
# ## make this into a long format
# cod<-melt(cod,id.vars = idcol)
# 
# #Split the entity axis coding by dropping the first two letters and replacing spaces and ampersand
# cod<-cod%>%mutate(value=str_sub(value,3),
#                   value=str_replace_all(value,' ',''),
#                   value=str_replace_all(value,'&',""))
# 
# ## Now pull out ICD-10 codes for each
# for(iicd in names(icd)){
#   cod[[paste0('icd.',iicd)]]<- as.numeric(cod$value %in% icd[[paste0(iicd)]])
# }
# 
# cod_sum<-cod%>%select(-variable,-value)%>%
#   group_by_at(vars(idcol))%>%
#   summarize_all(funs(as.numeric(sum(.)>0)))
# 
# 
# ## Save and output
# result<-left_join(result,cod_sum,by=idcol)
# 
# ## Output
# write.csv(result,'result.csv',row.names=FALSE)



