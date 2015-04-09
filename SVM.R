profiles = read.csv("/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/fourth run/f.profiles/profiles_2014_CM_spell_check_synom.csv")
profiles2 = read.csv("/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/third run/f.profiles/profiles_2014_CM_spell_check_synom.csv")
num <- 50;
num_seven <- 35;
num_three <- 15;
num2 <- 158;
num_seven2 <- 111;
num_three2 <- 47;

path_rating <- "/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/fourth run/supervised learning/50CM.csv"
path_rating2 <- "/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/fifth run/supervised learning/3.cm.csv"
#path_cri <- "/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/fourth run/supervised learning/6_ES_original.csv"
rating_category <- 4
#path_SVM <- "/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/fourth run/supervised learning/spell_check_synom_SS2_SVM_result.csv"
#path_log <- "/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/fourth run/supervised learning/spell_check_synom_SS2_LOG_result.csv"
#path_SVM_cross <- "/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/fourth run/supervised learning/spell_check_synom_SS2_SVM_cross.csv"
#path_log_cross <- "/Users/keleigong/Google Drive/SCRC 2015 work/2014_data/fourth run/supervised learning/spell_check_synom_SS2_LOG_cross.csv"

dim(profiles) 
#--------------------- remove empty lines
i = 2
dataset = profiles[,1:2]
result = dataset[1,]
while(i < dim(dataset)[1]+1){
  if(dataset[i,1] != ""){
    result = rbind(result, dataset[i,])
  }
  i =i+1
}
profiles = as.matrix(result)
# check dimension of company profile
dim(profiles) 

#------------------- combine additional lines
i = 1
num_companies = num
profiles_final = matrix(nrow=num_companies,ncol=2)
count = 1
while(i < dim(profiles)[1] + 1 && count < num_companies + 1)
{
  profiles_final[count,1] = profiles[i,1]
  profiles_final[count,2] = profiles[i,2]
  i = i+1
  if(i < dim(profiles)[1] + 1 && regexpr(".txt", profiles[i,1]) < 0)
  {
    while(i < dim(profiles)[1] + 1 && regexpr(".txt", profiles[i,1]) < 0)
    {
      profiles_final[count,2] = paste(profiles_final[count,2], profiles[i,1])
      i = i + 1
    }
  }
  count = count + 1
}
profiles = profiles_final



#============ profiles 2
dim(profiles2) 
#--------------------- remove empty lines
i = 2
dataset2 = profiles2[,1:2]
result2 = dataset2[1,]
while(i < dim(dataset2)[1]+1){
  if(dataset2[i,1] != ""){
    result2 = rbind(result2, dataset2[i,])
  }
  i =i+1
}
profiles2 = as.matrix(result2)
# check dimension of company profile
dim(profiles2) 

#------------------- combine additional lines
i = 1
num_companies2 = num2
profiles_final2 = matrix(nrow=num_companies2,ncol=2)
count = 1
while(i < dim(profiles2)[1] + 1 && count < num_companies2 + 1)
{
  profiles_final2[count,1] = profiles2[i,1]
  profiles_final2[count,2] = profiles2[i,2]
  i = i+1
  if(i < dim(profiles2)[1] + 1 && regexpr(".txt", profiles2[i,1]) < 0)
  {
    while(i < dim(profiles2)[1] + 1 && regexpr(".txt", profiles2[i,1]) < 0)
    {
      profiles_final2[count,2] = paste(profiles_final2[count,2], profiles2[i,1])
      i = i + 1
    }
  }
  count = count + 1
}
profiles2 = profiles_final2
# check dimension of company profile
dim(profiles2) 
#================


#--------------------- load rating created by human
ratings = read.csv(path_rating)
profiles = as.matrix(profiles)
ratings = as.matrix(ratings)

ratings2 = read.csv(path_rating2)
profiles2 = as.matrix(profiles2)
ratings2 = as.matrix(ratings2)

dim(profiles)
dim(ratings)

dim(profiles2)
dim(ratings2)

pr = rbind(profiles2,profiles)
rt = rbind(ratings2,ratings[,1:7])

pr = profiles
rt = ratings
#adding all the company profiles to a vector
i = 2;
num_profiles = dim(pr)[1]
docs = pr[1,2];
while(i < num_profiles + 1)
{
  docs = rbind(docs, pr[i,2])
  i = i+1;
}
dim(docs)



#creating a corpus
corp = Corpus(VectorSource(docs));
#creating the term-document matrix
tdm = TermDocumentMatrix(corp);
tdm = inspect(tdm);

#alter the column based on the criteria you are classifying
# 2-Spend Management  
# 3-Strategic Sourcing  
# 4-Category Management 
# 5-SRM	
# 6-LHR	
# 7-Sustainability 
classData = as.matrix(rt[,rating_category]);
no_training = 35
no_testing = 15
#extracting train and test sets
tdm = t(tdm)
train = tdm[1:no_training,]
test = tdm[(no_training+1):(no_training+no_testing),]
dim(tdm)
dim(train)
dim(test)
classTrain = classData[1:no_training]
classTest = classData[(no_training+1):(no_training+no_testing)]
classTrain = as.matrix(classTrain)
classTest = as.matrix(classTest)

colnames(classTrain) = c("Class")
colnames(classTest) = c("Class")
classTrain = classData[1:no_training]
classTest = classData[(no_training+1):(no_training+no_testing)]
#train = cbind(train, classTrain)
#test = cbind(test, classTest)
dim(train)
dim(test)

model = LiblineaR(data=train,target=classTrain,type=4,cost=1,bias=TRUE,verbose=FALSE)
p = predict(model,test)
matrix = confusion(p$predictions, classTest) #the confusion matrix
matrix
#write.csv(matrix,path_SVM)

model = LiblineaR(data=train,target=classTrain,type=6,cost=1,bias=TRUE,verbose=FALSE)
p = predict(model,test)
matrix = confusion(p$predictions, classTest) #the confusion matrix
matrix
#write.csv(matrix,path_log)

model = LiblineaR(data=rbind(train,test),target=c(classTrain,classTest),type=4,cost=1,bias=TRUE,verbose=FALSE,cross=10)
model
#write.csv(model,path_SVM_cross)

#logistic regression
model = LiblineaR(data=rbind(train,test),target=c(classTrain,classTest),type=6,cost=1,bias=TRUE,verbose=FALSE,cross=10)
model
#write.csv(model,path_log_cross)
