yreplace=function(y){
  i=1;
  z=as.character(y);
  for(s in y){

     if(s==1){
       s="WALKING";
     }
     else if(s==2){
       s="WALKING_UPSTAIRS";
     }
     else if(s==3){
       s="WALKING_DOWNSTAIRS";
     }
     else if(s==4){
       s="SITTING";
     }
     else if(s==5){
       s="STANDING";
     }
     else if(s==6){
       s="LAYING";
     }
     z[i]=s;
     i=i+1;
  }
  z;
}

run_analysis=function(yourfolderpath){
  x_train1=read.table(paste(yourfolderpath,"/UCI HAR Dataset/train/x_train.txt",sep=""),sep="",header=FALSE);
  x_test1=read.table(paste(yourfolderpath,"/UCI HAR Dataset/test/x_test.txt",sep=""),sep="",header=FALSE);
  y_train=read.table(paste(yourfolderpath,"/UCI HAR Dataset/train/y_train.txt",sep=""),sep="",header=FALSE);
  x_test1=read.table(paste(yourfolderpath,"/UCI HAR Dataset/test/y_test.txt",sep=""),sep="",header=FALSE);
  sub_train=read.table(paste(yourfolderpath,"/UCI HAR Dataset/train/subject_train.txt",sep=""),sep="",header=FALSE)
  sub_test=read.table(paste(yourfolderpath,"/UCI HAR Dataset/train/subject_test.txt",sep=""),sep="",header=FALSE)
  feature=read.table(paste(yourfolderpath,"/UCI HAR Dataset/features.txt",sep=""),sep="",header=FALSE)
  
  x_merge=rbind(x_train1,x_test1);
  featurename=as.vector(feature[,2]);
  colnames(x_merge)=featurename;
  toMatch <- c(".*mean\\(\\).*", ".*std\\(\\).*");
  matches <- unique (grep(paste(toMatch,collapse="|"),feature$V2, value=TRUE));
  selected_merge_data=x_merge[,matches];
  y_merge=rbind(y_train,y_test);
  y_string=as.vector(y_merge[,1]);
  y_col=yreplace(y_string);
  activity=y_col;
  result_merge2=cbind(activity,selected_merge_data);
  result_merge3=cbind(y_string,result_merge2);
  subject=rbind(subject_train,subject_test);
  final_result=cbind(subject,result_merge3);
  s=split(final_result,final_result[,c("V1","activity")]);
  final_work=do.call(rbind,lapply(s,function(x) colMeans(x[,seq(4,69)])));
}
