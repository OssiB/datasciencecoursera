pollutantmean <- function(directory, pollutant, id = 1:332) {
        meanVector=numeric(length(id));
        options(digits=4)
        pollutants<-c("nitrate"=3,"sulfate"=2);
        diff<-id[1]-1;
        sum=0;
        fileStart<-paste(directory,"/",sep="");
        count=0;
        for(x in id){
                weather<-read.csv(paste(fileStart,sprintf("%03s.csv",x),sep=""),header=TRUE,sep=",");
                nitrate<-weather[,pollutants[pollutant]];
                bad<-is.na(nitrate);
                sum<-sum+sum(nitrate[!bad]);
                count<-count+length(nitrate[!bad]);
                meanVector[x-diff]<-mean(nitrate[!bad]);

        } 
        sum/count;
}