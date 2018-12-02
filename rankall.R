## rank all hospital with respect to state
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  f =read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalide outcome")
  }
  i=c(11,17,23)[c("heart attack","heart failure","pneumonia")== outcome]
  ## For each state, find the hospital of the given rank
   ## split by state name, return a list of data frame
  df=data.frame(hospital=character(),state=character()) #create empty dataframe
  s=as.vector(unique(f[,7]))
  for (j in s){
    h=f[f$State==j,c(2,7,i)]
    a=!is.na(as.numeric(as.character(h[,3])))
    h=h[a,]  ##drop na rows
    if (nrow(h)==0){
      df=rbind(df,data.frame(hospital="<NA>",state=j))
    }else {
      m=h[order(as.numeric(as.character(h[,3])),as.character(h[,1])),]
      if (num =="best"){
        df = rbind(df,data.frame(hospital=m[1,1],state=m[1,2]))
      }else if (num =="worst"){
        df = rbind(df,data.frame(hospital=m[nrow(m),1],state=m[nrow(m),2]))
      }else {
        if (num > nrow(m)){
          df = rbind(df, data.frame(hospital="<NA>",state=j))
        }else {
          df = rbind(df, data.frame(hospital=m[num,1],state=m[num,2]))
        }
      }
    }
    
  }
  df
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
}