rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  f = read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  if (!state %in% f$State){
    stop("invalide state")
  }
  if (!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invalide outcome")
  }
  colnumber=c(11,17,23)[c("heart attack","heart failure","pneumonia")==outcome]
  if (num != "best" && num !="worst" && num > sum(!is.na(as.numeric(f[f$State==state,colnumber])))){
    stop(NA)
  }
  if (num=="best"){
    num = 1
  }
  ## Return hospital name in that state with the given rank ## 30-day death rate
  m=f[f$State==state,c(2,colnumber)]
  m=m[!is.na(as.numeric(as.character(m[,2]))),]
  m=m[order(as.numeric(as.character(m[,2])),as.character(m[,1])),]
  if (num=="worst"){
    return(as.character(tail(m[,1],1)))
  }
  as.character(m[,1][num])
}
