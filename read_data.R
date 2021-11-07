read_data <- function(){
  d1=read.table("student-mat.csv",sep=";",header=TRUE)
  d2=read.table("student-por.csv",sep=";",header=TRUE)
  index = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
  d3=merge(d1,
           d2,
           by=index,suffixes = c("_m", "_p"))
  return (d3)
}