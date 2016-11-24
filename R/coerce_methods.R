setOldClass("json")

setAs(
  from = "three", to = "json",
  def = function(from){
    paste(
      unlist(lapply(
        names(from@json),
        function(name){ paste(name, " = ", from@json[[name]], ";", sep="") })
      ),
      collapse = "\n"
    )    
  }
)