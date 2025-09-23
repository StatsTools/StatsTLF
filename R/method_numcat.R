mtnumcat <- function(dataset){
  j <- 1
  for(i in colnames(dataset)){
    if(stringr::str_sub(attr(dataset[[i]], "label"), -4, -1) == " (N)"){
      dataset <- dataset |>
        derive2(var_target = !!colnames(dataset)[j],
                var_source = !!colnames(dataset)[j-1],
                by = rlang::exprs(USUBJID))
    }

    j <- j + 1
  }
  return(dataset)
}

vaicorinthia <- mtnumcat(adsl)
