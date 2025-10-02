auto_pred <- function(dataset = adae,
                      from = pre_adsl,
                      by_argumento = rlang::exprs(USUBJID)){
  for(i in base::colnames(dataset)){
    if(base::attributes(dataset[[i]])$origin == "Predecessor" &
       base::sub("\\..*", "", base::attributes(dataset[[i]])$predecessor) == base::attributes(from)$name){
      dataset <- dataset |>
        StatsTLF::derive(var = !!i,
                         from = list(origem = from),
                         by = by_argumento,
                         cases = list(
                           list(
                             condition = StatsTLF::derive_expr(TRUE),
                             value = StatsTLF::derive_expr(origem[[i]])
                           )
                         ),
                         default = NA
        )
    }
  }
  return(dataset)
}


adae <- auto_pred(dataset = adae,
                  from = pre_adsl,
                  by = rlang::exprs(USUBJID))
