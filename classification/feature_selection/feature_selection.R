
kruskal <- function(dat,       # dataset 
                    target,    # binary class target variable
                    var_list   # features
){
  test_output <- c()
  for(i in 1:length(var_list)){
    #  print(log_cnt_var[i])
    #  print(kruskal.test(eval(parse(text =  paste0(log_cnt_var[i], " ~ regular7_target_yn"))), 
    #                     data = log_cnt)$statistic) 
    test_output[[i]] <- data.frame(var = var_list[i], 
                                   statistics = kruskal.test(eval(parse(text =  paste0(var_list[i], paste0(" ~ ",target)))), 
                                                             data = dat)$statistic,
                                   p_value = kruskal.test(eval(parse(text =  paste0(var_list[i], paste0(" ~ ",target)))), 
                                                          data = dat)$p.value)
  }
  
  ins <- do.call("rbind", test_output)
  test_table <- ins[order(-ins$statistics),]
  return(test_output = test_table)
}

feature.select <- function(dat,        # dataset 
                           target,     # binary class target variable
                           var_list,   # features
                           th          # correlation threshould
){
  test_output <- kruskal(valid2, target, var_list)
  corr <- data.frame(cor(valid2[, var_list]))
  corr$rowname <- rownames(corr)
  
  remove_var <- list()
  i = 1
  while(i <= nrow(test_output)){
    new_var_list <- corr[corr[[ as.character(test_output$var[[i]]) ]] < th | corr[[ as.character(test_output$var[[i]]) ]] == 1, c("rowname")]
    remove_var[[i]] <- corr[corr[[ as.character(test_output$var[[i]]) ]] >= th & corr[[ as.character(test_output$var[[i]]) ]] != 1, c("rowname")]
    test_output <- test_output[test_output$var %in% new_var_list,]
    corr <- corr[corr$rowname %in% new_var_list, c("rowname", new_var_list)]
    i <- i + 1
    print(nrow(corr))
  }
  selected <- corr$rowname
  return(selected)
}

