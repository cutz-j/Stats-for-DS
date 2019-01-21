install.packages("klaR")
library(klaR)
loan_data = read.csv("d:/data/sds/loan_data.csv")
naive_model <- NaiveBayes(outcome ~ purpose_ + home_ + emp_len_,
                          data=na.omit(loan_data))
naive_model$table

new_loan <- loan_data[147, c('purpose_', 'home_', 'emp_len_')]
row.names(new_loan) <- NULL
new_loan
predict(naive_model, new_loan)
new_loan
