
#rewrite to your working directory
setwd("D:\\Adam\\Szakdolgozat\\_Gitre\\ReMo_data_analysis\\data_googleforms")
#Processing all form data
#it will take a while
exportAllAnswersCsv(14,"all_answers.csv")
exportAllFormsCsv(14,"all_forms.csv")

answers_all <- read_csv2("all_answers.csv")
answers_all <- addEmptyVariable(answers_all)
forms_all <- read_csv2("all_forms.csv")
#rewrite to your working directory
setwd("D:\\Adam\\Szakdolgozat\\_Gitre\\ReMo_data_analysis\\data_tests")

exportAllTestTables()

zh_1 <- as.data.frame(unclass(read_csv2("zh-1.csv")))
zh_2 <- as.data.frame(unclass(read_csv2("zh-2.csv")))
zh_3 <- as.data.frame(unclass(read_csv2("zh-3.csv")))
zh_4 <- as.data.frame(unclass(read_csv2("zh-4.csv")))

zh_all <- as.data.frame(unclass(read_csv2("zh-all.csv")))

zh_1_answers <- as.data.frame(unclass(read_csv2("zh-1-answers.csv")))
zh_2_answers <- as.data.frame(unclass(read_csv2("zh-2-answers.csv")))
zh_3_answers <- as.data.frame(unclass(read_csv2("zh-3-answers.csv")))
zh_4_answers <- as.data.frame(unclass(read_csv2("zh-4-answers.csv")))

zh_all_answers <- as.data.frame(unclass(read_csv2("zh-all-answers.csv")))
zh_all_answers$test_id <- as.factor(zh_all_answers$test_id)
zh_all_answers$type <- as.factor(zh_all_answers$type)
zh_all_answers$subtype <- as.factor(zh_all_answers$subtype)

#rewrite to your working directory
setwd("D:\\Adam\\Szakdolgozat\\_Gitre\\ReMo_data_analysis")