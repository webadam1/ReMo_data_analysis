readTestResults <- function(filename) {
  return(read.csv2(filename, header = TRUE))
}

readTestMeta <- function(filename) {
  return(read.table(filename, sep = ",", col.names = c("test_id", "group_id", "question_id", "question_type", "question_subtype")))
}

generateColnames <- function(zh) {
  columns = c(1:length(zh))
  columns[1:3] = c("Neptun","Course","Group")
  for(i in 0:9) {
    columns[3 + i * 4 + 1] = paste("p", i+1, "a", sep="")
    columns[3 + i * 4 + 2] = paste("p", i+1, "b", sep="")
    columns[3 + i * 4 + 3] = paste("p", i+1, "c", sep="")
    columns[3 + i * 4 + 4] = paste("p", i+1, "d", sep="")
  }
  columns[length(zh) - 5] = "Prereq_score"
  columns[length(zh) - 4] = "Prereq_passed"
  columns[length(zh) - 3] = "F1"
  columns[length(zh) - 2] = "F2"
  columns[length(zh) - 1] = "Sum"
  columns[length(zh) - 0] = "Extra_points"
  return(columns)
}

removeTaskPointCols <- function(zh) {
  end = length(zh)-6
  zh[,44:end] <- NULL
  return(zh)
}

removeAbsents <- function(zh) {
  return(subset(zh, !(zh$Sum == "Nem jelent meg")))
}

groupLetterToNum <- function(zh) {
  myLetters <- toupper(letters[1:26])
  zh$Group <- match(zh$Group, myLetters)
  return(zh)
}

factorToNum <- function(x) {
  return(as.numeric(as.character(x)))
}

setTypes <- function(zh) {
  #the Sum column contained text, so when importing, the decimals stayed ','
  zh$Sum <- gsub(",",".",zh$Sum)
  zh[, c(4:44,46:49)] = sapply(zh[, c(4:44,46:49)], factorToNum)
  return(zh)
}

exportTestsByBounds <- function(baseData, offset, testBoundaries) {
  start = offset+1
  for(i in seq_along(testBoundaries)) {
    zh <- baseData[,c(1:offset,start:(testBoundaries[i]-1))]
    zh <- removeTaskPointCols(zh)
    colnames(zh) = generateColnames(zh)
    
    zh <- groupLetterToNum(zh)
    zh <- removeAbsents(zh)
    zh <- setTypes(zh)
    
    write.csv2(zh, file = paste("zh-",i,".csv", sep=""), row.names = FALSE)
    start = testBoundaries[i]
  }
}

importTestById <- function(id) {
  zh <- read.csv2(paste("zh-",id,".csv", sep=""))
  zh$test_id <- id
  return(zh)
}

testToAnswersTable <- function(zh, metafilenames) {
  metas = list()
  for(i in seq_along(metafilenames)) {
    metas[[i]] = readTestMeta(metafilenames[i])
  }
  answers <- data.frame(test_id = factor(),
                        group_id = factor(),
                        question_id = factor(),
                        score = numeric(),
                        type = factor(),
                        subtype = factor(),
                        a = numeric(),
                        b = numeric(),
                        c = numeric(),
                        d = numeric()
                        )
  for (i in seq_along(zh$Neptun)) {
    meta = metas[[zh$Group[i]]]
    for (j in seq_along(meta$question_id)) {
      colNum = 4 + ((j - 1) * 4)
      colNumEnd = colNum + 3
      pointSum = sum(as.numeric(zh[i, colNum:colNumEnd]))
      newrow <- data.frame(test_id = zh$test_id[j],
                          group_id = zh$Group[i],
                          question_id = meta$question_id[j],
                          score = pointSum,
                          type = meta$question_type[j],
                          subtype = meta$question_subtype[j],
                          a = zh[i, colNum + 0],
                          b = zh[i, colNum + 1],
                          c = zh[i, colNum + 2],
                          d = zh[i, colNum + 3]
                          )
      answers <- rbind(answers, newrow)
    }
  }
  return(answers)
}

exportAllTestTables <- function(filename = "testresults.csv", myTestBoundaries = c(63,125,186,246)) {
  tests_all <- readTestResults(filename)
  exportTestsByBounds(tests_all,2,myTestBoundaries)
  
  zh_all = data.frame()
  zh_all_answers = data.frame()
  
  for(i in seq_along(myTestBoundaries)) {
    zh <- importTestById(i)
    zh_all <- rbind(zh_all, zh)
    zh_answers <- testToAnswersTable(zh, c(paste("zh-",i,"-a-meta.txt",sep=""),paste("zh-",i,"-b-meta.txt",sep="")))
    write.csv2(zh_answers, file = paste("zh-",i,"-answers.csv",sep=""), row.names = FALSE)
    zh_all_answers <- rbind(zh_all_answers,zh_answers)
  }
  
  write.csv2(zh_all, file = "zh-all.csv", row.names = FALSE)
  
  write.csv2(zh_all_answers, file = "zh-all-answers.csv", row.names = FALSE)
}

makeCorrPlot <- function() {
  M <- NULL
  M <- rbind(M,data.frame(zh_all_answers$score,as.numeric(zh_all_answers$type),as.numeric(zh_all_answers$subtype)))
  colnames(M) <- c("score","type","subtype")
  for(i in seq_along(levels(zh_all_answers$type))) {
    colName = levels(zh_all_answers$type)[i]
    M[[colName]] <- with(M, as.numeric(type == i))
  }
  for(i in seq_along(levels(zh_all_answers$subtype))) {
    colName = levels(zh_all_answers$subtype)[i]
    M[[colName]] <- with(M, as.numeric(subtype == i))
  }
  M <- cor(M)
  corrplot(M, method = "circle")
}