library(tidyverse)
library(plyr)

readMeta <- function(filename) {
  return(read.table(filename, sep = ",", col.names = c("id", "question_type", "question_subtype", "a", "b", "c", "d")))
}

readForm <- function(filename, meta, formid) {
  question_names <- c()
  question_names[1] = "date"
  question_names[2] = "score"
  offset = 2
  for (i in seq_along(meta$question_type)) {
    question_names[i+offset] <- paste("col", i, sep = "")
  }
  table <- read.csv2(filename, col.names = question_names, encoding="UTF-8")
  table = table[-1, ]
  #prevention of misterious double spaces
  for(i in seq_along(table)) {
    table[[i]] <- gsub("  "," ",table[[i]])
  }
  table$score <- as.numeric(table$score)
  table$form_id <- formid;
  table$date <- as.POSIXct(table$date,format = "%m.%d.%Y %H:%M:%S")
  return(table)
}

readAnswers <- function(filename, meta, formid) {
  form <- readForm(filename, meta, formid)
  answers <- data.frame(date = character(),
                        score = numeric(),
                        type = character(),
                        subtype = character(),
                        form_id = character(),
                        question_id = numeric(),
                        answer = character())
  for (i in seq_along(form$date)) {
    for (j in seq_along(meta$id)) {
      newrow <- data.frame(date = form$date[i],
                           score = 0,
                           type = meta$question_type[j],
                           subtype = meta$question_subtype[j],
                           form_id = formid,
                           question_id = j,
                           answer = form[[j+2]][[i]])
      answers <- rbind(answers, newrow)
    }
  }
  answers$date <- as.POSIXct(answers$date,format = "%m.%d.%Y %H:%M:%S")
  return(answers)
}

readQuestions <- function(filename) {
  table <- read.table(filename, sep = ",", col.names = c("id", "a", "b", "c", "d"), encoding="UTF-8")
  for(i in seq_along(table)) {
    table[[i]] <- gsub("  "," ",table[[i]])
    table[[i]] <- gsub("\\\\", "\\\\\\\\", table[[i]])
  }
  return(table)
}

makeRegex <- function(text) {
  text <- gsub("\\)", "\\\\)", text)
  text <- gsub("\\(", "\\\\(", text)
  text <- gsub("\\.", "\\\\.", text)
  text <- gsub("\\+", "\\\\+", text)
  text <- gsub("\\?", "\\\\?", text)
  text <- gsub("\\[", "\\\\[", text)
  text <- gsub("\\]", "\\\\]", text)
  text <- gsub("\\$", "\\\\$", text)
  text <- gsub("\\*", "\\\\*", text)
}

resolveForm <- function(answers, questions) {
  ids <- c("a","b","c","d")
  firstcolumn = 3
  for(i in seq_along(questions$id)) {
    for(j in seq_along(ids)) {
      answers$newCol <- 0
      answerid = grep(makeRegex(questions[i, 1+j]), answers[[firstcolumn]])
      answers$newCol[answerid] <- 1
      
      col = paste(colnames(answers)[firstcolumn],ids[j], sep="-")
      colnames(answers)[length(answers)] <- col
    }
    answers[[firstcolumn]] <- NULL
  }
  return(as.data.frame(answers))
}

resolveAnswers <- function(answers, questions) {
  ids <- c("a","b","c","d")
  for(j in seq_along(ids)) {
      answers$newCol <- 0
    for(i in seq_along(questions$id)) {
      answers$newCol[grep(makeRegex(questions[i, 1+j]), answers[[7]])] <- 1
    }
    colnames(answers)[length(answers)] <- paste(ids[j])
  }
  answers$answer <- NULL
  return(as.data.frame(answers))
}

calculateFormScore <- function(answers, meta) {
  metaoffset = 4
  answeroffset = 4
  for(row in seq_along(answers$score)) {
    answerpoint = 0
    for(keyrow in seq_along(meta$id)) {
      inputcol = answeroffset+(keyrow-1)*4
      key = c(meta[[metaoffset]][[keyrow]], meta[[metaoffset+1]][[keyrow]], meta[[metaoffset+2]][[keyrow]], meta[[metaoffset+3]][[keyrow]])
      input = c(answers[[inputcol+0]][[row]],answers[[inputcol+1]][[row]],answers[[inputcol+2]][[row]],answers[[inputcol+3]][[row]])
      answerpoint = answerpoint + calculatePoints(key, input)
    }
    answers$score[row] = answerpoint
  }
  return(answers)
}

calculateScore <- function(answers, meta) {
  metaoffset = 4
  answeroffset = 6
  for(row in seq_along(answers$score)) {
    keyrow = answers$question_id[row]
    key = c(meta[[metaoffset]][[keyrow]], meta[[metaoffset+1]][[keyrow]], meta[[metaoffset+2]][[keyrow]], meta[[metaoffset+3]][[keyrow]])
    input = c(answers$a[row],answers$b[row],answers$c[row],answers$d[row])
    answers$score[row] = calculatePoints(key, input)
  }
  return(answers)
}

calculatePoints <- function(key, input) {
  points = 0
  goodanswers = which(key == c(1,1,1,1))
  markedanswers = which(input == c(1,1,1,1))
  if (all(key == input)) points = 1
  else if (all(input == c(0,0,0,0))) points = 0
  else {
    for(i in seq_along(goodanswers)) {
      if(input[goodanswers[i]] == 1) points = points + 1/length(goodanswers)
    }
    for(i in seq_along(markedanswers)) {
      if(key[markedanswers[i]] == 0) points = points - 0.25
    }
    if(points < -1) points = -1
  }
  return(points)
}

cleanFormImport <- function(answersfilename, questionsfilename, metafilename, formid) {
  meta <- readMeta(metafilename)
  questions <- readQuestions(questionsfilename)
  answers <- readForm(answersfilename, meta, formid)
  answers <- resolveForm(answers, questions)
  answers <- calculateFormScore(answers, meta)
  return(answers)
}

cleanFormAllImport <- function(csvcount) {
  allforms <- data.frame()
  for(i in 1:csvcount) {
    answersfilename = paste(i, "-answers.csv", sep="")
    questionsfilename = paste(i, "-questions.txt", sep="")
    metafilename = paste(i, "-meta.txt", sep="")
    newForm <- cleanFormImport(answersfilename, questionsfilename, metafilename, i)
    allforms <- rbind.fill(allforms, newForm)
  }
  return(allforms)
}

cleanAnswerImport <- function(answersfilename, questionsfilename, metafilename, formid) {
  meta <- readMeta(metafilename)
  questions <- readQuestions(questionsfilename)
  answers <- readAnswers(answersfilename, meta, formid)
  answers <- resolveAnswers(answers, questions)
  answers <- calculateScore(answers, meta)
  return(answers)
}

cleanAnswerAllImport <- function(csvcount) {
  answers <- data.frame(date = as.Date(character()),
    score = numeric(),
    type = character(),
    subtype = character(),
    form_id = character(),
    question_id = numeric(),
    answer = character())
  for(i in 1:csvcount) {
    answersfilename = paste(i, "-answers.csv", sep="")
    questionsfilename = paste(i, "-questions.txt", sep="")
    metafilename = paste(i, "-meta.txt", sep="")
    newAnswers <- cleanAnswerImport(answersfilename, questionsfilename, metafilename, i)
    answers <- rbind(answers, newAnswers)
  }
  return(answers)
}

addEmptyVariable <- function(answers) {
  answers <- answers %>%
    mutate(
      empty = (a==0 & b==0 & c==0 & d==0 & score==0)) 
}

exportAllFormsCsv <- function(csvcount, filename) {
  write.csv2(cleanFormAllImport(csvcount), file = filename, row.names=FALSE)
}

exportAllAnswersCsv <- function(csvcount, filename) {
  write.csv2(cleanAnswerAllImport(csvcount), file = filename, row.names=FALSE)
}