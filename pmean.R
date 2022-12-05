getcsv <- function(directory, file_id) {
  return(read.csv(paste0(directory, "/", formatC(file_id, width = 3, flag = "0"), ".csv")))
}
### 1.Написати функцію pmean, яка обчислює середнє значення (mean) забруднення сульфатами або нітратами серед заданого переліка моніторів.
pmean <- function(directory, pollutant, id = 1:332) {
  data = NULL
  for (i in id) {
    data = c(data, getcsv(directory, i)[[pollutant]])
  }
  return(mean(data, na.rm = TRUE))
}
### 2.Написати функцію complete, яка виводить кількість повних спостережень (the number of completely observed cases) для кожного файлу.
complete <- function(directory, id = 1:332) {
  cases = NULL
  for (i in id) {
    cases = c(cases, nrow(na.omit(getcsv(directory, i))))
  }
  return(data.frame(id, cases))
}
### 3.Написати функцію corr, яка приймає два аргументи: directory (папка, де знаходяться файли спостережень) та threshold (порогове значення, за замовчуванням дорівнює 0) та обчислює кореляцію між сульфатами та нітратами для моніторів, кількість повних спостережень для яких більше порогового значення.
corr <- function(directory, threshold = 0) {
  result = numeric()
  for (i in 1:332) {
    cases = na.omit(getcsv(directory, i))
    if (nrow(cases) > threshold) {
      result = c(result, cor(cases$sulfate, cases$nitrate))
    }
  }
  return(result)
}