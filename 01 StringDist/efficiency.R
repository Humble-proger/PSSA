# Установка зеркала CRAN
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Функция для установки и загрузки пакетов
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Устанавливаем необходимые пакеты
tryCatch({
  install_and_load("stringdist")
  install_and_load("microbenchmark")
}, error = function(e) {
  cat("Ошибка при установке пакетов:", e$message, "\n")
})

# Функция для генерации случайной строки заданной длины
generate_random_string <- function(length) {
  paste0(sample(c(letters, LETTERS, " "), length, replace = TRUE), collapse = "")
}

# Функция для тестирования производительности одной метрики
benchmark_metric <- function(string1, string2, method, q = NULL, p = NULL) {
  if (!is.null(q) && !is.null(p)) {
    result <- microbenchmark(
      stringdist(string1, string2, method = method, q = q, p = p),
      times = 50
    )
  } else if (!is.null(q)) {
    result <- microbenchmark(
      stringdist(string1, string2, method = method, q = q),
      times = 50
    )
  } else if (!is.null(p)) {
    result <- microbenchmark(
      stringdist(string1, string2, method = method, p = p),
      times = 50
    )
  } else {
    result <- microbenchmark(
      stringdist(string1, string2, method = method),
      times = 50
    )
  }
  
  return(median(result$time) / 1e6) # Возвращаем медианное время в миллисекундах
}

# Функция для тестирования всех метрик на одной паре строк
benchmark_all_metrics <- function(string1, string2, length) {
  results <- data.frame(
    length = length,
    levenshtein = benchmark_metric(string1, string2, "lv"),
    damerau_levenshtein = benchmark_metric(string1, string2, "dl"),
    hamming = ifelse(nchar(string1) == nchar(string2),
                    benchmark_metric(string1, string2, "hamming"),
                    NA),
    jaccard_q2 = benchmark_metric(string1, string2, "jaccard", q = 2),
    jaccard_q3 = benchmark_metric(string1, string2, "jaccard", q = 3),
    cosine_q2 = benchmark_metric(string1, string2, "cosine", q = 2),
    cosine_q3 = benchmark_metric(string1, string2, "cosine", q = 3),
    qgram_q2 = benchmark_metric(string1, string2, "qgram", q = 2),
    qgram_q3 = benchmark_metric(string1, string2, "qgram", q = 3),
    jaro = benchmark_metric(string1, string2, "jw", p = 0),
    jaro_winkler = benchmark_metric(string1, string2, "jw", p = 0.1),
    lcs = benchmark_metric(string1, string2, "lcs"),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Запускаем бенчмаркинг с кратно увеличивающейся длиной строк
cat("ЗАПУСК АНАЛИЗА ПРОИЗВОДИТЕЛЬНОСТИ С КРАТНЫМ УВЕЛИЧЕНИЕМ ДЛИНЫ\n")
cat("============================================================\n\n")

# Длины строк: от 2^3 до 2^10 (кратное увеличение)
lengths <- 2^(6:12)  # 8, 16, 32, 64, 128, 256, 512, 1024 символов
performance_results <- data.frame()

for (len in lengths) {
  cat("Тестирование для длины:", len, "символов...\n")
  
  # Генерируем две случайные строки одинаковой длины
  str1 <- generate_random_string(len)
  str2 <- generate_random_string(len)
  
  # Добавляем некоторые различия во вторую строку
  if (len > 10) {
    # Заменяем некоторые символы для создания различий
    positions <- sample(1:len, min(5, len %/% 10))
    for (pos in positions) {
      substr(str2, pos, pos) <- sample(letters, 1)
    }
  }
  
  result <- benchmark_all_metrics(str1, str2, len)
  performance_results <- rbind(performance_results, result)
  
  cat("Завершено для длины", len, "символов\n")
  cat("Пример строк: \"", substr(str1, 1, 20), "...\" vs \"", substr(str2, 1, 20), "...\"\n", sep = "")
  cat(paste0(rep("-", 60), collapse = ""), "\n\n")
}

# Вывод результатов
cat("РЕЗУЛЬТАТЫ АНАЛИЗА ПРОИЗВОДИТЕЛЬНОСТИ\n")
cat("=====================================\n\n")

# Таблица с результатами для всех длин
cat("Зависимость времени выполнения (мс) от длины строк:\n\n")
cat("Длина |  Lev  | D-Lev |  Ham  | Jac2  | Jac3  | Cos2  | Cos3  | Qgr2  | Qgr3  |  Jaro | J-Wink|  LCS  \n")
cat(paste0(rep("-", 110), collapse = ""), "\n")

for (i in 1:nrow(performance_results)) {
  cat(sprintf("%5d | %5.2f | %5.2f | %5s | %5.2f | %5.2f | %5.2f | %5.2f | %5.2f | %5.2f | %5.2f | %5.2f | %5.2f\n",
              performance_results$length[i],
              performance_results$levenshtein[i],
              performance_results$damerau_levenshtein[i],
              ifelse(is.na(performance_results$hamming[i]), "  NA", sprintf("%5.2f", performance_results$hamming[i])),
              performance_results$jaccard_q2[i],
              performance_results$jaccard_q3[i],
              performance_results$cosine_q2[i],
              performance_results$cosine_q3[i],
              performance_results$qgram_q2[i],
              performance_results$qgram_q3[i],
              performance_results$jaro[i],
              performance_results$jaro_winkler[i],
              performance_results$lcs[i]))
}