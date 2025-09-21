# Установка зеркала CRAN
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Функция для установки и загрузки пакетов
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Пытаемся установить только stringdist
tryCatch({
  install_and_load("stringdist")
  install_and_load("dplyr")
  install_and_load("knitr")
}, error = function(e) {
  cat("Ошибка при установке stringdist:", e$message, "\n")
  cat("Использую базовые функции.\n")
})

# Функция для сравнения метрик
compare_metrics <- function(string1, string2, pair_name = NULL) {
  if (is.null(pair_name)) {
    pair_name <- paste0('"', string1, '" - "', string2, '"')
  }
  
  results <- data.frame(
    pair = pair_name,
    levenshtein = stringdist(string1, string2, method = "lv"),
    damerau_levenshtein = stringdist(string1, string2, method = "dl"),
    hamming = ifelse(nchar(string1) == nchar(string2), 
                    stringdist(string1, string2, method = "hamming"), 
                    NA),
    jaccard_q2 = stringdist(string1, string2, method = "jaccard", q = 2),
    jaccard_q3 = stringdist(string1, string2, method = "jaccard", q = 3),
    cosine_q2 = stringdist(string1, string2, method = "cosine", q = 2),
    cosine_q3 = stringdist(string1, string2, method = "cosine", q = 3),
    qgram_q2 = stringdist(string1, string2, method = "qgram", q = 2),
    qgram_q3 = stringdist(string1, string2, method = "qgram", q = 3),
    jaro = stringdist(string1, string2, method = "jw", p = 0),
    jaro_winkler = stringdist(string1, string2, method = "jw", p = 0.1),
    lcs = stringdist(string1, string2, method = "lcs"),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Тестовые пары строк
test_pairs <- list(
  c("kitten", "sitting"),
  c("hello", "hell"),
  c("abc", "acb"),
  c("book", "back"),
  c("data", "date"),
  c("martha", "marhta"),
  c("example", "samples"),
  c("programming", "program"),
  c("identical", "identical"),
  c("completely", "different")
)

# Создаем имена для пар
pair_names <- sapply(test_pairs, function(x) paste0('"', x[1], '" - "', x[2], '"'))

# Сравниваем все пары
all_results <- data.frame()
for (i in 1:length(test_pairs)) {
  result <- compare_metrics(test_pairs[[i]][1], test_pairs[[i]][2], pair_names[i])
  all_results <- rbind(all_results, result)
}

# Выводим результаты в виде таблицы
cat("Сравнение метрик расстояния между строками\n")
cat("==========================================\n\n")

kable(all_results, digits = 3, caption = "Результаты сравнения метрик расстояния")

correlation_matrix <- all_results %>%
  select(-pair) %>%
  cor(use = "complete.obs")

kable(correlation_matrix, digits=3, caption="Матрица корреляции между метриками")

# Детальный анализ для примера
cat("\n")
cat("==================================================\n")
cat("ДЕТАЛЬНЫЙ АНАЛИЗ ДЛЯ ПАРЫ 'kitten' - 'sitting'\n")
cat("==================================================\n")

detailed_analysis <- function(string1, string2) {
  cat("Строка 1:", string1, "\n")
  cat("Строка 2:", string2, "\n")
  cat("Длины:", nchar(string1), "и", nchar(string2), "символов\n\n")
  
  results <- compare_metrics(string1, string2)
  
  cat("Результаты для всех метрик:\n")
  for (i in 2:ncol(results)) {
    metric_name <- colnames(results)[i]
    value <- results[1, i]
    cat(sprintf("  %-20s: %s\n", metric_name, value))
  }
}

detailed_analysis("kitten", "sitting")

# Сводка по метрикам
cat("\n")
cat("==================================================\n")
cat("СВОДНАЯ ИНФОРМАЦИЯ ПО МЕТРИКАМ\n")
cat("==================================================\n")

cat("Метрики редактирования:\n")
cat("  - Levenshtein: минимальное количество операций редактирования\n")
cat("  - Damerau-Levenshtein: включает транспозиции соседних символов\n")
cat("  - Hamming: только для строк одинаковой длины\n\n")

cat("N-граммные метрики:\n")
cat("  - Jaccard: на основе пересечения множеств n-грамм\n")
cat("  - Cosine: косинус угла между векторами n-грамм\n")
cat("  - Q-gram: разность множеств n-грамм\n\n")

cat("Специализированные метрики:\n")
cat("  - Jaro: учитывает близость matching символов\n")
cat("  - Jaro-Winkler: дополнительно учитывает общие префиксы\n")
cat("  - LCS: на основе самой длинной общей подстроки\n")

cat("\nАнализ завершен!\n")