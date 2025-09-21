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
  install_and_load("dplyr")
}, error = function(e) {
  cat("Ошибка при установке пакетов:", e$message, "\n")
})

# Функция для сравнения метрик
compare_metrics <- function(string1, string2, test_type, description) {
  results <- data.frame(
    test_type = test_type,
    description = description,
    strings = paste0('"', string1, '" - "', string2, '"'),
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

# Создаем тестовые случаи для каждого типа различий
sensitivity_results <- data.frame()

## 1. Анализ чувствительности к ОПЕЧАТКАМ
cat("Анализ чувствительности к ОПЕЧАТКАМ\n")
cat("===================================\n\n")

typo_tests <- list(
  c("hello", "hallo", "Замена одной буквы"),
  c("programming", "programing", "Пропуск буквы"),
  c("computer", "compuuter", "Дублирование буквы"),
  c("language", "lanquage", "Замена похожих букв")
)

for (test in typo_tests) {
  result <- compare_metrics(test[1], test[2], "Опечатки", test[3])
  sensitivity_results <- rbind(sensitivity_results, result)
}

## 2. Анализ чувствительности к РАЗНОЙ ДЛИНЕ СТРОК
cat("Анализ чувствительности к РАЗНОЙ ДЛИНЕ СТРОК\n")
cat("============================================\n\n")

length_tests <- list(
  c("hello", "hell", "Укорачивание на 1 символ"),
  c("program", "programming", "Удлинение на 4 символа"),
  c("data", "database", "Удлинение на 4 символа"),
  c("text", "txt", "Укорачивание на 1 символ")
)

for (test in length_tests) {
  result <- compare_metrics(test[1], test[2], "Разная длина", test[3])
  sensitivity_results <- rbind(sensitivity_results, result)
}

## 3. Анализ чувствительности к ПЕРЕСТАНОВКАМ СИМВОЛОВ
cat("Анализ чувствительности к ПЕРЕСТАНОВКАМ СИМВОЛОВ\n")
cat("================================================\n\n")

permutation_tests <- list(
  c("abc", "acb", "Перестановка соседних символов"),
  c("martha", "marhta", "Перестановка несоседних символов"),
  c("form", "from", "Перестановка в коротком слове"),
  c("example", "exapmle", "Перестановка в середине слова")
)

for (test in permutation_tests) {
  result <- compare_metrics(test[1], test[2], "Перестановки", test[3])
  sensitivity_results <- rbind(sensitivity_results, result)
}

## 4. Анализ чувствительности к РАЗНОМУ РЕГИСТРУ
cat("Анализ чувствительности к РАЗНОМУ РЕГИСТРУ\n")
cat("==========================================\n\n")

case_tests <- list(
  c("Hello", "hello", "Первая буква заглавная"),
  c("PROGRAMMING", "programming", "Все буквы заглавные"),
  c("Data Science", "data science", "Разный регистр в словах"),
  c("Python", "PYTHON", "Полное изменение регистра")
)

for (test in case_tests) {
  result <- compare_metrics(test[1], test[2], "Разный регистр", test[3])
  sensitivity_results <- rbind(sensitivity_results, result)
}

# Вывод результатов в виде красивых таблиц
cat("РЕЗУЛЬТАТЫ АНАЛИЗА ЧУВСТВИТЕЛЬНОСТИ МЕТРИК\n")
cat("==========================================\n\n")

# Функция для красивого вывода таблицы
print_sensitivity_table <- function(data, title) {
  cat("\n", title, "\n")
  cat(paste0(rep("=", nchar(title)), collapse = ""), "\n\n")
  
  for (i in 1:nrow(data)) {
    cat(sprintf("%-20s: %s\n", "Тип теста", data$test_type[i]))
    cat(sprintf("%-20s: %s\n", "Описание", data$description[i]))
    cat(sprintf("%-20s: %s\n", "Строки", data$strings[i]))
    cat(sprintf("%-20s: %s\n", "Levenshtein", data$levenshtein[i]))
    cat(sprintf("%-20s: %s\n", "Damerau-Lev", data$damerau_levenshtein[i]))
    cat(sprintf("%-20s: %s\n", "Hamming", ifelse(is.na(data$hamming[i]), "N/A", data$hamming[i])))
    cat(sprintf("%-20s: %s\n", "Jaccard (q=2)", round(data$jaccard_q2[i], 3)))
    cat(sprintf("%-20s: %s\n", "Jaccard (q=3)", round(data$jaccard_q3[i], 3)))
    cat(sprintf("%-20s: %s\n", "Cosine (q=2)", round(data$cosine_q2[i], 3)))
    cat(sprintf("%-20s: %s\n", "Cosine (q=3)", round(data$cosine_q3[i], 3)))
    cat(sprintf("%-20s: %s\n", "Q-gram (q=2)", data$qgram_q2[i]))
    cat(sprintf("%-20s: %s\n", "Q-gram (q=3)", data$qgram_q3[i]))
    cat(sprintf("%-20s: %s\n", "Jaro", round(data$jaro[i], 3)))
    cat(sprintf("%-20s: %s\n", "Jaro-Winkler", round(data$jaro_winkler[i], 3)))
    cat(sprintf("%-20s: %s\n", "LCS", data$lcs[i]))
    cat("\n", paste0(rep("-", 50), collapse = ""), "\n\n")
  }
}

# Вывод результатов по типам тестов
print_sensitivity_table(sensitivity_results[sensitivity_results$test_type == "Опечатки", ], 
                       "ЧУВСТВИТЕЛЬНОСТЬ К ОПЕЧАТКАМ")

print_sensitivity_table(sensitivity_results[sensitivity_results$test_type == "Разная длина", ], 
                       "ЧУВСТВИТЕЛЬНОСТЬ К РАЗНОЙ ДЛИНЕ СТРОК")

print_sensitivity_table(sensitivity_results[sensitivity_results$test_type == "Перестановки", ], 
                       "ЧУВСТВИТЕЛЬНОСТЬ К ПЕРЕСТАНОВКАМ СИМВОЛОВ")

print_sensitivity_table(sensitivity_results[sensitivity_results$test_type == "Разный регистр", ], 
                       "ЧУВСТВИТЕЛЬНОСТЬ К РАЗНОМУ РЕГИСТРУ")

# Сводная таблица со средними значениями по типам тестов
cat("СВОДНАЯ СТАТИСТИКА ПО ТИПАМ ТЕСТОВ\n")
cat("==================================\n\n")

summary_stats <- sensitivity_results %>%
  group_by(test_type) %>%
  summarise(
    avg_levenshtein = mean(levenshtein, na.rm = TRUE),
    avg_damerau_lev = mean(damerau_levenshtein, na.rm = TRUE),
    avg_jaccard_q2 = mean(jaccard_q2, na.rm = TRUE),
    avg_jaccard_q3 = mean(jaccard_q3, na.rm = TRUE),
    avg_cosine_q2 = mean(cosine_q2, na.rm = TRUE),
    avg_cosine_q3 = mean(cosine_q3, na.rm = TRUE),
    avg_jaro = mean(jaro, na.rm = TRUE),
    avg_jaro_winkler = mean(jaro_winkler, na.rm = TRUE),
    avg_lcs = mean(lcs, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Средние значения метрик по типам тестов:\n\n")
for (i in 1:nrow(summary_stats)) {
  cat(sprintf("%-15s: \n", summary_stats$test_type[i]))
  cat(sprintf("  Levenshtein: %.2f, Damerau-Lev: %.2f\n", 
              summary_stats$avg_levenshtein[i], summary_stats$avg_damerau_lev[i]))
  cat(sprintf("  Jaccard (q=2): %.3f, Jaccard (q=3): %.3f\n", 
              summary_stats$avg_jaccard_q2[i], summary_stats$avg_jaccard_q3[i]))
  cat(sprintf("  Cosine (q=2): %.3f, Cosine (q=3): %.3f\n", 
              summary_stats$avg_cosine_q2[i], summary_stats$avg_cosine_q3[i]))
  cat(sprintf("  Jaro: %.3f, Jaro-Winkler: %.3f\n", 
              summary_stats$avg_jaro[i], summary_stats$avg_jaro_winkler[i]))
  cat(sprintf("  LCS: %.2f\n", summary_stats$avg_lcs[i]))
  cat("\n")
}

# Анализ наиболее чувствительных метрик
cat("ВЫВОДЫ ИЗ АНАЛИЗА ЧУВСТВИТЕЛЬНОСТИ\n")
cat("==================================\n\n")

cat("1. К ОПЕЧАТКАМ наиболее чувствительны:\n")
cat("   - Метрики редактирования (Levenshtein, Damerau-Levenshtein)\n")
cat("   - Hamming (для строк одинаковой длины)\n")
cat("   - Q-gram метрики\n\n")

cat("2. К РАЗНОЙ ДЛИНЕ СТРОК наиболее чувствительны:\n")
cat("   - Все метрики, кроме Hamming (который не работает для разной длины)\n")
cat("   - Метрики редактирования хорошо捕捉 insert/delete операции\n\n")

cat("3. К ПЕРЕСТАНОВКАМ СИМВОЛОВ наиболее чувствительны:\n")
cat("   - Damerau-Levenshtein (специально designed для транспозиций)\n")
cat("   - Jaro и Jaro-Winkler (учитывают близость символов)\n")
cat("   - N-граммные метрики (устойчивы к перестановкам)\n\n")

cat("4. К РАЗНОМУ РЕГИСТРУ наиболее чувствительны:\n")
cat("   - Все метрики чувствительны к регистру!\n")
cat("   - Для case-insensitive сравнения нужно предварительно привести к одному регистру\n\n")

cat("5. РЕКОМЕНДАЦИИ:\n")
cat("   - Для обработки опечаток: Damerau-Levenshtein или Jaro-Winkler\n")
cat("   - Для разной длины строк: Levenshtein или n-граммные метрики\n")
cat("   - Для перестановок: Damerau-Levenshtein\n")
cat("   - Всегда нормализуйте регистр перед сравнением!\n")

cat("\nАнализ завершен!\n")