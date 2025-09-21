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
lengths <- 2^(3:10)  # 8, 16, 32, 64, 128, 256, 512, 1024 символов
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

# Анализ роста времени выполнения
cat("\nАНАЛИЗ РОСТА ВРЕМЕНИ ВЫПОЛНЕНИЯ\n")
cat("==============================\n\n")

# Вычисляем коэффициент роста для каждой метрики
growth_analysis <- data.frame()
for (metric in colnames(performance_results)[-1]) {
  if (!all(is.na(performance_results[[metric]]))) {
    # Берем последние 4 точки для анализа асимптотического поведения
    last_points <- tail(performance_results, 4)
    times <- last_points[[metric]]
    lengths <- last_points$length
    
    # Линейная регрессия в логарифмической шкале для определения сложности
    if (length(times[!is.na(times)]) >= 2) {
      log_lm <- lm(log(times + 1e-6) ~ log(lengths))
      complexity <- coef(log_lm)[2]  # Коэффициент наклона
    } else {
      complexity <- NA
    }
    
    growth_analysis <- rbind(growth_analysis, data.frame(
      metric = metric,
      time_8 = performance_results[[metric]][1],
      time_1024 = performance_results[[metric]][8],
      growth_factor = performance_results[[metric]][8] / performance_results[[metric]][1],
      estimated_complexity = round(complexity, 2),
      stringsAsFactors = FALSE
    ))
  }
}

cat("Анализ роста времени выполнения (8 vs 1024 символов):\n\n")
for (i in 1:nrow(growth_analysis)) {
  cat(sprintf("%-15s: %6.2f мс -> %6.2f мс (рост в %5.1f раз, сложность ~O(n^%.2f))\n",
              growth_analysis$metric[i],
              growth_analysis$time_8[i],
              growth_analysis$time_1024[i],
              growth_analysis$growth_factor[i],
              growth_analysis$estimated_complexity[i]))
}

# Группировка метрик по сложности
cat("\nГРУППИРОВКА МЕТРИК ПО ВЫЧИСЛИТЕЛЬНОЙ СЛОЖНОСТИ\n")
cat("=============================================\n\n")

# Метрики с линейной сложностью O(n)
linear_metrics <- growth_analysis[growth_analysis$estimated_complexity < 1.5, ]
cat("ЛИНЕЙНАЯ СЛОЖНОСТЬ O(n):\n")
for (i in 1:nrow(linear_metrics)) {
  cat(sprintf("  %-15s: O(n^%.2f)\n", linear_metrics$metric[i], linear_metrics$estimated_complexity[i]))
}

cat("\n")

# Метрики с квадратичной сложностью O(n²)
quadratic_metrics <- growth_analysis[growth_analysis$estimated_complexity >= 1.5 & 
                                     growth_analysis$estimated_complexity < 2.5, ]
cat("КВАДРАТИЧНАЯ СЛОЖНОСТЬ O(n²):\n")
for (i in 1:nrow(quadratic_metrics)) {
  cat(sprintf("  %-15s: O(n^%.2f)\n", quadratic_metrics$metric[i], quadratic_metrics$estimated_complexity[i]))
}

cat("\n")

# Метрики с другой сложностью
other_metrics <- growth_analysis[growth_analysis$estimated_complexity >= 2.5, ]
if (nrow(other_metrics) > 0) {
  cat("ВЫСОКАЯ СЛОЖНОСТЬ:\n")
  for (i in 1:nrow(other_metrics)) {
    cat(sprintf("  %-15s: O(n^%.2f)\n", other_metrics$metric[i], other_metrics$estimated_complexity[i]))
  }
  cat("\n")
}

# Практические рекомендации
cat("ПРАКТИЧЕСКИЕ РЕКОМЕНДАЦИИ ДЛЯ РАЗНЫХ ДЛИН СТРОК\n")
cat("===============================================\n\n")

cat("КОРОТКИЕ СТРОКИ (< 50 символов):\n")
cat("  - Можно использовать любые метрики\n")
cat("  - Damerau-Levenshtein для точного учета транспозиций\n")
cat("  - Levenshtein для баланса точности и скорости\n\n")

cat("СРЕДНИЕ СТРОКИ (50-200 символов):\n")
cat("  - N-граммные метрики (Jaccard, Cosine, Q-gram)\n")
cat("  - Jaro и Jaro-Winkler\n")
cat("  - Избегать Damerau-Levenshtein для частых вычислений\n\n")

cat("ДЛИННЫЕ СТРОКИ (200-1000 символов):\n")
cat("  - Только n-граммные метрики и Jaro\n")
cat("  - Q-gram с маленьким q (2-3)\n")
cat("  - Избегать метрик редактирования (Levenshtein, Damerau-Levenshtein)\n\n")

cat("ОЧЕНЬ ДЛИННЫЕ СТРОКИ (> 1000 символов):\n")
cat("  - Только Jaccard и Cosine с маленьким q\n")
cat("  - Рассмотреть предобработку и сегментацию текста\n")
cat("  - Метрики редактирования не практичны\n\n")

# Сводная таблица рекомендаций
cat("СВОДНАЯ ТАБЛИЦА РЕКОМЕНДАЦИЙ:\n\n")
cat("Метрика           | Короткие | Средние | Длинные | Очень длинные\n")
cat("------------------|----------|---------|---------|-------------\n")
cat("Levenshtein       |    ✓     |    △    |    ✗    |      ✗\n")
cat("Damerau-Lev       |    ✓     |    △    |    ✗    |      ✗\n")
cat("Jaccard (q=2)     |    ✓     |    ✓    |    ✓    |      ✓\n")
cat("Cosine (q=2)      |    ✓     |    ✓    |    ✓    |      ✓\n")
cat("Q-gram (q=2)      |    ✓     |    ✓    |    ✓    |      ✓\n")
cat("Jaro              |    ✓     |    ✓    |    ✓    |      △\n")
cat("Jaro-Winkler      |    ✓     |    ✓    |    ✓    |      △\n")
cat("LCS               |    ✓     |    △    |    ✗    |      ✗\n")
cat("\n✓ - рекомендуется, △ - с осторожностью, ✗ - не рекомендуется\n")

cat("\nАнализ завершен! Произведено тестирование для длин:", paste(lengths, collapse = ", "), "символов\n")