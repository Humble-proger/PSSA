def damerau_levenshtein(s1, s2):
    len_s1 = len(s1)
    len_s2 = len(s2)
    
    # Создаем матрицу (len_s1 + 1) x (len_s2 + 1)
    d = [[0] * (len_s2 + 1) for _ in range(len_s1 + 1)]

    for i in range(len_s1 + 1):
        d[i][0] = i
    for j in range(len_s2 + 1):
        d[0][j] = j

    for i in range(1, len_s1 + 1):
        for j in range(1, len_s2 + 1):
            cost = 0 if s1[i - 1] == s2[j - 1] else 1
            d[i][j] = min(d[i - 1][j] + 1,      # удаление
                           d[i][j - 1] + 1,      # вставка
                           d[i - 1][j - 1] + cost)  # замена

            if i > 1 and j > 1 and s1[i - 1] == s2[j - 2] and s1[i - 2] == s2[j - 1]:
                d[i][j] = min(d[i][j], d[i - 2][j - 2] + cost)  # транспозиция

    return d[len_s1][len_s2]

# Пример использования
str1 = "ca"
str2 = "abc"
distance = damerau_levenshtein(str1, str2)
print("Расстояние Дамерау-Левенштейна:", distance)
