# ==============================================================================
# ПРАКТИЧЕСКОЕ ЗАНЯТИЕ: ОПИСАТЕЛЬНАЯ СТАТИСТИКА И ВИЗУАЛИЗАЦИЯ
# ==============================================================================

# 1. ЗАГРУЗКА ПАКЕТОВ
options(scipen = 100, digits = 4)

library(dplyr)
library(ggplot2)
library(psych)
library(e1071)
library(gridExtra)
library(tidyr)

# ==============================================================================
# БЛОК 0: ЗАГРУЗКА И ПАРСИНГ ДАННЫХ
# ==============================================================================

file_path <- "Variant_177.csv"

# Читаем файл как текст
raw_lines <- readLines(file_path, encoding = "UTF-8")

cat("=== ДИАГНОСТИКА ===\n")
cat("Количество строк в файле:", length(raw_lines), "\n")
cat("Первая строка:\n")
print(raw_lines[1])
cat("\nВторая строка:\n")
print(raw_lines[2])

# Функция для парсинга строки
parse_line <- function(line) {
  # Убираем кавычки в начале и конце
  line <- trimws(line)
  
  # Разбиваем по запятой, но учитываем кавычки
  # Используем простой подход: убираем кавычки и разбиваем
  line <- gsub('"', '', line)
  parts <- strsplit(line, ",")[[1]]
  
  # Если частей меньше 6, возвращаем NA
  if(length(parts) < 6) {
    return(rep(NA, 6))
  }
  
  return(parts)
}

# Парсим заголовок
header_line <- raw_lines[1]
header_parts <- parse_line(header_line)
cat("\nЗаголовок:", paste(header_parts, collapse = " | "), "\n")

# Парсим данные
data_list <- list()
for(i in 2:length(raw_lines)) {
  parsed <- parse_line(raw_lines[i])
  if(length(parsed) >= 6 && !all(is.na(parsed))) {
    data_list[[length(data_list) + 1]] <- parsed[1:6]
  }
}

# Создаем data frame
cows_data <- as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)
names(cows_data) <- c("ID", "Breed", "Diet", "Lactation", "Milk_Yield", "Weight_Gain")

cat("\n=== ПРОВЕРКА ПАРСИНГА ===\n")
cat("Размер данных:", nrow(cows_data), "строк,", ncol(cows_data), "столбцов\n")
cat("\nПервые 5 строк:\n")
print(head(cows_data, 5))

# Преобразуем типы данных
cows_data$Lactation <- as.numeric(cows_data$Lactation)
cows_data$Milk_Yield <- as.numeric(cows_data$Milk_Yield)
cows_data$Weight_Gain <- as.numeric(cows_data$Weight_Gain)

# Удаляем строки с NA
cows_data <- cows_data[!is.na(cows_data$Milk_Yield) & !is.na(cows_data$Weight_Gain), ]

# Чистим текст
cows_data$Breed <- trimws(cows_data$Breed)
cows_data$Diet <- trimws(cows_data$Diet)

# Исправляем породы (убираем возможные кракозябры)
# Создаем правильные названия по ключевым словам
cows_data$Breed <- case_when(
  grepl("Голштин|Голш", cows_data$Breed) ~ "Голштинская",
  grepl("Джерсей|Джер", cows_data$Breed) ~ "Джерсейская",
  grepl("Симменталь|Симм", cows_data$Breed) ~ "Симментальская",
  TRUE ~ cows_data$Breed
)

cows_data$Diet <- case_when(
  grepl("Стандарт|Станд", cows_data$Diet) ~ "Стандарт",
  grepl("Обогащен|Обог", cows_data$Diet) ~ "Обогащенный",
  TRUE ~ cows_data$Diet
)

# Проверяем результат
cat("\n=== ПРОВЕРКА ДАННЫХ ===\n")
cat("Размер данных:", nrow(cows_data), "строк\n")
cat("\nПервые 5 строк:\n")
print(head(cows_data, 5))

cat("\nУникальные породы:\n")
print(unique(cows_data$Breed))

cat("\nУникальные рационы:\n")
print(unique(cows_data$Diet))

cat("\nСтатистика удоя:\n")
summary(cows_data$Milk_Yield)

cat("\nСтатистика привеса:\n")
summary(cows_data$Weight_Gain)

# ==============================================================================
# БЛОК 1: ФОРМА РАСПРЕДЕЛЕНИЯ
# ==============================================================================
cat("\n========== БЛОК 1: Оценка распределения удоев ==========\n")
cat("Асимметрия (Skewness):", e1071::skewness(cows_data$Milk_Yield), "\n")
cat("Эксцесс (Kurtosis):", e1071::kurtosis(cows_data$Milk_Yield), "\n")

# ГРАФИК 1: Гистограмма
p1 <- ggplot(cows_data, aes(x = Milk_Yield)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "#3498db", color = "white", alpha = 0.7) +
  geom_density(color = "#e74c3c", linewidth = 1.2) +
  geom_vline(aes(xintercept = mean(Milk_Yield)), color = "black", linetype = "dashed", linewidth = 1) +
  theme_minimal() +
  labs(title = "ГРАФИК 1: Распределение суточного удоя",
       subtitle = paste0("Среднее = ", round(mean(cows_data$Milk_Yield), 1), " кг"),
       x = "Удой (кг)", y = "Плотность")
print(p1)

# ГРАФИК 2: Q-Q Plot
p2 <- ggplot(cows_data, aes(sample = Milk_Yield)) +
  stat_qq(color = "#2c3e50", alpha = 0.6, size = 2) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  theme_minimal() +
  labs(title = "ГРАФИК 2: Q-Q График",
       subtitle = "Если точки на линии — распределение нормальное",
       x = "Теоретические квантили", y = "Квантили выборки")
print(p2)

# ==============================================================================
# БЛОК 2: ВЫЯВЛЕНИЕ ВЫБРОСОВ
# ==============================================================================
cat("\n========== БЛОК 2: Поиск выбросов ==========\n")

Q1 <- quantile(cows_data$Weight_Gain, 0.25)
Q3 <- quantile(cows_data$Weight_Gain, 0.75)
IQR_val <- IQR(cows_data$Weight_Gain)
lower_fence <- Q1 - 1.5 * IQR_val
upper_fence <- Q3 + 1.5 * IQR_val

cat("Нижняя граница:", lower_fence, "\n")
cat("Верхняя граница:", upper_fence, "\n")

cows_data <- cows_data %>%
  mutate(Is_Outlier = ifelse(Weight_Gain < lower_fence | Weight_Gain > upper_fence, "Выброс", "Норма"))

outliers <- cows_data %>% filter(Is_Outlier == "Выброс")
cat("Найдено выбросов:", nrow(outliers), "\n")
if(nrow(outliers) > 0) {
  print(outliers[, c("ID", "Breed", "Weight_Gain")])
}

# ГРАФИК 3: Boxplot с выбросами
p3 <- ggplot(cows_data, aes(y = Weight_Gain, x = "")) +
  geom_boxplot(fill = "#ecf0f1", color = "#2c3e50", outlier.shape = NA) + 
  geom_jitter(aes(color = Is_Outlier), width = 0.1, size = 2, alpha = 0.7) +
  scale_color_manual(values = c("Норма" = "#7f8c8d", "Выброс" = "#e74c3c")) +
  theme_bw() +
  labs(title = "ГРАФИК 3: Ящичная диаграмма привесов",
       subtitle = "Красные точки - потенциальные выбросы",
       x = "", y = "Привес (г)", color = "Статус")
print(p3)

# Очищенный датасет
cows_clean <- cows_data %>% filter(Is_Outlier == "Норма")
cat("\nРазмер очищенной выборки:", nrow(cows_clean), "из", nrow(cows_data), "\n")

# ==============================================================================
# БЛОК 3: РОБАСТНАЯ СТАТИСТИКА
# ==============================================================================
cat("\n========== БЛОК 3: Влияние выбросов на среднее ==========\n")
cat("Обычное среднее (с выбросами):", mean(cows_data$Weight_Gain), "\n")
cat("Усеченное среднее (Trimmed 5%):", mean(cows_data$Weight_Gain, trim = 0.05), "\n")
cat("Медиана (игнорирует выбросы):", median(cows_data$Weight_Gain), "\n")
# Винзоризация (замена экстремумов на границы 5% и 95%)
winsorized_gain <- psych::winsor(cows_data$Weight_Gain, trim = 0.05)
cat("Винзоризованное среднее:", mean(winsorized_gain), "\n")
# ==============================================================================
# БЛОК 4: СРАВНЕНИЕ ГРУПП
# ==============================================================================

# ГРАФИК 4: Скрипичная диаграмма по породам
p4 <- ggplot(cows_clean, aes(x = Breed, y = Milk_Yield, fill = Breed)) +
  geom_violin(alpha = 0.5, trim = FALSE, color = NA) +
  geom_boxplot(width = 0.2, fill = "white", color = "black", outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "ГРАФИК 4: Удой по породам",
       subtitle = "Ширина показывает плотность данных",
       x = "Порода", y = "Удой (кг)") +
  theme(legend.position = "none")
print(p4)

# ГРАФИК 5: Распределение по рационам
p5 <- ggplot(cows_clean, aes(x = Breed, fill = Diet)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.8) +
  geom_text(stat = 'count', aes(label = after_stat(count)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("Стандарт" = "#95a5a6", "Обогащенный" = "#2ecc71")) +
  theme_classic() +
  labs(title = "ГРАФИК 5: Распределение рационов по породам",
       x = "Порода", y = "Количество голов", fill = "Рацион")
print(p5)

# ГРАФИК 5b: Влияние лактации
p5b <- ggplot(cows_clean, aes(x = factor(Lactation), y = Milk_Yield, fill = Breed)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "ГРАФИК 5b: Влияние лактации на удой",
       x = "Номер лактации", y = "Удой (кг)", fill = "Порода")
print(p5b)

# ==============================================================================
# БЛОК 5: СТАНДАРТНАЯ ОШИБКА
# ==============================================================================

summary_se <- cows_clean %>%
  group_by(Diet) %>%
  summarise(
    n = n(),
    Mean_Yield = mean(Milk_Yield),
    SE = sd(Milk_Yield) / sqrt(n)
  )

p6 <- ggplot(summary_se, aes(x = Diet, y = Mean_Yield, fill = Diet)) +
  geom_col(width = 0.6, alpha = 0.8, color = "black") +
  geom_errorbar(aes(ymin = Mean_Yield - SE, ymax = Mean_Yield + SE), width = 0.2, linewidth = 1) +
  scale_fill_manual(values = c("Стандарт" = "#bdc3c7", "Обогащенный" = "#f39c12")) +
  theme_minimal() +
  labs(title = "ГРАФИК 6: Средний удой по рационам",
       subtitle = "Усы = стандартная ошибка среднего",
       x = "Тип рациона", y = "Средний удой (кг)") +
  theme(legend.position = "none")
print(p6)

# ==============================================================================
# БЛОК 6: КОРРЕЛЯЦИЯ
# ==============================================================================

cor_result <- cor.test(cows_clean$Milk_Yield, cows_clean$Weight_Gain)
cat("\n========== БЛОК 6: Корреляционный анализ ==========\n")
cat("Корреляция удой-привес: r =", round(cor_result$estimate, 3), 
    ", p-value =", format.pval(cor_result$p.value), "\n")

cor_lact <- cor.test(cows_clean$Lactation, cows_clean$Milk_Yield)
cat("Корреляция лактация-удой: r =", round(cor_lact$estimate, 3), 
    ", p-value =", format.pval(cor_lact$p.value), "\n")

# ГРАФИК 7: Диаграмма рассеяния
p7 <- ggplot(cows_clean, aes(x = Milk_Yield, y = Weight_Gain)) +
  geom_point(aes(color = Breed), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", fill = "gray50", alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(title = "ГРАФИК 7: Связь удоя и привеса",
       subtitle = paste0("Корреляция: r = ", round(cor_result$estimate, 3)),
       x = "Удой (кг)", y = "Привес (г)", color = "Порода")
print(p7)

# ГРАФИК 7b: Лактация vs удой
p7b <- ggplot(cows_clean, aes(x = Lactation, y = Milk_Yield, color = Breed)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  theme_bw() +
  labs(title = "ГРАФИК 7b: Влияние лактации на удой",
       x = "Номер лактации", y = "Удой (кг)", color = "Порода")
print(p7b)

# ==============================================================================
# БЛОК 7: ИТОГОВАЯ ТАБЛИЦА
# ==============================================================================
cat("\n========== БЛОК 7: Итоговая статистика ==========\n")

final_table <- cows_clean %>%
  group_by(Breed, Diet) %>%
  summarise(
    n = n(),
    M_Yield = round(mean(Milk_Yield), 1),
    SD_Yield = round(sd(Milk_Yield), 1),
    Min_Yield = round(min(Milk_Yield), 1),
    Max_Yield = round(max(Milk_Yield), 1),
    Mdn_Gain = round(median(Weight_Gain), 0),
    .groups = "drop"
  )

print(final_table)

cat("\n=== АНАЛИЗ ЗАВЕРШЕН ===\n")
cat("Создано графиков: 8\n")
cat("Очищенная выборка:", nrow(cows_clean), "наблюдений\n")

# ==============================================================================
# БЛОК 7: ПРАКТИЧЕСКАЯ ЗНАЧИМОСТЬ (РАЗМЕР ЭФФЕКТА) И КРАСИВАЯ ТАБЛИЦА
# ==============================================================================

cat("\n========== БЛОК 7: Практическая значимость (Размер эффекта) ==========\n")

# Проверяем, установлен ли пакет effectsize
if(!require(effectsize)) {
  cat("Устанавливаем пакет effectsize...\n")
  install.packages("effectsize")
  library(effectsize)
}

# Насколько сильно рацион увеличивает удой? (d Коэна)
cat("\n--- Cohen's d для влияния рациона на удой ---\n")
effect_d <- effectsize::cohens_d(Milk_Yield ~ Diet, data = cows_clean)
print(effect_d)
cat("\nИнтерпретация Cohen's d:\n")
cat("  d < 0.2 - очень слабый эффект\n")
cat("  d = 0.2-0.5 - слабый эффект\n")
cat("  d = 0.5-0.8 - средний эффект\n")
cat("  d > 0.8 - большой эффект\n")

# Дополнительно: размер эффекта для породы (Eta-squared)
cat("\n--- Eta-squared для влияния породы на удой ---\n")
effect_eta <- effectsize::eta_squared(aov(Milk_Yield ~ Breed, data = cows_clean))
print(effect_eta)
cat("\nИнтерпретация Eta-squared:\n")
cat("  η² < 0.01 - очень слабый эффект\n")
cat("  η² = 0.01-0.06 - слабый эффект\n")
cat("  η² = 0.06-0.14 - средний эффект\n")
cat("  η² > 0.14 - большой эффект\n")

# ==============================================================================
# БЛОК 8: КРАСИВАЯ ТАБЛИЦА ДЛЯ ДИССЕРТАЦИИ (С ИСПОЛЬЗОВАНИЕМ FLEXTABLE)
# ==============================================================================

cat("\n========== БЛОК 8: Создание публикационной таблицы ==========\n")

# Проверяем, установлен ли пакет flextable
if(!require(flextable)) {
  cat("Устанавливаем пакет flextable...\n")
  install.packages("flextable")
  library(flextable)
}

# Создаем итоговую сводную таблицу
final_table <- cows_clean %>%
  dplyr::group_by(Breed, Diet) %>%
  dplyr::summarise(
    n = dplyr::n(),
    M_Yield = round(mean(Milk_Yield), 1),
    SD_Yield = round(sd(Milk_Yield), 1),
    Min_Yield = round(min(Milk_Yield), 1),
    Max_Yield = round(max(Milk_Yield), 1),
    Mdn_Gain = round(median(Weight_Gain), 0),
    Q1_Gain = round(quantile(Weight_Gain, 0.25), 0),
    Q3_Gain = round(quantile(Weight_Gain, 0.75), 0),
    .groups = "drop"
  )

# Выводим обычную таблицу в консоль
cat("\n=== ИТОГОВАЯ ТАБЛИЦА (КОНСОЛЬ) ===\n")
print(final_table)

# Создаем красивую таблицу для отчета
cat("\n=== СОЗДАНИЕ КРАСИВОЙ ТАБЛИЦЫ ===\n")
cat("Таблица отобразится в окне Viewer (справа внизу в RStudio)\n")

# Создаем flextable с дополнительной информацией
ft <- final_table %>%
  flextable::flextable() %>%
  flextable::set_header_labels(
    Breed = "Порода", 
    Diet = "Рацион", 
    n = "Голов (n)", 
    M_Yield = "Удой (M, кг)", 
    SD_Yield = "Удой (SD, кг)",
    Min_Yield = "Мин. удой (кг)",
    Max_Yield = "Макс. удой (кг)",
    Mdn_Gain = "Привес (Me, г)",
    Q1_Gain = "Q1 привеса (г)",
    Q3_Gain = "Q3 привеса (г)"
  ) %>%
  flextable::theme_zebra() %>%
  flextable::autofit() %>%
  flextable::align_text_col(align = "center") %>%
  flextable::align(align = "center", part = "header") %>%
  flextable::bold(part = "header") %>%
  flextable::fontsize(size = 10, part = "all") %>%
  flextable::set_caption("Таблица 1. Описательная статистика продуктивности коров по породам и типам рациона")

# Отображаем таблицу
ft

# Дополнительно: сохраняем таблицу в Word (если нужно)
cat("\n=== СОХРАНЕНИЕ ТАБЛИЦЫ ===\n")
cat("Чтобы сохранить таблицу в Word, раскомментируйте строку:\n")
cat("# save_as_docx(ft, path = 'C:/Users/ADMIN/Desktop/R/Таблица_статистика.docx')\n")

# Сохраняем таблицу в CSV (простой формат)
write.csv(final_table, "итоговая_статистика.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")
cat("✓ Таблица сохранена в CSV: C:/Users/ADMIN/Desktop/R/Итоговая_статистика.csv\n")

# Сохраняем размеры эффекта в файл
sink("размеры_эффекта.txt")
cat("=== АНАЛИЗ РАЗМЕРА ЭФФЕКТА ===\n\n")
cat("Cohen's d (влияние рациона на удой):\n")
print(effect_d)
cat("\nИнтерпретация: d > 0.8 - большой эффект\n\n")
cat("Eta-squared (влияние породы на удой):\n")
print(effect_eta)
cat("\nИнтерпретация: η² > 0.14 - большой эффект\n")
sink("размеры_эффекта.txt")

cat("✓ Размеры эффекта сохранены в: C:/Users/ADMIN/Desktop/R/Размеры_эффекта.txt\n")

# ==============================================================================
# ФИНАЛЬНОЕ СООБЩЕНИЕ
# ==============================================================================

cat("\n" + paste(rep("=", 60), collapse = ""))
cat("\n                АНАЛИЗ УСПЕШНО ЗАВЕРШЕН\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n\nРЕЗУЛЬТАТЫ:\n")
cat("  • Очищенная выборка:", nrow(cows_clean), "наблюдений\n")
cat("  • Количество выбросов:", nrow(cows_data) - nrow(cows_clean), "\n")
cat("  • Создано графиков: 8\n")
cat("  • Создана публикационная таблица\n")
cat("  • Рассчитаны размеры эффекта (Cohen's d, Eta-squared)\n")
cat("\nСОХРАНЕННЫЕ ФАЙЛЫ:\n")
cat("  • Итоговая_статистика.csv\n")
cat("  • Размеры_эффекта.txt\n")
cat("\nДля сохранения графиков раскомментируйте строки с ggsave()\n")
cat("Для сохранения таблицы в Word раскомментируйте save_as_docx()\n")
