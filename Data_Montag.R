# 首先, 从CSV文件中加载数据到data_haos变量中
data_haos <- read.csv("/Users/jackpeng/Desktop/Praktikum/data_GuK-App_Fragebogen_numerische_Codes.csv")

# 加载读取Excel文件所需的库
library("readxl")

# 从Excel文件中加载数据到my_data变量中
my_data <- read_excel("/Users/jackpeng/Desktop/Praktikum/data_GuK-App_Fragebogen_beschriftete_Antwortcodes.xlsx")

# 加载用于数据处理和绘图的库
library(dplyr)
library(ggplot2)
library(tidyr)

# 选择my_data中的DA03x01列，并赋值给filtered_data_DA03x01变量
filtered_data_DA03x01 <- select(my_data , DA03x01)

# 从filtered_data_DA03x01中移除第303行
filtered_data_DA03x01 <- filtered_data_DA03x01[-303,]

# 计算DA03x01列中NA值的数量
na_count_DA03x01 <- sum(is.na(filtered_data_DA03x01$DA03x01))
na_count_DA03x01

# 从my_data中选择1到302行的"QUESTNNR"列，并赋值给column_subset_my_data变量
column_subset_my_data <- my_data[1:302, "QUESTNNR"]

# 将column_subset_my_data合并到filtered_data_DA03x01中
filtered_data_DA03x01 <- cbind(filtered_data_DA03x01, column_subset_my_data)

# 如果DA03x01列中的值不是NA，将其替换为"Antwort"
filtered_data_DA03x01$DA03x01 <- ifelse(!is.na(filtered_data_DA03x01$DA03x01), "Antwort", filtered_data_DA03x01$DA03x01)

# 根据QUESTNNR和DA03x01对数据进行分组，并计算每组的数量
data_counts_DA03x01 <- filtered_data_DA03x01 %>%
  group_by(QUESTNNR, DA03x01) %>%
  summarise(count = n())

# 将DA03x01列的NA值转换为字符串"NA"
data_counts_DA03x01$DA03x01 <- as.character(data_counts_DA03x01$DA03x01)
data_counts_DA03x01$DA03x01[is.na(data_counts_DA03x01$DA03x01)] <- "NA"

# 定义一个颜色调色板
color_palette <- scales::hue_pal()(2)

# 使用ggplot2库创建柱状图，显示不同组的NA和Antwort计数
ggplot(data_counts_DA03x01, aes(x = QUESTNNR, y = count, fill = DA03x01)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    plot.title = element_text(size = 16, hjust = 0.5),
  ) +
  labs(
    x = "Group",
    y = "Count",
    fill = "Status",
    title = "Comparison of NA and Antwort Counts by Group"
  )

# 下面的代码块是针对不同问题（DA08_02,03,04、DA08_05a、EA01、EA02）的相似处理，包括数据筛选、长格式转换、数据汇总和绘图



# 处理DA08_02,03,04问题的数据

# 从my_data中选择DA08_02, DA08_03, DA08_04和QUESTNNR列
filtered_data_DA08_0234 <- select(my_data , DA08_02, DA08_03, DA08_04, QUESTNNR)

# 从filtered_data_DA08_0234中移除第303行
filtered_data_DA08_0234 <- filtered_data_DA08_0234[-303,]

# 将数据重塑为长格式，以便绘图
data_long <- filtered_data_DA08_0234 %>%
  pivot_longer(cols = starts_with("DA08"), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) # 过滤掉NA值

# 检查数据的结构以确保它符合预期的格式
str(data_long)

# 定义与之前图形相同的颜色调色板
color_palette <- scales::hue_pal()(2)

# 现在，创建图形
# 使用facet_wrap为QUESTNNR级别创建单独的图表
ggplot(data_long, aes(x = variable, fill = value)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~QUESTNNR) +
  labs(
    x = "Category",
    y = "Count",
    fill = "Response",
    title = "Response Distribution for DA08 Questions",
    subtitle = "Separated by QUESTNNR Groups",
    caption = "Data source: Your Data Source Here"
  ) +
  scale_fill_manual(values = color_palette) + # 使用相同的颜色调色板
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 处理DA08_05问题的数据

# 从my_data中选择DA08_05a和QUESTNNR列
filtered_data_DA08_05a <- select(my_data, DA08_05a, QUESTNNR)

# 从filtered_data_DA08_05a中移除第303行
filtered_data_DA08_05a <- filtered_data_DA08_05a[-303,]

# 使用scales::hue_pal()生成颜色调色板
color_palette <- scales::hue_pal()(2)

# 提取"Antwort"和"NA"的特定颜色
antwort_color <- color_palette[1] # 获取"Antwort"的颜色
na_color <- color_palette[2]       # 获取"NA"的颜色

# 使用提取的颜色创建图形
ggplot(data_counts_DA08_05a, aes(x = QUESTNNR, y = count, fill = DA08_05a)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Antwort" = antwort_color, "NA" = na_color)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  labs(
    x = "Group",
    y = "Count",
    fill = "Response",
    title = "Comparison of NA and Antwort Counts by Group (DA08_05a)"
  )

# 处理问题EA01的数据

# 从my_data中选择EA01和QUESTNNR列
filtered_data_EA01 <- select(my_data, EA01, QUESTNNR)

# 从filtered_data_EA01中移除第303行
filtered_data_EA01 <- filtered_data_EA01[-303,]

# 根据'QUESTNNR'和'EA01'对数据进行分组，然后计算每种组合的计数
data_counts_EA01 <- filtered_data_EA01 %>%
  group_by(QUESTNNR, EA01) %>%
  summarise(count = n())

# 定义一个包含两种颜色的颜色调色板
color_palette <- scales::hue_pal()(2)

# 创建一个条形图以显示data_counts_EA01
ggplot(data_counts_EA01, aes(x = QUESTNNR, y = count, fill = EA01)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    plot.title = element_text(size = 16, hjust = 0.5),
  ) +
  labs(
    x = "Group",
    y = "Count",
    fill = "Status",
    title = "Comparison of NA and Antwort Counts by Group (EA01)"
  )

# 处理问题EA02的数据

# 从my_data中选择相关列
filtered_data_EA02 <- select(my_data , EA02_01, EA02_02, EA02_03, EA02_04, EA02_05, EA02_06, QUESTNNR)

# 从filtered_data_EA02中移除第303行
filtered_data_EA02 <- filtered_data_EA02[-303,]

# 将数据重塑为长格式，以便绘图
data_long_EA02 <- filtered_data_EA02 %>%
  pivot_longer(cols = starts_with("EA02"), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) # 如果需要，过滤掉NA值

# 根据'QUESTNNR', 'variable', 和 'value'对数据进行分组，然后计算每种组合的计数
data_counts_EA02 <- data_long_EA02 %>%
  group_by(QUESTNNR, variable, value) %>%
  summarise(count = n(), .groups = 'drop')

# 定义一个包含两种颜色的颜色调色板
color_palette <- scales::hue_pal()(2)

# 创建一个向量，用于重新映射x轴上的标签
new_labels <- c("Kestner-Gebärden-App", "Spreadthesign", "Gebärdensammlung GuK", "Eis-App", "Münsteraner Gebärden-App", "Andere")

# 过滤掉"value"为 "nicht gewählt" 的值
data_counts_EA02_filtered <- data_counts_EA02 %>%
  filter(value != "nicht gewählt")

# 使用相同的代码创建图表，但使用过滤后的数据
ggplot(data_counts_EA02_filtered, aes(x = factor(variable, levels = c("EA02_01", "EA02_02", "EA02_03", "EA02_04", "EA02_05", "EA02_06"), labels = new_labels), y = count, fill = value)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = color_palette) +
  facet_wrap(~QUESTNNR) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    plot.title = element_text(size = 16, hjust = 0.5),
  ) +
  labs(
    x = "Question",
    y = "Count",
    fill = "Response",
    title = "Response Distribution for EA02 Questions",
    subtitle = "Separated by QUESTNNR Groups",
    caption = "Data source: Your Data Source Here"
  )
















