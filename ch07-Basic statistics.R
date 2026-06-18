# ================================================================
# 第七章 
# 数据集：mtcars, Arthritis, state.x77, UScrime, Salaries
# 说明：涵盖描述统计、列联表、相关分析、t检验、
#       非参数检验及多重比较（wmc 函数已修正）
# ================================================================

# -------------------- 0. 环境准备 ----------------------------
# install.packages(c("Hmisc", "pastecs", "psych", "dplyr", 
#                    "carData", "vcd", "gmodels", "ggm", "MASS"))

library(Hmisc)
library(pastecs)
library(psych)
library(dplyr)
library(carData)
library(vcd)
library(gmodels)
library(ggm)
library(MASS)

cat("==================== 环境加载完成 ====================\n\n")

# -------------------- 1. 自定义函数区 ------------------------
# 1.1 自定义描述统计（含偏度、峰度）
mystats <- function(x, na.omit = FALSE) {
  if (na.omit) x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x - m)^3 / s^3) / n
  kurt <- sum((x - m)^4 / s^4) / n - 3
  return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}

# 1.2 非参数多重比较函数（已修复 xtfrm 错误）
wmc <- function(formula, data, exact = FALSE, sort = TRUE, method = "holm") {
  df <- model.frame(formula, data)
  y <- df[[1]]
  x <- as.factor(df[[2]])
  
  # 按中位数排序（关键修正：使用 [[2]] 提取为向量）
  if (sort) {
    medians <- aggregate(y, by = list(x), FUN = median)[[2]]   # 修正点
    index <- order(medians)
    x <- factor(x, levels(x)[index])
  }
  
  groups <- levels(x)
  k <- length(groups)
  
  # 描述统计
  stats <- function(z) c(N = length(z), Median = median(z), MAD = mad(z))
  sumstats <- t(aggregate(y, by = list(x), FUN = stats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  cat("\n========== 描述统计 ==========\n")
  print(sumstats)
  
  # 准备结果数据框
  mc <- data.frame(
    Group.1 = character(0), Group.2 = character(0),
    W = numeric(0), p.unadj = numeric(0),
    p = numeric(0), stars = character(0),
    stringsAsFactors = FALSE
  )
  
  # 两两 Wilcoxon 检验
  row <- 0
  for (i in 1:k) {
    for (j in 1:k) {
      if (j > i) {
        row <- row + 1
        y1 <- y[x == groups[i]]
        y2 <- y[x == groups[j]]
        test <- wilcox.test(y1, y2, exact = exact)
        mc[row, 1] <- groups[i]
        mc[row, 2] <- groups[j]
        mc[row, 3] <- test$statistic
        mc[row, 4] <- test$p.value
      }
    }
  }
  
  # P 值校正
  mc$p <- p.adjust(mc$p.unadj, method = method)
  
  # 显著性星号
  mc$stars <- " "
  mc$stars[mc$p < 0.1] <- "."
  mc$stars[mc$p < 0.05] <- "*"
  mc$stars[mc$p < 0.01] <- "**"
  mc$stars[mc$p < 0.001] <- "***"
  names(mc)[6] <- " "
  
  cat("\n========== 多重比较 (Wilcoxon 秩和检验) ==========\n")
  cat(paste("P值校正方法:", method, "\n\n"))
  print(mc[-4], right = TRUE)  # 不显示未校正 p 值
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  return(invisible(NULL))
}

cat("========== 自定义函数加载完成 ==========\n\n")

# -------------------- 2. 描述性统计 ------------------------
cat("################ 第1部分：描述性统计 ################\n")

myvars <- c("mpg", "hp", "wt")

cat("\n--- 2.1 summary() 快速概览 ---\n")
print(summary(mtcars[myvars]))

cat("\n--- 2.2 sapply() 自定义统计（含偏度峰度）---\n")
print(sapply(mtcars[myvars], mystats))

cat("\n--- 2.3 Hmisc::describe() ---\n")
print(describe(mtcars[myvars]))

cat("\n--- 2.4 pastecs::stat.desc() ---\n")
print(stat.desc(mtcars[myvars]))

cat("\n--- 2.5 psych::describe() ---\n")
print(describe(mtcars[myvars]))

# -------------------- 3. 分组描述与 dplyr 聚合 --------------------
cat("\n################ 第2部分：分组统计 ################\n")

dstats <- function(x) sapply(x, mystats)
cat("\n--- 3.1 by() 按 am 分组 ---\n")
print(by(mtcars[myvars], mtcars$am, dstats))

cat("\n--- 3.2 dplyr 分组摘要（按 am 和 vs）---\n")
group_summary <- mtcars %>%
  group_by(am, vs) %>%
  summarise(
    n = n(),
    mpg_med = median(mpg),
    hp_med = median(hp),
    wt_med = median(wt),
    .groups = "drop"
  )
print(group_summary)

cat("\n--- 3.3 dplyr 聚合 Salaries 数据 ---\n")
Salaries %>%
  group_by(rank, sex) %>%
  summarise(
    n = n(),
    med_salary = median(salary),
    min_salary = min(salary),
    max_salary = max(salary),
    .groups = "drop"
  ) %>%
  print()

# -------------------- 4. 频数表与列联表 ------------------------
cat("\n################ 第3部分：列联表分析 ################\n")

mytable1 <- table(Arthritis$Improved)
cat("\n--- 4.1 一维频数表 ---\n")
print(mytable1)
cat("\n百分比:\n")
print(prop.table(mytable1) * 100)

mytable2 <- xtabs(~ Treatment + Improved, data = Arthritis)
cat("\n--- 4.2 二维列联表 ---\n")
print(mytable2)
cat("\n行比例（按 Treatment）:\n")
print(prop.table(mytable2, 1))
cat("\n列比例（按 Improved）:\n")
print(prop.table(mytable2, 2))
cat("\n添加边际和:\n")
print(addmargins(mytable2))

mytable3 <- xtabs(~ Treatment + Sex + Improved, data = Arthritis)
cat("\n--- 4.3 三维列联表 (ftable) ---\n")
print(ftable(mytable3))
cat("\n按 Treatment 和 Sex 分层的 Improved 比例:\n")
print(ftable(prop.table(mytable3, c(1, 2))))

cat("\n--- 4.4 CrossTable 详细输出 ---\n")
CrossTable(Arthritis$Treatment, Arthritis$Improved, 
           prop.chisq = FALSE, prop.t = FALSE, dnn = c("Treatment", "Improved"))

# -------------------- 5. 独立性检验与关联强度 --------------------
cat("\n################ 第4部分：假设检验（分类数据）################\n")

cat("\n--- 5.1 卡方检验 ---\n")
print(chisq.test(mytable2))

cat("\n--- 5.2 Fisher 精确检验 ---\n")
print(fisher.test(mytable2))

cat("\n--- 5.3 CMH 检验（控制 Sex）---\n")
print(mantelhaen.test(mytable3))

cat("\n--- 5.4 关联强度 (assocstats) ---\n")
print(assocstats(mytable2))

# -------------------- 6. 相关分析与偏相关 ------------------------
cat("\n################ 第5部分：连续变量关系 ################\n")

states <- state.x77[, 1:6]

cat("\n--- 6.1 协方差矩阵 ---\n")
print(cov(states))
cat("\n--- 6.2 Pearson 相关系数矩阵 ---\n")
print(cor(states))
cat("\n--- 6.3 Spearman 秩相关系数 ---\n")
print(cor(states, method = "spearman"))

x <- states[, c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[, c("Life Exp", "Murder")]
cat("\n--- 6.4 交叉相关 (x vs y) ---\n")
print(cor(x, y))

cat("\n--- 6.5 偏相关 (控制 Income, Illiteracy, HS Grad) ---\n")
print(pcor(c(1, 5, 2, 3, 6), cov(states)))

cat("\n--- 6.6 cor.test() 检验 Illiteracy 与 Murder ---\n")
print(cor.test(states[, "Illiteracy"], states[, "Murder"]))

cat("\n--- 6.7 corr.test() 批量检验 ---\n")
print(corr.test(states, use = "complete"))

# -------------------- 7. 均值比较：t检验与非参数替代 --------------------
cat("\n################ 第6部分：均值与位置比较 ################\n")

cat("\n--- 7.1 独立样本 t检验 (Welch) ---\n")
print(t.test(Prob ~ So, data = UScrime))

cat("\n--- 7.2 配对 t检验 (U1 vs U2) ---\n")
with(UScrime, print(t.test(U1, U2, paired = TRUE)))

cat("\n--- 7.3 Mann-Whitney U 检验 ---\n")
print(wilcox.test(Prob ~ So, data = UScrime))

cat("\n--- 7.4 Wilcoxon 配对符号秩检验 ---\n")
with(UScrime, print(wilcox.test(U1, U2, paired = TRUE)))

# -------------------- 8. 多组比较与事后检验 ------------------------
cat("\n################ 第7部分：多组非参数比较 ################\n")

states_df <- data.frame(state.region, state.x77)

cat("\n--- 8.1 Kruskal-Wallis 检验 ---\n")
print(kruskal.test(Illiteracy ~ state.region, data = states_df))

cat("\n--- 8.2 事后两两比较 (Wilcoxon + Holm 校正) ---\n")
wmc(Illiteracy ~ state.region, data = states_df, method = "holm")

# -------------------- 脚本结束 ----------------------------
cat("\n==================== 所有分析执行完毕！ ====================\n")

# ================================================================
# 第七章 练习题
# 涵盖：描述统计、分组聚合、列联表、独立性检验、相关分析、
#       t检验、非参数检验、多重比较
# 数据来源：mtcars, iris, Arthritis, state.x77, UScrime,
#           PlantGrowth, Salaries, CO2, chickwts
# ================================================================

# -------------------- 0. 环境准备 ----------------------------
# 安装所需包（如果尚未安装，请取消下面一行的注释并运行）
# install.packages(c("Hmisc", "pastecs", "psych", "dplyr", "carData", 
#                    "vcd", "gmodels", "ggm", "MASS"))

library(Hmisc)
library(pastecs)
library(psych)
library(dplyr)
library(carData)
library(vcd)
library(gmodels)
library(ggm)
library(MASS)

# 自定义 wmc 函数
wmc <- function(formula, data, exact = FALSE, sort = TRUE, method = "holm") {
  df <- model.frame(formula, data)
  y <- df[[1]]
  x <- as.factor(df[[2]])
  
  if (sort) {
    medians <- aggregate(y, by = list(x), FUN = median)[[2]]
    index <- order(medians)
    x <- factor(x, levels(x)[index])
  }
  
  groups <- levels(x)
  k <- length(groups)
  
  stats <- function(z) c(N = length(z), Median = median(z), MAD = mad(z))
  sumstats <- t(aggregate(y, by = list(x), FUN = stats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  cat("\n========== 描述统计 ==========\n")
  print(sumstats)
  
  mc <- data.frame(
    Group.1 = character(0), Group.2 = character(0),
    W = numeric(0), p.unadj = numeric(0),
    p = numeric(0), stars = character(0),
    stringsAsFactors = FALSE
  )
  
  row <- 0
  for (i in 1:k) {
    for (j in 1:k) {
      if (j > i) {
        row <- row + 1
        y1 <- y[x == groups[i]]
        y2 <- y[x == groups[j]]
        test <- wilcox.test(y1, y2, exact = exact)
        mc[row, 1] <- groups[i]
        mc[row, 2] <- groups[j]
        mc[row, 3] <- test$statistic
        mc[row, 4] <- test$p.value
      }
    }
  }
  mc$p <- p.adjust(mc$p.unadj, method = method)
  mc$stars <- " "
  mc$stars[mc$p < 0.1] <- "."
  mc$stars[mc$p < 0.05] <- "*"
  mc$stars[mc$p < 0.01] <- "**"
  mc$stars[mc$p < 0.001] <- "***"
  names(mc)[6] <- " "
  
  cat("\n========== 多重比较 (Wilcoxon 秩和检验) ==========\n")
  cat(paste("P值校正方法:", method, "\n\n"))
  print(mc[-4], right = TRUE)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  return(invisible(NULL))
}

cat("==================== 环境与函数准备完成 ====================\n\n")

# ================================================================
# 练习题开始
# ================================================================

# -------------------- 练习题 1 --------------------------------
cat("\n########## 练习题 1 ##########\n")
cat("任务：使用 mtcars 数据集，对 mpg, hp, wt 计算描述性统计，\n")
cat("      包括均值、标准差、偏度、峰度，并比较 summary() 和 psych::describe() 的输出。\n\n")

# 自定义统计函数（含偏度峰度）
my_stats <- function(x) {
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  skew <- sum((x - m)^3 / s^3) / n
  kurt <- sum((x - m)^4 / s^4) / n - 3
  c(mean = m, sd = s, skew = skew, kurtosis = kurt)
}

cat("--- 使用 summary() ---\n")
print(summary(mtcars[, c("mpg", "hp", "wt")]))

cat("\n--- 使用自定义函数计算偏度峰度 ---\n")
print(sapply(mtcars[, c("mpg", "hp", "wt")], my_stats))

cat("\n--- 使用 psych::describe() ---\n")
print(describe(mtcars[, c("mpg", "hp", "wt")]))

# -------------------- 练习题 2 --------------------------------
cat("\n########## 练习题 2 ##########\n")
cat("任务：使用 mtcars，按气缸数 (cyl) 分组，计算 mpg 的均值、中位数、标准差。\n\n")

mpg_by_cyl <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    n = n(),
    mean_mpg = mean(mpg),
    median_mpg = median(mpg),
    sd_mpg = sd(mpg)
  )
print(mpg_by_cyl)

# -------------------- 练习题 3 --------------------------------
cat("\n########## 练习题 3 ##########\n")
cat("任务：使用 iris 数据集，按 Species 分组，输出 Sepal.Length 的\n")
cat("      描述统计（n, mean, sd, median, MAD），使用 dplyr。\n\n")

iris_summary <- iris %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    mean = mean(Sepal.Length),
    sd = sd(Sepal.Length),
    median = median(Sepal.Length),
    mad = mad(Sepal.Length)
  )
print(iris_summary)

# -------------------- 练习题 4 --------------------------------
cat("\n########## 练习题 4 ##########\n")
cat("任务：使用 Arthritis 数据集，制作 Treatment 和 Improved 的列联表，\n")
cat("      并计算行百分比、列百分比，添加边际和。\n\n")

mytable <- xtabs(~ Treatment + Improved, data = Arthritis)
cat("--- 列联表 ---\n")
print(mytable)
cat("\n--- 行百分比（按 Treatment）---\n")
print(prop.table(mytable, 1))
cat("\n--- 列百分比（按 Improved）---\n")
print(prop.table(mytable, 2))
cat("\n--- 添加边际和 ---\n")
print(addmargins(mytable))

# -------------------- 练习题 5 --------------------------------
cat("\n########## 练习题 5 ##########\n")
cat("任务：对 Arthritis 的 Treatment 和 Improved 进行卡方检验，并计算 Cramer's V。\n\n")

chi_result <- chisq.test(mytable)
print(chi_result)
cat("\n--- 关联强度 (Cramer's V) ---\n")
print(assocstats(mytable)$cramer)

# -------------------- 练习题 6 --------------------------------
cat("\n########## 练习题 6 ##########\n")
cat("任务：使用 Arthritis 三维表（Treatment, Sex, Improved），\n")
cat("      进行 Cochran-Mantel-Haenszel 检验，控制 Sex。\n\n")

mytable3 <- xtabs(~ Treatment + Sex + Improved, data = Arthritis)
print(mantelhaen.test(mytable3))

# -------------------- 练习题 7 --------------------------------
cat("\n########## 练习题 7 ##########\n")
cat("任务：使用 state.x77 数据集，计算 Population, Income, Illiteracy,\n")
cat("      Life Exp, Murder 之间的相关系数矩阵，并检验显著性（使用 corr.test）。\n\n")

states <- state.x77[, c("Population", "Income", "Illiteracy", "Life Exp", "Murder")]
print(corr.test(states, use = "complete"))

# -------------------- 练习题 8 --------------------------------
cat("\n########## 练习题 8 ##########\n")
cat("任务：在控制 Income 和 Illiteracy 后，计算 Population 和 Life Exp 的偏相关系数。\n\n")

# 变量索引：1=Population, 2=Income, 3=Illiteracy, 4=Life Exp, 5=Murder
# 控制变量为 2 和 3，计算 1 和 4 的偏相关
pcor_value <- pcor(c(1, 4, 2, 3), cov(states))
cat("偏相关系数 (控制 Income 和 Illiteracy)：", pcor_value, "\n")

# -------------------- 练习题 9 --------------------------------
cat("\n########## 练习题 9 ##########\n")
cat("任务：使用 UScrime 数据集，检验 Prob 在 So 两个水平上的差异\n")
cat("      （独立样本 t检验 和 Wilcoxon 检验）。\n\n")

cat("--- 独立样本 t检验 (Welch) ---\n")
print(t.test(Prob ~ So, data = UScrime))

cat("\n--- Mann-Whitney U 检验 ---\n")
print(wilcox.test(Prob ~ So, data = UScrime))

# -------------------- 练习题 10 --------------------------------
cat("\n########## 练习题 10 ##########\n")
cat("任务：使用 UScrime 数据集，检验 U1 和 U2 的差异\n")
cat("      （配对 t检验 和 Wilcoxon 符号秩检验）。\n\n")

cat("--- 配对 t检验 ---\n")
with(UScrime, print(t.test(U1, U2, paired = TRUE)))

cat("\n--- Wilcoxon 配对符号秩检验 ---\n")
with(UScrime, print(wilcox.test(U1, U2, paired = TRUE)))

# -------------------- 练习题 11 --------------------------------
cat("\n########## 练习题 11 ##########\n")
cat("任务：使用 PlantGrowth 数据集，进行 Kruskal-Wallis 检验比较不同 group 的 weight，\n")
cat("      然后使用 wmc 进行事后两两比较（Holm 校正）。\n\n")

cat("--- Kruskal-Wallis 检验 ---\n")
print(kruskal.test(weight ~ group, data = PlantGrowth))

cat("\n--- 事后两两比较 (wmc) ---\n")
wmc(weight ~ group, data = PlantGrowth, method = "holm")

# -------------------- 练习题 12 --------------------------------
cat("\n########## 练习题 12 ##########\n")
cat("任务：使用 chickwts 数据集，比较不同饲料类型 (feed) 对体重 (weight) 的影响，\n")
cat("      使用非参数方法（Kruskal-Wallis + wmc）。\n\n")

cat("--- Kruskal-Wallis 检验 ---\n")
print(kruskal.test(weight ~ feed, data = chickwts))

cat("\n--- 事后两两比较 (wmc) ---\n")
wmc(weight ~ feed, data = chickwts, method = "holm")

# -------------------- 练习题 13 --------------------------------
cat("\n########## 练习题 13 ##########\n")
cat("任务：使用 CO2 数据集，比较不同 Treatment（chilled vs nonchilled）对 uptake 的影响，\n")
cat("      使用 Wilcoxon 检验（独立样本，但注意每个植物有重复，此处简单处理为独立样本）。\n\n")

# 简单提取两组数据（忽略植物个体差异，仅作示例）
co2_chilled <- CO2$uptake[CO2$Treatment == "chilled"]
co2_nonchilled <- CO2$uptake[CO2$Treatment == "nonchilled"]
wilcox_result <- wilcox.test(co2_chilled, co2_nonchilled)
print(wilcox_result)

# -------------------- 练习题 14 --------------------------------
cat("\n########## 练习题 14 ##########\n")
cat("任务：使用 mtcars，制作 am 和 vs 的二维列联表，并计算 phi 系数。\n\n")

mytable_am_vs <- table(mtcars$am, mtcars$vs)
print(mytable_am_vs)
cat("\n--- 卡方检验 ---\n")
print(chisq.test(mytable_am_vs))
cat("\n--- phi 系数 (关联强度) ---\n")
print(assocstats(mytable_am_vs)$phi)

# -------------------- 练习题 15 --------------------------------
cat("\n########## 练习题 15 ##########\n")
cat("任务：使用 dplyr 对 Salaries 数据集，按 rank 和 sex 分组，\n")
cat("      计算 salary 的均值、中位数、标准差。\n\n")

salary_summary <- Salaries %>%
  group_by(rank, sex) %>%
  summarise(
    n = n(),
    mean_salary = mean(salary),
    median_salary = median(salary),
    sd_salary = sd(salary),
    .groups = "drop"
  )
print(salary_summary)

# -------------------- 完成 --------------------------------
savehistory(file = paste0("gpy_week6_R_history_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".Rhistory"))