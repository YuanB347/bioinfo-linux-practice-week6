

> 本章以 `mtcars`、`Arthritis`、`state.x77`、`Salaries` 等经典数据集为载体，系统展示了R中**描述性统计、频数分析、相关性、假设检验及非参数多重比较**的全套工具链。

---

## 一、描述性统计：从“感觉”到“数据”的转换

### 1.1 基础函数速览
| 函数/包 | 用途 | 亮点 |
|---------|------|------|
| `summary()` | 快速概览（最小值、四分位数、均值、最大值） | 内置，零依赖 |
| `sapply()` + 自定义函数 | 灵活计算偏度、峰度等自定义指标 | 可扩展性强 |
| `Hmisc::describe()` | 输出缺失值、唯一值、最值、百分位数 | 适合数据清洗初探 |
| `pastecs::stat.desc()` | 一次输出均值、标准差、变异系数、置信区间等 | 统计量全面 |
| `psych::describe()` | 输出中位数、修剪均值、最小值、最大值、偏度、峰度 | 心理学常用，结果清晰 |

**理解**：  
- 没有“最好”的函数，只有“最适合当前任务”的函数。  
- 快速探索用 `summary()` + `sapply()`；正式报告推荐 `psych::describe()`，因为它的输出既包含稳健统计量（中位数、修剪均值），又包含分布形状（偏度、峰度），便于判断是否接近正态。

### 1.2 分组描述与交互式聚合
```r
# 按一个变量分组
by(mtcars[myvars], mtcars$am, dstats)

# 按两个变量分组（自动生成交叉分组）
by(mtcars[myvars], list(Transmission=mtcars$am, Engine=mtcars$vs), FUN=dstats)
```
**思考**：  
`by()` 是基础R的分组利器，但返回的是列表，不易进一步处理。而 `dplyr` 的 `group_by() + summarize()` 更加现代、易读，且可以直接输出整洁的数据框，便于后续绘图或导出。

```r
Salaries %>%
  group_by(rank, sex) %>%
  summarize(n = n(), med = median(salary), min = min(salary), max = max(salary))
```
**对比**：  
`by()` 适合快速查看，`dplyr` 适合构建可复用的分析流水线。在实际项目中，我倾向于 `dplyr` 链，因为它与 `ggplot2` 和 `tidyr` 无缝衔接。

---

## 二、频数表和列联表：类别数据的“放大镜”

### 2.1 一维、二维、三维表
- `table()` / `xtabs()` 生成列联表。
- `prop.table()` 计算比例，`addmargins()` 添加边际和。
- `ftable()` 将三维表展平为二维，便于阅读。

**关键技巧**：
```r
# 行比例（行和为1）
prop.table(mytable, 1)
# 列比例（列和为1）
prop.table(mytable, 2)
# 三维表中指定 margin 进行比例计算
ftable(prop.table(mytable, c(1, 2)))  # 按 Treatment×Sex 分层计算 Improved 比例
```

**思考**：  
- 在处理三维表时，一定要想清楚“比例相对于谁”。比如医学研究中，常按“治疗组×性别”分层，计算“改善”的比例，此时 `c(1,2)` 的 margin 正是我们需要的。
- `gmodels::CrossTable()` 提供了卡方检验、期望频数、残差等，是探索关联性的好起点，尤其适合教学。

### 2.2 独立性检验与关联强度
| 检验 | 适用场景 | 函数 |
|------|----------|------|
| 卡方检验 | 两个分类变量是否独立 | `chisq.test()` |
| Fisher 精确检验 | 样本量小（期望频数<5） | `fisher.test()` |
| Cochran-Mantel-Haenszel | 控制第三个分层变量后检验独立性 | `mantelhaen.test()` |
| 关联强度 | 计算 φ 系数、列联系数、Cramer's V | `assocstats()` |

**代码示例**：
```r
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)   # p = 0.0014，显著相关
assocstats(mytable)   # Cramer's V = 0.394，中等强度
```
**理解**：  
- 卡方检验只能判断“是否独立”，而 `assocstats` 给出效应量，对于实际意义更重要。
- 当三维表涉及分层变量时，`mantelhaen.test` 能检验在控制分层因素后，两个变量是否仍然独立。这在流行病学（如控制年龄、性别后分析暴露与疾病关系）中极其重要。

---

## 三、相关分析与协方差：连续变量间的“舞蹈”

### 3.1 协方差与相关系数
- `cov()` 协方差矩阵，`cor()` 相关系数矩阵（默认 Pearson）。
- `cor(method="spearman")` 处理等级数据或非线性关系。

**关键代码**：
```r
# 两组变量间的交叉相关
x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)
```

### 3.2 偏相关（Partial Correlation）
偏相关衡量在控制其他变量影响后，两个变量间的净关联。
```r
library(ggm)
pcor(c(1,5,2,3,6), cov(states))
# 表示控制变量 2,3,6 后，变量1与变量5的偏相关系数
```
**思考**：  
偏相关是回归分析的基础，能剔除混杂变量干扰。例如，在控制收入、文盲率、高中毕业率后，人口与寿命的偏相关性可能比简单相关更真实。

### 3.3 相关系数的显著性检验
- `cor.test()` 对单一相关系数进行检验。
- `psych::corr.test()` 一次性输出所有相关系数及其 p 值矩阵，非常高效。

**使用建议**：  
当需要快速筛选变量关系时，`corr.test()` 返回的矩阵直接标注显著性星号，大大提升效率。

---

## 四、均值检验：参数 vs 非参数

### 4.1 独立样本 t 检验 vs 配对 t 检验
```r
# 独立样本（双样本）t检验
t.test(Prob ~ So, data=UScrime)   # 方差不相等时默认 Welch 校正

# 配对样本 t 检验
with(UScrime, t.test(U1, U2, paired=TRUE))
```

### 4.2 非参数替代：Wilcoxon 检验
当数据不满足正态性或存在异常值时，采用秩和检验：
```r
# 两独立样本 Mann-Whitney U 检验
wilcox.test(Prob ~ So, data=UScrime)

# 配对样本 Wilcoxon 符号秩检验
with(UScrime, wilcox.test(U1, U2, paired=TRUE))
```

**感悟**：  
- 参数检验（t检验）在样本量足够大且分布对称时效率更高；非参数检验更稳健，但对小样本的检验效能略低。
- 实际数据分析中，我总是先做探索性绘图（箱线图、QQ图），若明显偏态则直接选择非参数方法，避免因不符合假设而导致错误结论。

### 4.3 多组比较：Kruskal-Wallis 与事后多重比较
```r
kruskal.test(Illiteracy ~ state.region, data=states)   # 整体差异
# 事后两两比较（自定义 wmc 函数）
wmc(Illiteracy ~ state.region, data=states, method="holm")
```
**重点剖析 `wmc` 函数的设计思想**（结合 ch07-wmc script.R）：
1. **排序**：默认按中位数排序，使结果表一目了然（低→高）。
2. **描述统计**：输出 N、Median、MAD（中位数绝对偏差），比标准差更稳健。
3. **两两 Wilcoxon 检验**：采用循环枚举所有配对。
4. **P 值校正**：`holm` 法控制族系错误率，比 Bonferroni 更灵敏。
5. **星号标记**：按 p < 0.001, 0.01, 0.05, 0.1 分别标记，便于快速识别。

**思考**：  
- 这个函数体现了“非参数+多重校正”的完整流程，比市面上很多仅输出原始 p 值的包更严谨。
- 唯一不足是未提供效应量（如 r = Z/√N），若后续需要可以自行添加。
- 在实际报告中，我会将 `wmc` 与箱线图叠加，可视化展示组间差异。

---

## 五、综合实战

1. **数据清洗**：用 `summary()` + `psych::describe()` 检查缺失和异常。
2. **分组探索**：使用 `dplyr` 链计算分组摘要，同时绘制 `ggplot2` 箱线图/直方图。
3. **关系分析**：
   - 分类 vs 分类 → 列联表 + `chisq.test` + `assocstats`
   - 连续 vs 连续 → 散点图 + `corr.test`
   - 控制混杂 → 偏相关或回归。
4. **假设检验**：
   - 两组比较：先检验正态性（`shapiro.test`），酌情选择 t 或 Wilcoxon。
   - 多组比较：Kruskal-Wallis + `wmc` 事后检验。
5. **结果输出**：使用 `knitr::kable` 美化表格，`ggsave` 保存图表。

---

## 六、本章核心思想

- **统计不是机械执行函数，而是结合问题背景和数据特征选择合适工具。**
- **描述统计看清数据全貌；推断统计量化证据强度。**
- **非参数方法虽然“安全”，但当数据满足参数假设时，参数方法更敏感。因此，事前探索至关重要。**
- **多重比较校正不是可选项，而是必选项——否则我们会被“偶然显著性”误导。**

