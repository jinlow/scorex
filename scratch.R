# Scratch


library(data.table)

df <- fread("E:/Customer_Projects/ForaFin1905_8883/data/LexisNexis_RetroScore_Data_ForaFinancial_merged.csv")
setkey(df, "key")

# SBA Score
sba <- fread("E:/Customer_Projects/ForaFin1905_8883/data/fora_8883_appdt_sba_nonsbfe_return.csv")
setkey(sba, "key")

fp <- fread("E:/Customer_Projects/ForaFin1905_8883/data/fora_8883_appdt_fp30_return.csv")

# Create Performance Fields -----------------------------------------------
df[, early_dflt := ifelse(PerformanceTag == "EarlyDefaultWO"
                          | PerformanceTag == "ActiveEarlyDefault", 1, 0)]
df[, WrittenOff := ifelse(PerformanceTag == "WrittenOff", 1, 0)]
df[, ActiveDefault := ifelse(PerformanceTag == 'ActiveDefault', 1, 0)]

df[sba, model1score := i.model1score, on = 'key']
df[sba, model2score := i.model2score, on = 'key']
df[fp, fp_score := i.fp_score, on = 'key']
df[fp, fp_min := fp_score*-1]


# Test Scorex -------------------------------------------------------------
library(scorex)
sctabs <- scorex(model1score + fp_score ~ model2score | early_dflt, data = df,
                 cut_method = "percentiles", method_args = c(1, 5, 25, 50, 80, 100),
                 exceptions = c(0, 222, NA))

sctabs$tables

scorex_to_xl(sctabs)



