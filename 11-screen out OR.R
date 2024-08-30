
library(openxlsx)
library(dplyr)
library(stringr)


rev <- read.xlsx("GCST90275044.tsv___rev_or_ivw_significant.xlsx") %>%
       .$id.outcome %>%
       str_extract(.,".*?(?=_)")

mr <- read.xlsx("GCST90275044.tsv___outcome_mr_all_significant.xlsx") %>%
      subset(!id.exposure %in% rev)

mr_ivw <- read.xlsx("GCST90275044.tsv___outcome_mr_ivw_significant.xlsx") %>%
          subset(!id.exposure %in% rev)

write.xlsx(mr,"OR table (GCST90275044).xlsx")
write.xlsx(mr_ivw,"OR table (GCST90275044)_ivw.xlsx")


