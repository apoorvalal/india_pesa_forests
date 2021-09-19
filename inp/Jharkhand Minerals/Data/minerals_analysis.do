cd "~/Dropbox/india_pesa_forests/inp/Jharkhand Minerals/Data/"
insheet using "Master_Data_v2.csv", names clear


use "nameb_index" to merge with "jharkhand_blocks.csv"
