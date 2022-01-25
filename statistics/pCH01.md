第一章
================

參考書籍：R語言 量表編制、統計分析與試題反映理論<br>
資料來源：<https://www.wunan.com.tw/bookdetail?NO=13814><br>
範例檔案：CH01_1.csv<br>

# 一、匯入資料與前處理

``` r
# 引入套件
library(readr)
```

``` r
# 範例檔案來源
file_name <- "/DATA/CH01/CH01_1.csv"
file_sourse <- paste0(getwd(), file_name)

# 原始資料 data0
data0 <- read.csv(file_sourse, fileEncoding="UCS-2LE")
head(data0)
```

    ##   座號 德育 智育 體育 群育 美育
    ## 1    1   88   90   86   90   83
    ## 2    2   85   89   87   91   85
    ## 3    3   84   90   83   86   80
    ## 4    4   83   81   83   93   81
    ## 5    5   84   85   85   89   88
    ## 6    6   88   93   86   91   87

``` r
# 抓取"德育"的資料
data0[, 2]
```

    ##  [1] 88 85 84 83 84 88 87 88 88 74 83 81 92 88 91 93 90 92 92 94 92 93 90 94 89
    ## [26] 95 93 88 94 94 90 84 92 97 92 92 93 96 94

``` r
# 將5種資料都儲存到變數內
moral  <- data0[, 2]
wisdom <- data0[, 3]
sport  <- data0[, 4]
group  <- data0[, 5]
pretty <- data0[, 6]
```

# 二、計算個變項的平均數、中位數以及總和。

``` r
# 計算平均數
cat("平均數\n")
```

    ## 平均數

``` r
cat("德育：", mean(moral), "\n")
```

    ## 德育： 89.66667

``` r
cat("智育：", mean(wisdom), "\n")
```

    ## 智育： 89.84615

``` r
cat("體育：", mean(sport), "\n")
```

    ## 體育： 87.69231

``` r
cat("群育：", mean(group), "\n")
```

    ## 群育： 90.82051

``` r
cat("美育：", mean(pretty), "\n")
```

    ## 美育： 86.74359

``` r
# 計算中位數
cat("中位數\n")
```

    ## 中位數

``` r
cat("德育：", median(moral), "\n")
```

    ## 德育： 91

``` r
cat("智育：", median(wisdom), "\n")
```

    ## 智育： 92

``` r
cat("體育：", median(sport), "\n")
```

    ## 體育： 88

``` r
cat("群育：", median(group), "\n")
```

    ## 群育： 91

``` r
cat("美育：", median(pretty), "\n")
```

    ## 美育： 88

``` r
# 計算總和
cat("總和\n")
```

    ## 總和

``` r
cat("德育：", sum(moral), "\n")
```

    ## 德育： 3497

``` r
cat("智育：", sum(wisdom), "\n")
```

    ## 智育： 3504

``` r
cat("體育：", sum(sport), "\n")
```

    ## 體育： 3420

``` r
cat("群育：", sum(group), "\n")
```

    ## 群育： 3542

``` r
cat("美育：", sum(pretty), "\n")
```

    ## 美育： 3383

# 三、分析各變項的全距、變異數、標準差、第1四分位數、第2四分位數、第3四分位數等變異量數。

``` r
# 計算全距
cat("全距\n")
```

    ## 全距

``` r
cat("德育：", max(moral)-min(moral), "\n")
```

    ## 德育： 23

``` r
cat("智育：", max(wisdom)-min(wisdom), "\n")
```

    ## 智育： 26

``` r
cat("體育：", max(sport)-min(sport), "\n")
```

    ## 體育： 13

``` r
cat("群育：", max(group)-min(group), "\n")
```

    ## 群育： 14

``` r
cat("美育：", max(pretty)-min(pretty), "\n")
```

    ## 美育： 26

``` r
# 計算變異數
cat("變異數\n")
```

    ## 變異數

``` r
cat("德育：", var(moral), "\n")
```

    ## 德育： 22.4386

``` r
cat("智育：", var(wisdom), "\n")
```

    ## 智育： 38.92308

``` r
cat("體育：", var(sport), "\n")
```

    ## 體育： 6.744939

``` r
cat("群育：", var(group), "\n")
```

    ## 群育： 8.993252

``` r
cat("美育：", var(pretty), "\n")
```

    ## 美育： 22.45884

``` r
# 計算標準差
cat("標準差\n")
```

    ## 標準差

``` r
cat("德育：", sd(moral), "\n")
```

    ## 德育： 4.73694

``` r
cat("智育：", sd(wisdom), "\n")
```

    ## 智育： 6.238836

``` r
cat("體育：", sd(sport), "\n")
```

    ## 體育： 2.597102

``` r
cat("群育：", sd(group), "\n")
```

    ## 群育： 2.998875

``` r
cat("美育：", sd(pretty), "\n")
```

    ## 美育： 4.739076

``` r
# 計算第1四分位數、第2四分位數(中位數)、第3四分位數
summary(data0)
```

    ##       座號           德育            智育            體育            群育      
    ##  Min.   : 1.0   Min.   :74.00   Min.   :72.00   Min.   :80.00   Min.   :81.00  
    ##  1st Qu.:10.5   1st Qu.:88.00   1st Qu.:87.00   1st Qu.:86.50   1st Qu.:89.50  
    ##  Median :20.0   Median :91.00   Median :92.00   Median :88.00   Median :91.00  
    ##  Mean   :20.0   Mean   :89.67   Mean   :89.85   Mean   :87.69   Mean   :90.82  
    ##  3rd Qu.:29.5   3rd Qu.:93.00   3rd Qu.:94.00   3rd Qu.:90.00   3rd Qu.:93.00  
    ##  Max.   :39.0   Max.   :97.00   Max.   :98.00   Max.   :93.00   Max.   :95.00  
    ##       美育      
    ##  Min.   :69.00  
    ##  1st Qu.:84.00  
    ##  Median :88.00  
    ##  Mean   :86.74  
    ##  3rd Qu.:89.50  
    ##  Max.   :95.00

# 四、新增一個 total 變項，此變項為前5個變項的總和，並檢視資料。

``` r
head(data0, 5)
```

    ##   座號 德育 智育 體育 群育 美育
    ## 1    1   88   90   86   90   83
    ## 2    2   85   89   87   91   85
    ## 3    3   84   90   83   86   80
    ## 4    4   83   81   83   93   81
    ## 5    5   84   85   85   89   88

``` r
total <- apply(head(data0[c('德育', '智育', '體育', '群育', '美育')], 5), 2, sum)
print(total)
```

    ## 德育 智育 體育 群育 美育 
    ##  424  435  424  449  417
