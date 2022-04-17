kNN Supervised ML - Breast Cancer
================

## R Markdown

## Supervised Machine Learning w/ kNN algorithm.

#### Dados disponíveis em: <http://archive.ics.uci.edu/ml>

#### Estes dados apresentam medidas obtidas pela digitalização de imagens. Os valores representam as medidas do núcleo celular presente na imagem. As features são as medidas de determinada característica de três formas diferentes, exemplo: área, perímetros etc.

#### O objetivo desta análise é predizer com base nos valores se o cancer de mama é benigno ou maligno.

#### O dataset possui 569 exemplos de biopcias de cancer, com 32 features (colunas).

#### a feature de interesse está codada como M para maligno e B para Benigno (cancer).

### Chamando as bibliotecas

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.1.2

    ## Warning: package 'tibble' was built under R version 4.1.2

    ## Warning: package 'tidyr' was built under R version 4.1.2

    ## Warning: package 'purrr' was built under R version 4.1.2

    ## Warning: package 'dplyr' was built under R version 4.1.2

    ## Warning: package 'stringr' was built under R version 4.1.2

    ## Warning: package 'forcats' was built under R version 4.1.2

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.1.2

    ## Carregando pacotes exigidos: lattice

    ## Warning: package 'lattice' was built under R version 4.1.2

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(class)
```

    ## Warning: package 'class' was built under R version 4.1.2

## 1. Explorando os dados

### 1.1 Lendo os dados

``` r
data = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/wisc_bc_data.csv', header = T, stringsAsFactors = F)
head(data,5)
```

    ##         id diagnosis radius_mean texture_mean perimeter_mean area_mean
    ## 1   842302         M       17.99        10.38         122.80    1001.0
    ## 2   842517         M       20.57        17.77         132.90    1326.0
    ## 3 84300903         M       19.69        21.25         130.00    1203.0
    ## 4 84348301         M       11.42        20.38          77.58     386.1
    ## 5 84358402         M       20.29        14.34         135.10    1297.0
    ##   smoothness_mean compactness_mean concavity_mean concave.points_mean
    ## 1         0.11840          0.27760         0.3001             0.14710
    ## 2         0.08474          0.07864         0.0869             0.07017
    ## 3         0.10960          0.15990         0.1974             0.12790
    ## 4         0.14250          0.28390         0.2414             0.10520
    ## 5         0.10030          0.13280         0.1980             0.10430
    ##   symmetry_mean fractal_dimension_mean radius_se texture_se perimeter_se
    ## 1        0.2419                0.07871    1.0950     0.9053        8.589
    ## 2        0.1812                0.05667    0.5435     0.7339        3.398
    ## 3        0.2069                0.05999    0.7456     0.7869        4.585
    ## 4        0.2597                0.09744    0.4956     1.1560        3.445
    ## 5        0.1809                0.05883    0.7572     0.7813        5.438
    ##   area_se smoothness_se compactness_se concavity_se concave.points_se
    ## 1  153.40      0.006399        0.04904      0.05373           0.01587
    ## 2   74.08      0.005225        0.01308      0.01860           0.01340
    ## 3   94.03      0.006150        0.04006      0.03832           0.02058
    ## 4   27.23      0.009110        0.07458      0.05661           0.01867
    ## 5   94.44      0.011490        0.02461      0.05688           0.01885
    ##   symmetry_se fractal_dimension_se radius_worst texture_worst perimeter_worst
    ## 1     0.03003             0.006193        25.38         17.33          184.60
    ## 2     0.01389             0.003532        24.99         23.41          158.80
    ## 3     0.02250             0.004571        23.57         25.53          152.50
    ## 4     0.05963             0.009208        14.91         26.50           98.87
    ## 5     0.01756             0.005115        22.54         16.67          152.20
    ##   area_worst smoothness_worst compactness_worst concavity_worst
    ## 1     2019.0           0.1622            0.6656          0.7119
    ## 2     1956.0           0.1238            0.1866          0.2416
    ## 3     1709.0           0.1444            0.4245          0.4504
    ## 4      567.7           0.2098            0.8663          0.6869
    ## 5     1575.0           0.1374            0.2050          0.4000
    ##   concave.points_worst symmetry_worst fractal_dimension_worst
    ## 1               0.2654         0.4601                 0.11890
    ## 2               0.1860         0.2750                 0.08902
    ## 3               0.2430         0.3613                 0.08758
    ## 4               0.2575         0.6638                 0.17300
    ## 5               0.1625         0.2364                 0.07678

### 1.2 Verificando as classes das colunas

``` r
str(data)
```

    ## 'data.frame':    569 obs. of  32 variables:
    ##  $ id                     : int  842302 842517 84300903 84348301 84358402 843786 844359 84458202 844981 84501001 ...
    ##  $ diagnosis              : chr  "M" "M" "M" "M" ...
    ##  $ radius_mean            : num  18 20.6 19.7 11.4 20.3 ...
    ##  $ texture_mean           : num  10.4 17.8 21.2 20.4 14.3 ...
    ##  $ perimeter_mean         : num  122.8 132.9 130 77.6 135.1 ...
    ##  $ area_mean              : num  1001 1326 1203 386 1297 ...
    ##  $ smoothness_mean        : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
    ##  $ compactness_mean       : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
    ##  $ concavity_mean         : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
    ##  $ concave.points_mean    : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
    ##  $ symmetry_mean          : num  0.242 0.181 0.207 0.26 0.181 ...
    ##  $ fractal_dimension_mean : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
    ##  $ radius_se              : num  1.095 0.543 0.746 0.496 0.757 ...
    ##  $ texture_se             : num  0.905 0.734 0.787 1.156 0.781 ...
    ##  $ perimeter_se           : num  8.59 3.4 4.58 3.44 5.44 ...
    ##  $ area_se                : num  153.4 74.1 94 27.2 94.4 ...
    ##  $ smoothness_se          : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
    ##  $ compactness_se         : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
    ##  $ concavity_se           : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
    ##  $ concave.points_se      : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
    ##  $ symmetry_se            : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
    ##  $ fractal_dimension_se   : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
    ##  $ radius_worst           : num  25.4 25 23.6 14.9 22.5 ...
    ##  $ texture_worst          : num  17.3 23.4 25.5 26.5 16.7 ...
    ##  $ perimeter_worst        : num  184.6 158.8 152.5 98.9 152.2 ...
    ##  $ area_worst             : num  2019 1956 1709 568 1575 ...
    ##  $ smoothness_worst       : num  0.162 0.124 0.144 0.21 0.137 ...
    ##  $ compactness_worst      : num  0.666 0.187 0.424 0.866 0.205 ...
    ##  $ concavity_worst        : num  0.712 0.242 0.45 0.687 0.4 ...
    ##  $ concave.points_worst   : num  0.265 0.186 0.243 0.258 0.163 ...
    ##  $ symmetry_worst         : num  0.46 0.275 0.361 0.664 0.236 ...
    ##  $ fractal_dimension_worst: num  0.1189 0.089 0.0876 0.173 0.0768 ...

#### Uma verificação importante a ser feita é em relação a presença de valores NA ou Vazios, pois estes interferem em nossas analises.

### Valores NA ou vazios.

``` r
colSums(is.na(data) | data == '')
```

    ##                      id               diagnosis             radius_mean 
    ##                       0                       0                       0 
    ##            texture_mean          perimeter_mean               area_mean 
    ##                       0                       0                       0 
    ##         smoothness_mean        compactness_mean          concavity_mean 
    ##                       0                       0                       0 
    ##     concave.points_mean           symmetry_mean  fractal_dimension_mean 
    ##                       0                       0                       0 
    ##               radius_se              texture_se            perimeter_se 
    ##                       0                       0                       0 
    ##                 area_se           smoothness_se          compactness_se 
    ##                       0                       0                       0 
    ##            concavity_se       concave.points_se             symmetry_se 
    ##                       0                       0                       0 
    ##    fractal_dimension_se            radius_worst           texture_worst 
    ##                       0                       0                       0 
    ##         perimeter_worst              area_worst        smoothness_worst 
    ##                       0                       0                       0 
    ##       compactness_worst         concavity_worst    concave.points_worst 
    ##                       0                       0                       0 
    ##          symmetry_worst fractal_dimension_worst 
    ##                       0                       0

#### como pode ser visto acima os dados estão completos.

#### Como pode ser verificado a primeira coluna é ‘ID’, esta coluna deve ser sempre retirada em modelos de machine learning pois se caso ela seja considerada como uma ‘feature’ ela pode ser utilizada para predizer ‘unicamente’ o valor, não generalizando para outros dados, causando problemas com overfitting.

### 1.3 Para tal vamos remove-la:

``` r
data = data[,-1]
names(data)
```

    ##  [1] "diagnosis"               "radius_mean"            
    ##  [3] "texture_mean"            "perimeter_mean"         
    ##  [5] "area_mean"               "smoothness_mean"        
    ##  [7] "compactness_mean"        "concavity_mean"         
    ##  [9] "concave.points_mean"     "symmetry_mean"          
    ## [11] "fractal_dimension_mean"  "radius_se"              
    ## [13] "texture_se"              "perimeter_se"           
    ## [15] "area_se"                 "smoothness_se"          
    ## [17] "compactness_se"          "concavity_se"           
    ## [19] "concave.points_se"       "symmetry_se"            
    ## [21] "fractal_dimension_se"    "radius_worst"           
    ## [23] "texture_worst"           "perimeter_worst"        
    ## [25] "area_worst"              "smoothness_worst"       
    ## [27] "compactness_worst"       "concavity_worst"        
    ## [29] "concave.points_worst"    "symmetry_worst"         
    ## [31] "fractal_dimension_worst"

#### A próxima variável será diagnosis, que é a classe rótulo, e será a classe que nós esperamos predizer no modelo. A função table() no R apresenta as seguintes características para esta feature

### 1.4 Apresentando a classe de predição

``` r
round(prop.table(table(data$diagnosis))*100, digits = 2)
```

    ## 
    ##     B     M 
    ## 62.74 37.26

#### Conforme pode ser visualizado acima, os dados apresentam a seguinte distribuição em porcentagem.

#### Um dos requisitos importantes que pode ser visualizado no indice 1.2 é o fato que essa feature está na classe chr (string), devemos transforma-la para factor pois nosso modelo necessita que esteja de tal forma.

### 1.5 Transformando chr em factor da variável de dependente.

``` r
data$diagnosis = factor(data$diagnosis, levels = c('B', 'M'), labels = c('Benign', 'Malignant'))
levels(data$diagnosis)
```

    ## [1] "Benign"    "Malignant"

#### Observe que os dados de diagnosis agora apresentam levels (níveis), pois é um fator.

#### O restante das variáveis são todas numéricas (item 1.1),

#### Uma rápida descrição de três variáveis a título de curiosidade.

### 1.6 Summary de três variáveis

``` r
summary(data[,c("radius_mean", "area_mean", "smoothness_mean")])
```

    ##   radius_mean       area_mean      smoothness_mean  
    ##  Min.   : 6.981   Min.   : 143.5   Min.   :0.05263  
    ##  1st Qu.:11.700   1st Qu.: 420.3   1st Qu.:0.08637  
    ##  Median :13.370   Median : 551.1   Median :0.09587  
    ##  Mean   :14.127   Mean   : 654.9   Mean   :0.09636  
    ##  3rd Qu.:15.780   3rd Qu.: 782.7   3rd Qu.:0.10530  
    ##  Max.   :28.110   Max.   :2501.0   Max.   :0.16340

#### O algorítimo de KNN é altamente dependente da ‘padronização’ das escalas das unidades, conhecido como feature scaling. A padronização ou normalização dos dados podem ser utilizados para tal.

#### Observe o range das features acima, elas não devem ser utilizadas desta forma no modelo, pois poderiam causar problemas.

#### Iremos utilizar a normalização ao invés ad padronização de dados. A normalização ocorre da seguinte forma: (x - min(x) / (max(x) - min(x)).

### 1.7 Normalizando os dados numéricos.

``` r
norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

datan = as.data.frame(lapply(data[,2:31],norm))
head(datan,5)
```

    ##   radius_mean texture_mean perimeter_mean area_mean smoothness_mean
    ## 1   0.5210374    0.0226581      0.5459885 0.3637328       0.5937528
    ## 2   0.6431445    0.2725736      0.6157833 0.5015907       0.2898799
    ## 3   0.6014956    0.3902604      0.5957432 0.4494168       0.5143089
    ## 4   0.2100904    0.3608387      0.2335015 0.1029056       0.8113208
    ## 5   0.6298926    0.1565776      0.6309861 0.4892895       0.4303512
    ##   compactness_mean concavity_mean concave.points_mean symmetry_mean
    ## 1        0.7920373      0.7031396           0.7311133     0.6863636
    ## 2        0.1817680      0.2036082           0.3487575     0.3797980
    ## 3        0.4310165      0.4625117           0.6356859     0.5095960
    ## 4        0.8113613      0.5656045           0.5228628     0.7762626
    ## 5        0.3478928      0.4639175           0.5183897     0.3782828
    ##   fractal_dimension_mean radius_se texture_se perimeter_se    area_se
    ## 1              0.6055181 0.3561470 0.12046941    0.3690336 0.27381126
    ## 2              0.1413227 0.1564367 0.08258929    0.1244405 0.12565979
    ## 3              0.2112468 0.2296216 0.09430251    0.1803704 0.16292179
    ## 4              1.0000000 0.1390911 0.17587518    0.1266550 0.03815479
    ## 5              0.1868155 0.2338222 0.09306489    0.2205626 0.16368757
    ##   smoothness_se compactness_se concavity_se concave.points_se symmetry_se
    ## 1     0.1592956     0.35139844   0.13568182         0.3006251  0.31164518
    ## 2     0.1193867     0.08132304   0.04696970         0.2538360  0.08453875
    ## 3     0.1508312     0.28395470   0.09676768         0.3898466  0.20569032
    ## 4     0.2514532     0.54321507   0.14295455         0.3536655  0.72814769
    ## 5     0.3323588     0.16791841   0.14363636         0.3570752  0.13617943
    ##   fractal_dimension_se radius_worst texture_worst perimeter_worst area_worst
    ## 1            0.1830424    0.6207755     0.1415245       0.6683102 0.45069799
    ## 2            0.0911101    0.6069015     0.3035714       0.5398177 0.43521431
    ## 3            0.1270055    0.5563856     0.3600746       0.5084417 0.37450845
    ## 4            0.2872048    0.2483102     0.3859275       0.2413467 0.09400806
    ## 5            0.1457996    0.5197439     0.1239339       0.5069476 0.34157491
    ##   smoothness_worst compactness_worst concavity_worst concave.points_worst
    ## 1        0.6011358         0.6192916       0.5686102            0.9120275
    ## 2        0.3475533         0.1545634       0.1929712            0.6391753
    ## 3        0.4835898         0.3853751       0.3597444            0.8350515
    ## 4        0.9154725         0.8140117       0.5486422            0.8848797
    ## 5        0.4373638         0.1724151       0.3194888            0.5584192
    ##   symmetry_worst fractal_dimension_worst
    ## 1      0.5984624               0.4188640
    ## 2      0.2335896               0.2228781
    ## 3      0.4037059               0.2134330
    ## 4      1.0000000               0.7737111
    ## 5      0.1575005               0.1425948

#### conforme pode ser observado acima, os dados já estão normalizados, basicamente feito uma reescala entre 0 e 1.

``` r
summary(datan$area_mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.1174  0.1729  0.2169  0.2711  1.0000

## ALGORÍTIMO kNN

### o Algoritimo kNN (k-nearest neighbors) é conhecido pela sua característica de classificar

### dados não rotulados pelo fato de ser capaz de assimilar estes aos dados rotulados mais similares.

### O algorítimo apresenta resultados satisfatórios quando existem diferenças bem definidas entre as classes, porém, caso a diferença não seja muito clara entre os grupos o algoritimo não é recomendado. As árvores de decisão (decision-tree-randomforest podriam ser utilizadas).

#### Separando em dados de teste e treino. Para isso utilizaremos o pacote CARET, separando em 80-20.

``` r
index = createDataPartition(data$diagnosis, times = 1, p=0.8, list= FALSE)

data_treino = datan[index,]
data_teste = datan[-index,]

data_treino_rotulo = data[index,1]
data_teste_rotulo = data[-index,1]
```

#### Construindo o classificador e realizando as predições.

#### O classificador KNN possui a variável K que determina o quão bem o classificador será quando generalizado com dados no futuro. Escolher um K muito grande reduz o impacto ou variancia causada por dados ruidosos, entretanto, mas pode enviesar o classificador no sentido de poder ignorar padrões pequenos.

#### Sendo assim, uma prática recomendada é setar o K como sendo a raiz quadrada do número de exemplos de treino, em nosso caso temos 469, resultado em um valor de \~21.

### Desta forma, podemos treinar nosso modelo.

``` r
data_pred = knn(data_treino, data_teste, cl = data_treino_rotulo, k = 21)
summary(data_pred)
```

    ##    Benign Malignant 
    ##        74        39

### Avaliando o modelo - Matriz de confusão.

``` r
confusionMatrix(data_teste_rotulo,data_pred)
```

    ## Confusion Matrix and Statistics
    ## 
    ##            Reference
    ## Prediction  Benign Malignant
    ##   Benign        71         0
    ##   Malignant      3        39
    ##                                           
    ##                Accuracy : 0.9735          
    ##                  95% CI : (0.9244, 0.9945)
    ##     No Information Rate : 0.6549          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9423          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.2482          
    ##                                           
    ##             Sensitivity : 0.9595          
    ##             Specificity : 1.0000          
    ##          Pos Pred Value : 1.0000          
    ##          Neg Pred Value : 0.9286          
    ##              Prevalence : 0.6549          
    ##          Detection Rate : 0.6283          
    ##    Detection Prevalence : 0.6283          
    ##       Balanced Accuracy : 0.9797          
    ##                                           
    ##        'Positive' Class : Benign          
    ## 

#### Como pode ser observado acima, nosso modelo teve uma acurácia global de 96%, tendo acertado todas as predições em caso do tumor ser Benigno. No caso do tumor ser maligno tivemos um acerto de 38 de 42.

#### Vale ressaltar que erros em relação ao tumor ser maligno mas estar sendo classificado como benigno deve ser levado a sério, uma vez que pode levar ao paciente pensar que ele possue um tumor benigno, entretanto a doença continuará a se espalhar.

#### Existem casos que o tumor é benigno, entretanto, é classificado como maligno. Esse erro apesar de não ser tão preocupante quanto ao exemplo anterior pode acarretar em problemas desnecessarios, tais como custos finaceiros.

## Melhorando o classificador

#### Duas abordagens que poderiam melhorar a qualidade do modelo seriam

#### 1° Alterar como os dados são transformados em mesma escala. No exemplo acima estamos usando normalização, entretanto, podemos utilizar a padronização (z-score) dos dados.

#### 2° Alterar o valor de k utilizado no modelo. Essa abordagem, conforme comentado, é o numero de ‘vizinhos mais próximos’ que o algoritimo se refere.

### Utilizando padronização dos dados - Z-score

#### Vamos atribuir o dataset a uma nova variável.

``` r
data.2 = as.data.frame(scale(data[,-1]))
head(data.2,5)
```

    ##   radius_mean texture_mean perimeter_mean  area_mean smoothness_mean
    ## 1   1.0960995   -2.0715123      1.2688173  0.9835095       1.5670875
    ## 2   1.8282120   -0.3533215      1.6844726  1.9070303      -0.8262354
    ## 3   1.5784992    0.4557859      1.5651260  1.5575132       0.9413821
    ## 4  -0.7682333    0.2535091     -0.5921661 -0.7637917       3.2806668
    ## 5   1.7487579   -1.1508038      1.7750113  1.8246238       0.2801253
    ##   compactness_mean concavity_mean concave.points_mean symmetry_mean
    ## 1        3.2806281     2.65054179           2.5302489   2.215565542
    ## 2       -0.4866435    -0.02382489           0.5476623   0.001391139
    ## 3        1.0519999     1.36227979           2.0354398   0.938858720
    ## 4        3.3999174     1.91421287           1.4504311   2.864862154
    ## 5        0.5388663     1.36980615           1.4272370  -0.009552062
    ##   fractal_dimension_mean radius_se texture_se perimeter_se    area_se
    ## 1              2.2537638 2.4875451 -0.5647681    2.8305403  2.4853907
    ## 2             -0.8678888 0.4988157 -0.8754733    0.2630955  0.7417493
    ## 3             -0.3976580 1.2275958 -0.7793976    0.8501802  1.1802975
    ## 4              4.9066020 0.3260865 -0.1103120    0.2863415 -0.2881246
    ## 5             -0.5619555 1.2694258 -0.7895490    1.2720701  1.1893103
    ##   smoothness_se compactness_se concavity_se concave.points_se symmetry_se
    ## 1    -0.2138135     1.31570389    0.7233897         0.6602390   1.1477468
    ## 2    -0.6048187    -0.69231710   -0.4403926         0.2599334  -0.8047423
    ## 3    -0.2967439     0.81425704    0.2128891         1.4235749   0.2368272
    ## 4     0.6890953     2.74186785    0.8187979         1.1140268   4.7285198
    ## 5     1.4817634    -0.04847723    0.8277425         1.1431989  -0.3607748
    ##   fractal_dimension_se radius_worst texture_worst perimeter_worst area_worst
    ## 1           0.90628565     1.885031   -1.35809849       2.3015755  1.9994782
    ## 2          -0.09935632     1.804340   -0.36887865       1.5337764  1.8888270
    ## 3           0.29330133     1.510541   -0.02395331       1.3462906  1.4550043
    ## 4           2.04571087    -0.281217    0.13386631      -0.2497196 -0.5495377
    ## 5           0.49888916     1.297434   -1.46548091       1.3373627  1.2196511
    ##   smoothness_worst compactness_worst concavity_worst concave.points_worst
    ## 1        1.3065367         2.6143647       2.1076718            2.2940576
    ## 2       -0.3752817        -0.4300658      -0.1466200            1.0861286
    ## 3        0.5269438         1.0819801       0.8542223            1.9532817
    ## 4        3.3912907         3.8899747       1.9878392            2.1738732
    ## 5        0.2203623        -0.3131190       0.6126397            0.7286181
    ##   symmetry_worst fractal_dimension_worst
    ## 1      2.7482041               1.9353117
    ## 2     -0.2436753               0.2809428
    ## 3      1.1512420               0.2012142
    ## 4      6.0407261               4.9306719
    ## 5     -0.8675896              -0.3967505

#### Criando dados de treino novamente (vamos criar um novo indexador)

``` r
index.z = createDataPartition(data$diagnosis, times = 1, p=0.8, list= FALSE)

data_treino.z = data.2[index,]
data_teste.z = data.2[-index,]

data_treino_rotulo.z = data[index,1]
data_teste_rotulo.z = data[-index,1]
```

#### Modelo

``` r
data_pred.z = knn(data_treino.z, data_teste.z, cl = data_treino_rotulo.z, k = 21)
summary(data_pred)
```

    ##    Benign Malignant 
    ##        74        39

#### Avaliando o modelo

``` r
cm = confusionMatrix(data_teste_rotulo.z,data_pred.z)
cm$overall
```

    ##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
    ##   9.646018e-01   9.227086e-01   9.118436e-01   9.902724e-01   6.637168e-01 
    ## AccuracyPValue  McnemarPValue 
    ##   3.495060e-15   1.336144e-01

#### Como pode ser observado a diferença não houve grandes diferenças utilizando os dados em formato padronizado ao invés de normalizados.

### Outra abordagem que será realizada é tentar alterar o número k do algoritimo.

``` r
for (i in seq(1,30,5) ) {
  model = knn(data_treino.z, data_teste.z, cl = data_treino_rotulo.z, k = i)
  cm = confusionMatrix(data_teste_rotulo.z,model)
  print(as.vector(cm$overall[1]))
}
```

    ## [1] 0.9646018
    ## [1] 0.9557522
    ## [1] 0.9823009
    ## [1] 0.9734513
    ## [1] 0.9646018
    ## [1] 0.9646018

#### como pode ser observado não houve grande diferença na acucarácia global alterando o valor de k.Uma abordagem que poderia ser realizada para verificar qual o melhor k a ser usado seria criar amostras aleatórias com 100 pacientes e verificar qual o melhor k que se adequa. Conforme comentado a literatura indica o valor de k como sendo a raiz quadradada do numero de amostras, porém nem sempre este apresenta a melhor acurácia. Essa mudança ocorre principalmente pelo fato que os dados possuem distribuição diferente.

## Resumindo

#### Apesar do fato de que o kNN é um algoritimo simples, este é capaz de ser eficaz em tarefas complexas, tais como classificação de cancer. Por meio do classificador kNN fomos capaz de classificar se um paciente possui cancer benigno ou maligno em aproximadamente 96% das vezes.
