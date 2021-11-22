Missing Data & Feature Engineering
================
Gulio Valenzuela 20190336
21 November 2021

# **Parte 1**

## 1. Reporte detallado de missing data para todas las columnas.

``` r
summary(ds_MD)
```

    ##   PassengerId       Survived          Pclass          Name          
    ##  Min.   :  2.0   Min.   :0.0000   Min.   :1.000   Length:183        
    ##  1st Qu.:263.5   1st Qu.:0.0000   1st Qu.:1.000   Class :character  
    ##  Median :457.0   Median :1.0000   Median :1.000   Mode  :character  
    ##  Mean   :455.4   Mean   :0.6721   Mean   :1.191                     
    ##  3rd Qu.:676.0   3rd Qu.:1.0000   3rd Qu.:1.000                     
    ##  Max.   :890.0   Max.   :1.0000   Max.   :3.000                     
    ##                                                                     
    ##      Sex                 Age            SibSp            Parch      
    ##  Length:183         Min.   : 0.92   Min.   :0.0000   Min.   :0.000  
    ##  Class :character   1st Qu.:24.00   1st Qu.:0.0000   1st Qu.:0.000  
    ##  Mode  :character   Median :35.50   Median :0.0000   Median :0.000  
    ##                     Mean   :35.69   Mean   :0.4611   Mean   :0.462  
    ##                     3rd Qu.:48.00   3rd Qu.:1.0000   3rd Qu.:1.000  
    ##                     Max.   :80.00   Max.   :3.0000   Max.   :4.000  
    ##                     NA's   :25      NA's   :3        NA's   :12     
    ##     Ticket               Fare           Cabin             Embarked        
    ##  Length:183         Min.   :  0.00   Length:183         Length:183        
    ##  Class :character   1st Qu.: 29.70   Class :character   Class :character  
    ##  Mode  :character   Median : 56.93   Mode  :character   Mode  :character  
    ##                     Mean   : 78.96                                        
    ##                     3rd Qu.: 90.54                                        
    ##                     Max.   :512.33                                        
    ##                     NA's   :8

El primer problema con la data es que la columnas con informacion
relevante tiene datos con un tipo incorrecto.

Uso mutate para arreglar este problema;

``` r
ds_MD <- ds_MD %>% mutate(Survived = factor(Survived),
                              Pclass = factor(Pclass),
                              Sex = factor(Sex),
                              SibSp = factor(SibSp),
                              Parch = factor(Parch),
                              Embarked = factor(Embarked)) %>% 
  select(-PassengerId, -Name)
summary(ds_MD)
```

    ##  Survived Pclass      Sex          Age         SibSp      Parch    
    ##  0: 60    1:158   ?     :51   Min.   : 0.92   0   :109   0   :116  
    ##  1:123    2: 15   female:64   1st Qu.:24.00   1   : 62   1   : 33  
    ##           3: 10   male  :68   Median :35.50   2   :  6   2   : 21  
    ##                               Mean   :35.69   3   :  3   4   :  1  
    ##                               3rd Qu.:48.00   NA's:  3   NA's: 12  
    ##                               Max.   :80.00                        
    ##                               NA's   :25                           
    ##     Ticket               Fare           Cabin           Embarked
    ##  Length:183         Min.   :  0.00   Length:183          : 12   
    ##  Class :character   1st Qu.: 29.70   Class :character   C: 59   
    ##  Mode  :character   Median : 56.93   Mode  :character   Q:  2   
    ##                     Mean   : 78.96                      S:110   
    ##                     3rd Qu.: 90.54                              
    ##                     Max.   :512.33                              
    ##                     NA's   :8

Observaciones: Faltantes de cada categoria:

-   Sex: 51  
-   Age: 25  
-   SibSp: 3
-   Parch: 12
-   Fare: 8
-   Embarked: 12

## 2. Para cada columna especificar qué tipo de modelo se utilizará (solo el nombre y el porqué) y qué valores se le darán a todos los missing values. (Ej. Imputación sectorizada por la moda, bins, y cualquier otro método visto anteriormente).

-   Para la columna `Sex` uso un modelo de regresión logística, pues por
    la cantidad de faltantes y al haber `female` y `male` este modelo se
    adecuara para no sesgar el resultado.
-   Para la columna `Age` uso una imputacion general mediante el uso de
    la mediana ya que es una distribucion relativamente normal no tendra
    tanto sesgo.
-   Para las columnas `SibSp`,`Parch` y `Embarked`yuso la imputacion
    general por moda pues son pocos faltantes que no alteraran mucho el
    resultado el uso de la moda.
-   Para `Fare` uso una imputacion general mediante el uso de la mediana
    ya que es una distribucion relativamente normal y de esta forma me
    aseguro que los datos no se sesgen al ser informacion dispersa.

## 3. Reporte de qué filas están completas

Filas sin faltantes:

-   Survived
-   Pclass
-   Ticket
-   Cabin
-   Pasangerid
-   Name

## 4. Utilizar los siguientes métodos para cada columna que contiene missing values:

### a. Imputación general (media, moda y mediana)

``` r
# Imputacion por moda en la columna sex
Sex_mode <- ifelse(ds_MD$Sex=='?', 
                   yes = '3',
                   no = ds_MD$Sex)
# Imputacion por media en la columna Age
Age_mean <- ifelse(is.na(ds_MD$Age),
                   yes = mean(ds_MD$Age, na.rm = T),
                   no = ds_MD$Age)
# Imputacion por mediana en la columna Age
Age_median <- ifelse(is.na(ds_MD$Age),
                   yes = median(ds_MD$Age, na.rm = T),
                   no = ds_MD$Age)
# Imputacion por moda en la columna SibSb
SibSp_mode <- ifelse(is.na(ds_MD$SibSp), 
                   yes = '0',
                   no = ds_MD$SibSp)
# Imputacion por moda en la columna Parch
Parch_mode <- ifelse(is.na(ds_MD$Parch), 
                   yes = '0',
                   no = ds_MD$Parch)
# Imputacion por media en la columna Fare
Fare_mean <- ifelse(is.na(ds_MD$Fare),
                   yes = mean(ds_MD$Fare, na.rm = T),
                   no = ds_MD$Fare)
# Imputacion por mediana en la columna Fare
Fare_median <- ifelse(is.na(ds_MD$Fare),
                   yes = median(ds_MD$Fare, na.rm = T),
                   no = ds_MD$Fare)
# Imputacion por moda en la columna Embarked
Embarked_mode <- ifelse(ds_MD$Embarked=='', 
                   yes = '0',
                   no = ds_MD$Embarked)
```

### b. Modelo de regresión lineal

``` r
# Modelo de regresion en la columna Sex
data_for_regression <- ds_MD %>% filter(Sex!='?') %>% 
  select(Survived,Pclass,Age,Fare,Embarked,Sex) %>% 
  na.omit()
linreg_sex <- glm(formula = Sex~.,
                  data = data_for_regression, family = binomial)
new_data <- ds_MD[ds_MD$Sex=='?',]
probabilities <- linreg_sex %>% predict(new_data, type="response")
predicted_sex <- ifelse(probabilities>0.5,"female","male")
table(predicted_sex)
```

    ## predicted_sex
    ## female   male 
    ##     12     27

``` r
# Modelo de regresion lineal en la columna Age
linreg_age <- lm(Age~Survived+Pclass+Sex+SibSp+Parch+Fare+Embarked,
                 data = ds_MD)
Age_linreg <- predict(linreg_age, ds_MD)
# Modelo de regresion lineal en la columna Fare
linreg_fare <- lm(Fare~Survived+Pclass+Sex+SibSp+Parch+Age+Embarked,
                 data = ds_MD)
Fare_linreg <- predict(linreg_fare, ds_MD)
```

### c. Outliers: Uno de los dos métodos vistos en clase (Standard deviation approach o Percentile approach)

``` r
# Outliers metodo Percentile aproach en la columna Age
percentiles <- quantile(ds_MD$Age,na.rm = T)
Age_outliers <- ifelse(ds_MD$Age>percentiles[5],
                       yes = percentiles[5],
                       no = ds_MD$Age)
# Outliers metodo Percentile aproach en la columna Fare
percentiles <- quantile(ds_MD$Fare,na.rm = T)
Fare_outliers <- ifelse(ds_MD$Fare>percentiles[5],
                       yes = percentiles[5],
                       no = ds_MD$Fare)
```

## 5. Al comparar los métodos del inciso 4 contra “titanic.csv”, ¿Qué método (para cada columna) se acerca más a la realidad y por qué?

``` r
# Llenando los datos de las columnas de ds_MD
ds_MD$Age <- ifelse(is.na(ds_MD$Age),
                   yes = median(ds_MD$Age, na.rm = T),
                   no = ds_MD$Age)
ds_MD$SibSp <- ifelse(is.na(ds_MD$SibSp), 
                   yes = '0',
                   no = ds_MD$SibSp)
ds_MD$Parch <- ifelse(is.na(ds_MD$Parch), 
                   yes = '0',
                   no = ds_MD$Parch)
ds_MD$Fare <- ifelse(is.na(ds_MD$Fare),
                   yes = median(ds_MD$Fare, na.rm = T),
                   no = ds_MD$Fare)
ds_MD$Embarked <- ifelse(ds_MD$Embarked=='', 
                   yes = '0',
                   no = ds_MD$Embarked)
ds_for_regression <- ds_MD %>% filter(Sex!='?') %>% 
  select(Survived,Pclass,Age,SibSp,Parch,Fare,Embarked,Sex)
linreg_sex <- glm(formula = Sex~.,
                  data = ds_for_regression, family = binomial)
new_ds <- ds_MD[ds_MD$Sex=='?',]
```

comparando con `titanic.csv`: Comparacion Age

``` r
summary(ds$Age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.92   24.00   36.00   35.67   47.50   80.00

``` r
summary(ds_MD$Age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.92   25.00   35.50   35.67   46.50   80.00

Comparacion Fare

``` r
summary(ds$Fare)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   29.70   57.00   78.68   90.00  512.33

``` r
summary(ds_MD$Fare)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   30.00   56.93   78.00   90.00  512.33

Comparacion Sex

``` r
summary(ds$Sex)
```

    ##    Length     Class      Mode 
    ##       183 character character

``` r
summary(ds_MD$Sex)
```

    ##      ? female   male 
    ##     51     64     68

Comparacion Embarked

``` r
summary(ds$Embarked)
```

    ##    Length     Class      Mode 
    ##       183 character character

``` r
summary(ds_MD$Embarked)
```

    ##    Length     Class      Mode 
    ##       183 character character

En la comparacion vemos que los metodos utilizados nos regresan datos
muy parcedios a “titanic.csv” en valores numericos vemos como los datos
son muy parecidos previniendo algun sesgo por parte de la imputacion.

## 6. Conclusiones

Para trabajar cualquier tipo de dato hay que hacer la exploracion
correspondiente de manera de entender realmente que podemos encontrar en
los datos dados. Esto nos permite familiarizarnos con lo que estemos
trabjando y entender que debemos hacer y como hacerlo. Ya teniendo
conocimiento de la data dada se hace la limpiesa y ajuste de la misma
para tenerla en un formato adecuado para trabajar. Esta misma
exploracion nos permite saber las distribuciones de la data que esto ya
nos permite tomar la decision de que tipo de imputacion implementar para
tener un resultado acorde a lo que se esta trabajando. La mayoria de
metodos permiten hacercarse a los datos deseados sin enbargo es analizar
la situacion y el motivo del mismo para poder llevar acabo un mejor
analisis y que este acorde a lo espertado.

# **Parte 2**

### 1. Luego del pre-procesamiento de la data con Missing Values, normalice las columnas numéricas por los métodos:

## a. Standarization

``` r
stan_age <- (ds_MD$Age - mean(ds_MD$Age))/sd(ds_MD$Age)
stan_fare <- (ds_MD$Fare - mean(ds_MD$Fare))/sd(ds_MD$Fare)
```

### b. MinMaxScaling

``` r
mmsca_age <- (ds_MD$Age - min(ds_MD$Age))/(max(ds_MD$Age)-min(ds_MD$Age))
mmsca_fare <- (ds_MD$Fare - min(ds_MD$Fare))/(max(ds_MD$Fare)-min(ds_MD$Fare))
```

### c. MaxAbsScaler

``` r
abs_age <- ds_MD$Age/max(abs(ds_MD$Age))
abs_fare <- ds_MD$Fare/max(abs(ds_MD$Fare))
```

## 2. Compare los estadísticos que considere más importantes para su conclusión y compare contra la data completa de “titanic.csv” (deberán de normalizar también).

``` r
# Normalicacion en "Titanic.CSV"

#Age
stan_age2 <- (ds$Age - mean(ds$Age))/sd(ds$Age)
mmsca_age2 <- (ds$Age - min(ds$Age))/(max(ds$Age)-min(ds$Age))
abs_age2 <- ds$Age/max(abs(ds$Age))

#Fare
stan_fare2 <- (ds$Fare - mean(ds$Fare))/sd(ds$Fare)
mmsca_fare2 <- (ds$Fare - min(ds$Fare))/(max(ds$Fare)-min(ds$Fare))
abs_fare2 <- ds$Fare/max(abs(ds$Fare))
```

**Comparacion**

La columna \`\`Age\`\`\`

Standarization

``` r
summary(stan_age)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.39182 -0.73423 -0.01144  0.00000  0.74576  3.05180

``` r
summary(stan_age2)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.22160 -0.74626  0.02081  0.00000  0.75592  2.83342

MinMaxSacaling

``` r
summary(mmsca_age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.3045  0.4373  0.4394  0.5764  1.0000

``` r
summary(mmsca_age2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.2919  0.4436  0.4395  0.5890  1.0000

MaxAbs Scale

``` r
summary(abs_age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0115  0.3125  0.4437  0.4458  0.5813  1.0000

``` r
summary(abs_age2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0115  0.3000  0.4500  0.4459  0.5938  1.0000

La columna `Fare`

Standarization

``` r
summary(stan_fare)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -1.0337 -0.6361 -0.2792  0.0000  0.1591  5.7566

``` r
summary(stan_fare2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -1.0306 -0.6416 -0.2840  0.0000  0.1482  5.6799

MinMaxScaling

``` r
summary(mmsca_fare)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.00000 0.05856 0.11112 0.15224 0.17567 1.00000

``` r
summary(mmsca_fare2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.00000 0.05797 0.11126 0.15358 0.17567 1.00000

MaxAbs Scale

``` r
summary(abs_fare)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.00000 0.05856 0.11112 0.15224 0.17567 1.00000

``` r
summary(abs_fare2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.00000 0.05797 0.11126 0.15358 0.17567 1.00000
