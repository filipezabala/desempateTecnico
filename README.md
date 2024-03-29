# Desempate Técnico

Arquivos e funções utilizadas na dissertação de [Filipe Zabala (2009) - Desempate Técnico](https://doi.org/10.11606/D.45.2009.tde-01032021-140004).

## Instalação
```r
# Instalando pacotes
chooseCRANmirror(ind = 11) # Brazil
packs <- c('devtools', 'ellipse', 'klaR', 'rgl', 'VGAM')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(packs, dep=TRUE)
update.packages(checkBuilt = TRUE, ask = FALSE)
devtools::install_github('filipezabala/desempateTecnico', force = T)

# Chamando biblioteca
library(desempateTecnico)
```

## Exemplos
O exemplos a seguir dependem de simulações, portanto devem apresentar resultados ligeiramente diferentes entre as rodadas.
### `bayes`
```r
bayes(c(.4,.3,.3), 1000)
```
![](img/bayes1000.png)
```r
bayes(c(.3,.25,.2,.1,.05), 100)
```
![](img/bayes100.png)
```r
bayes(rep(1/5,5), 500)
```
![](img/bayes500.png)

A seguir estão cenários com empate técnico tríplice segundo os institutos de pesquisa.
```r
bayes(c(.5813972562, .3158114522, .1027912917), 50)
```
![](img/empate50.png)
```r
bayes(c(.5144202347, .3246860305, .1608937348), 100)
```
![](img/empate100.png)
```r
bayes(c(.4160925601, .3316216347, .2522858052), 500)
```
![](img/empate500.png)
```r
bayes(c(.3919345050, .3324785813, .2755869137), 10^3)
```
![](img/empate1000.png)
```r
bayes(c(.3518464606, .3332479566, .3149055828), 10^4)
```
![](img/empate10000.png)
```r
bayes(c(.3391808234, .3333247966, .3274943799), 10^5)
bayes(c(.3333333335, .3333333333, .3333333331), 10^20)
```

### `simplex2d`
```r
simplex2d()
```
![](img/simplex2d.png)

### `simplex3d`
```r
simplex3d(.4, .3, .3, 1000)
```
![](img/simplex3d1000.png)
```r
simplex3d(.5144202347, .3246860305, .1608937348, 100)
```
![](img/simplex3d100.png)

