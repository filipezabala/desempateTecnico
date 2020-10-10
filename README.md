# Desempate Técnico

Arquivos e funções utilizadas na dissertação de Filipe Zabala (2009) - Desempate Técnico.

## Instalação
```r
# Instalando pacotes
packs <- c('mvtnorm','VGAM','devtools')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(packs, dep=TRUE)
update.packages(checkBuilt = TRUE, ask = FALSE)
devtools::install_github('filipezabala/desempate_tecnico', force = T)
```
