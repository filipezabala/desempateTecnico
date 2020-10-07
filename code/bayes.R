#########################
### ANÁLISE BAYESIANA ###
#########################

# Limpando a memória
# rm(list=ls(all=TRUE))

# Instalando pacotes
packs <- c('mvtnorm','VGAM')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(packs, dep=TRUE)
update.packages(checkBuilt = TRUE, ask = FALSE)

# Chamando pacotes
library(VGAM) # Biblioteca para gerar pseudo-observações Dirichlet via 'rdiric'
library(mvtnorm) # Permite calcular probs em normais multivariadas


# Função para cálculo das posterioris via Monte Carlo Ordinário
bayes <- function(p=vector(),n){ 

  priori <- 1 # Definindo os parâmetros da priori (todos iguais)
  r <- 10^4   # Definindo o número de repetições 'r' da posteriori Dirichlet
  result <- rep(0, length(p)+choose(length(p),2)) # Vetor que guardará o número de vezes que ocorre cada cenário
  cenario  <- rep(0, length(p)+choose(length(p),2)) # Vetor que guardará os nomes dos candidatos
  comb <- combn(LETTERS[1:length(p)], 2)  # Criando todas as combinações dos candidatos 2 a 2
  cenario[1:length(p)] <- c(LETTERS[1:length(p)])  # Atribuindo nos 'nomes' dos candidatos às primeiras posições
  for(i in 1:ncol(comb)){
    cenario[length(p)+i] <- paste(comb[1,i],comb[2,i], sep = '')  # Adicionando os 'nomes' pareados
  }

  # Iniciando o loop do Monte Carlo Ordinário
  for(i in 1:r) 
    {
    d <- rdiric(n=1, shape=c(p*n+priori)) # Criando um vetor de observações Dirichlets de dimensão length(p)

    if(max(d)>.5) # Verificando a frequência dos length(p) candidatos que ganhou no 1º turno
       {
         j <- which.max(d)
         result[j] <- result[j] + 1
       }
    else  # Caso ninguém ganhe no 1º turno o loop cai aqui
       {
         prim <- which.max(d)  # Definindo a posição no vetor 'd' do primeiro colocado no 2º turno       
         d[which.max(d)] <- 0  # Zerando o máximo para ver quem é o segundo colocado                                                  
         seg  <- which.max(d)  # Definindo a posição no vetor 'd' do segundo colocado no 2º turno                                                
         # O contador 'j' abaixo não sai imediatamente. Para criá-lo precisei fazer um esquema listando 
         # todos os candidatos e todas a combinações 2 a 2, na ordem, para compreender o porque desta função.
         # Ex.: num cenário com 5 candidatos deve-se anotar A B C D E | AB AC AD AE BC BD BE CD CE DE
         #                                                  1 2 3 4 5   6  7  8  9  10 11 12 13 14 15
         # Note que length(d)=5 nesse caso. Logo, as posições 6 a 15 saem como função das posições 1 a 5 de
         # acordo com a expressão abaixo. Essas posições serão as posições no vetor 'result'.
         j <- ( prim + seg ) + sum( (length(p)-min(prim,seg)-1) : (length(p)-2) )           
         result[j] <- result[j] + 1                                                         
       }

   # cat(i/r*100, "%", "\n") # Apresentando o percentual de processamento concluído

		}
  
  final <- cbind(cenario, result/sum(result))
  colnames(final) <- c('cenario', 'posteriori')
  print(final)

} # Fim da função 'bayes'
