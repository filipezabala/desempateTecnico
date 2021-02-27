#' Generates a 2D simplex.
#' @import klaR
#' @examples simplex2d()
#' @export
simplex2d <- function(){
  # Gráfico para compreender mehor onde ocorrem os diversos empates
  rpa <- matrix(c(1,0,0, 0,.5,.5), nrow=2, ncol=3, byrow=T)
  rpb <- matrix(c(0,1,0, .5,0,.5), nrow=2, ncol=3, byrow=T)
  rpc <- matrix(c(0,0,1, .5,.5,0), nrow=2, ncol=3, byrow=T)
  
  mpa <- matrix(c(.5,0,.5, .5,.5,0), nrow=2, ncol=3, byrow=T)
  mpb <- matrix(c(0,.5,.5, .5,.5,0), nrow=2, ncol=3, byrow=T)
  mpc <- matrix(c(.5,0,.5, 0,.5,.5), nrow=2, ncol=3, byrow=T)
  
  l1 <- matrix(c(1,0,0, 0,1,0), nrow=2, ncol=3, byrow=T)
  l2 <- matrix(c(0,1,0, 0,0,1), nrow=2, ncol=3, byrow=T)
  l3 <- matrix(c(0,0,1, 1,0,0), nrow=2, ncol=3, byrow=T)
  
  klaR::triplot(label = c(expression(pc), expression(pa), expression(pb)), grid=F)
  klaR::trilines(rpa, col="red")
  klaR::trilines(rpb, col="red")
  klaR::trilines(rpc, col="red")
  klaR::trilines(mpa, col="green3", pch=16)
  klaR::trilines(mpb, col="green3")
  klaR::trilines(mpc, col="green3")
  
  # Colocando os pontos
  text(klaR::tritrafo(c(.5,.5,0)), "(0.5, 0, 0.5)", adj=c(1,-0.5))
  text(klaR::tritrafo(c(0,.5,.5)), "(0.5, 0.5, 0)", adj=c(0,-0.5))
  text(klaR::tritrafo(c(.5,0,.5)), "(0, 0.5, 0.5)", adj=c(0.5,1.5))
  
  text(klaR::tritrafo(c(0,1,0)), "(1, 0, 0)", adj=c(.5,-0.5))
  text(klaR::tritrafo(c(0,0,1)), "(0, 1, 0)", adj=c(1,1.5))
  text(klaR::tritrafo(c(1,0,0)), "(0, 0, 1)", adj=c(-0.3,1.5))
  
  # Apontando as regiões
  text(klaR::tritrafo(c(0.05, 0.65, 0.3)), "1", adj=c(3,0))
  text(klaR::tritrafo(c(0.3, 0.65, 0.05)), "2", adj=c(-2,0))
  text(klaR::tritrafo(c(0.05, 0.3, 0.65)), "3", adj=c(1,2))
  text(klaR::tritrafo(c(0.05, 0.3, 0.65)), "4", adj=c(5,6))
  text(klaR::tritrafo(c(0.65, 0.3, 0.05)), "5", adj=c(0,2))
  text(klaR::tritrafo(c(0.65, 0.3, 0.05)), "6", adj=c(-3,6))
  text(klaR::tritrafo(c(0.5, 0.4, 0.1)), "7", adj=c(-10,-1))
  text(klaR::tritrafo(c(0.5, 0.4, 0.1)), "8", adj=c(-12,2))
  text(klaR::tritrafo(c(0.5, 0.4, 0.1)), "9", adj=c(-6,-1))
  text(klaR::tritrafo(c(0.5, 0.4, 0.1)), "10", adj=c(-2,2))
  text(klaR::tritrafo(c(0.5, 0.2, 0.3)), "11", adj=c(-3,-0.4))
  text(klaR::tritrafo(c(0.5, 0.25, 0.25)), "12", adj=c(-1,1))
  
  # Legendas
  # triplot(grid=F, frame=F)
  # legend(x = -0.5, y = 0.5, 
  # 	 legend = c(
  #       expression(paste("1)   ", A, " ganha no 1º turno, ", B, " em 2º lugar")),
  #       expression(paste("2)   ", A, " ganha no 1º turno, ", C, " em 2º lugar")),
  #       expression(paste("3)   ", B, " ganha no 1º turno, ", A, " em 2º lugar")),
  #       expression(paste("4)   ", B, " ganha no 1º turno, ", C, " em 2º lugar")),
  #       expression(paste("5)   ", C, " ganha no 1º turno, ", A, " em 2º lugar")),
  #       expression(paste("6)   ", C, " ganha no 1º turno, ", B, " em 2º lugar")),
  #       expression(paste("7)   ", A, " e ", B, " no 2º turno, ", A, " na frente")) ,
  #       expression(paste("8)   ", A, " e ", B, " no 2º turno, ", B, " na frente")) ,
  #       expression(paste("9)   ", A, " e ", C, " no 2º turno, ", A, " na frente")) ,
  #       expression(paste("10) ", A, " e ", C, " no 2º turno, ", C, " na frente")) ,
  #       expression(paste("11) ", B, " e ", C, " no 2º turno, ", B, " na frente")) ,
  #       expression(paste("12) ", B, " e ", C, " no 2º turno, ", C, " na frente"))  
  #              )
  #       )
}
