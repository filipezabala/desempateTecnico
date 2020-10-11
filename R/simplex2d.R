#' Generates a 2D simplex.
#' @import klaR
#' @examples simplex2d()
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
  
  triplot(label = c(expression(p[A[3]]), expression(p[A[1]]), expression(p[A[2]])), grid=F)
  trilines(rpa, col="red")
  trilines(rpb, col="red")
  trilines(rpc, col="red")
  trilines(mpa, col="green3", pch=16)
  trilines(mpb, col="green3")
  trilines(mpc, col="green3")
  
  # Colocando os pontos
  text(tritrafo(c(.5,.5,0)), "(0.5, 0, 0.5)", adj=c(1,-0.5))
  text(tritrafo(c(0,.5,.5)), "(0.5, 0.5, 0)", adj=c(0,-0.5))
  text(tritrafo(c(.5,0,.5)), "(0, 0.5, 0.5)", adj=c(0.5,1.5))
  
  text(tritrafo(c(0,1,0)), "(1, 0, 0)", adj=c(.5,-0.5))
  text(tritrafo(c(0,0,1)), "(0, 1, 0)", adj=c(1,1.5))
  text(tritrafo(c(1,0,0)), "(0, 0, 1)", adj=c(-0.3,1.5))
  
  # Apontando as regiões
  text(tritrafo(c(0.05, 0.65, 0.3)), "1", adj=c(3,0))
  text(tritrafo(c(0.3, 0.65, 0.05)), "2", adj=c(-2,0))
  text(tritrafo(c(0.05, 0.3, 0.65)), "3", adj=c(1,2))
  text(tritrafo(c(0.05, 0.3, 0.65)), "4", adj=c(5,6))
  text(tritrafo(c(0.65, 0.3, 0.05)), "5", adj=c(0,2))
  text(tritrafo(c(0.65, 0.3, 0.05)), "6", adj=c(-3,6))
  text(tritrafo(c(0.5, 0.4, 0.1)), "7", adj=c(-10,-1))
  text(tritrafo(c(0.5, 0.4, 0.1)), "8", adj=c(-12,2))
  text(tritrafo(c(0.5, 0.4, 0.1)), "9", adj=c(-6,-1))
  text(tritrafo(c(0.5, 0.4, 0.1)), "10", adj=c(-2,2))
  text(tritrafo(c(0.5, 0.2, 0.3)), "11", adj=c(-3,-0.4))
  text(tritrafo(c(0.5, 0.25, 0.25)), "12", adj=c(-1,1))
  
  # Legendas
  # triplot(grid=F, frame=F)
  # legend(x = -0.5, y = 0.5, 
  # 	 legend = c(
  #       expression(paste("1)   ", A[1], " ganha no 1º turno, ", A[2], " em 2º lugar")),
  #       expression(paste("2)   ", A[1], " ganha no 1º turno, ", A[3], " em 2º lugar")),
  #       expression(paste("3)   ", A[2], " ganha no 1º turno, ", A[1], " em 2º lugar")),
  #       expression(paste("4)   ", A[2], " ganha no 1º turno, ", A[3], " em 2º lugar")),
  #       expression(paste("5)   ", A[3], " ganha no 1º turno, ", A[1], " em 2º lugar")),
  #       expression(paste("6)   ", A[3], " ganha no 1º turno, ", A[2], " em 2º lugar")),
  #       expression(paste("7)   ", A[1], " e ", A[2], " no 2º turno, ", A[1], " na frente")) ,
  #       expression(paste("8)   ", A[1], " e ", A[2], " no 2º turno, ", A[2], " na frente")) ,
  #       expression(paste("9)   ", A[1], " e ", A[3], " no 2º turno, ", A[1], " na frente")) ,
  #       expression(paste("10) ", A[1], " e ", A[3], " no 2º turno, ", A[3], " na frente")) ,
  #       expression(paste("11) ", A[2], " e ", A[3], " no 2º turno, ", A[2], " na frente")) ,
  #       expression(paste("12) ", A[2], " e ", A[3], " no 2º turno, ", A[3], " na frente"))  
  #              )
  #       )
}
