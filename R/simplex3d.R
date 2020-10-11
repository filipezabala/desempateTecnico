#' Generates a 2D simplex.
#' @param \code{pa},\code{pb} and \code{pc}. The proportions of votes of candidates A, B and C.
#' @param \code{n} Sample size.
#' @param \code{alpha} Significance level. Default: \code{alpha=0.05}.
#' @import ellipse rgl
#' @examples simplex3d(.4, .3, .3, 1000)
#' simplex3d(.3,.25,.2,.1,.05, 100)
#' simplex3d(rep(1/5,5), 500)
#' simplex3d(.5813972562, .3158114522, .1027912917, 50)
#' simplex3d(.5144202347, .3246860305, .1608937348, 100)
#' simplex3d(.4160925601, .3316216347, .2522858052, 500)
#' simplex3d(.3919345050, .3324785813, .2755869137, 10^3)
#' simplex3d(.3518464606, .3332479566, .3149055828, 10^4)
#' simplex3d(.3391808234, .3333247966, .3274943799, 10^5)
#' simplex3d(.3333333335, .3333333333, .3333333331, 10^20)
simplex3d <- function(pa,pb,pc,n,alpha=0.05){
    
  # Criando elementos para a geração do simplex
    z <- qnorm(1-alpha/2)
    
    vpa <- pa*(1-pa)/n 
    vpb <- pb*(1-pb)/n
    vpc <- pc*(1-pc)/n
    covpapb <- -pa*pb/n
    covpapc <- -pa*pc/n
    covpbpc <- -pb*pc/n
    
    Sab <- matrix( c(vpa, covpapb, covpapb, vpb), nrow=2, ncol=2)
    Sac <- matrix( c(vpa, covpapc, covpapc, vpc), nrow=2, ncol=2)
    Sbc <- matrix( c(vpb, covpbpc, covpbpc, vpc), nrow=2, ncol=2)
    
    eab <- ellipse::ellipse(Sab, centre=c(pa,pb), level=1-alpha)
    eac <- ellipse::ellipse(Sac, centre=c(pa,pc), level=1-alpha)
    ebc <- ellipse::ellipse(Sbc, centre=c(pb,pc), level=1-alpha)
    
    colnames(eab) <- c("pa", "pb")
    colnames(eac) <- c("pa", "pc")
    colnames(ebc) <- c("pb", "pc")
    
    
    ###############
    ### SIMPLEX ###
    ###############
    
    tam <- 26
    pac <- pbc <- pcc <- seq(0,1,length=tam)
    
    # Criando o vetor 'rr' que copia length(pbc) vezes cada ponto de 'pac'
    rr <- vector()
    j <- 1
    for(i in 1:tam^2)
    	{
    	if(i>1 & i %% tam == 1) {j <- j + 1}
    	rr[i] <- pac[j]
    	}
    
    
    # Criando a matriz 'ptc' que combina todos as ternas (rr, pbc, 1-rr-pbc)
    ptc <- cbind(rr, pbc, 1-rr-pbc)
    
    
    # Criando a matriz 'nptc', que conterá apenas as ternas de 'ptc' cuja soma é igual a 1
    j <- 0
    nptc <- matrix(0 , nrow=dim(ptc)[1], ncol=dim(ptc)[2])
    for(i in 1:dim(ptc)[1])
    	{
    	if(ptc[i,3]>0)
    		{
    		j <- j + 1
    		nptc[j,1] <- ptc[i,1]
    		nptc[j,2] <- ptc[i,2]
    		nptc[j,3] <- ptc[i,3]
    		}
    	}
    
    # Contando quantos pares de 'nptc' são 'NA'
    cont <- 0 
    for(i in 1:dim(nptc)[1])
    	{
    	if(is.na(nptc[i,1]) == TRUE) {cont <- cont+1}
    	}
    
    # Atribuindo 'nptc' a 'simplex' apenas com os valores não 'NA'
    simplex <- nptc[1:(dim(nptc)[1] - cont), ]
    colnames(simplex) <- c("pa", "pb", "pc")
    
    
    #########################
    ### Retas dos empates ###
    #########################
    
    # Aqui serão construídos os lugares geométricos no simplex que levam a empates duplos
    
    rpa <- matrix(c(1,0,0, 0,.5,.5), nrow=2, ncol=3, byrow=T)
    rpb <- matrix(c(0,1,0, .5,0,.5), nrow=2, ncol=3, byrow=T)
    rpc <- matrix(c(0,0,1, .5,.5,0), nrow=2, ncol=3, byrow=T)
    
    mpa <- matrix(c(.5,0,.5, .5,.5,0), nrow=2, ncol=3, byrow=T)
    mpb <- matrix(c(0,.5,.5, .5,.5,0), nrow=2, ncol=3, byrow=T)
    mpc <- matrix(c(.5,0,.5, 0,.5,.5), nrow=2, ncol=3, byrow=T)
    
    l1 <- matrix(c(1,0,0, 0,1,0), nrow=2, ncol=3, byrow=T)
    l2 <- matrix(c(0,1,0, 0,0,1), nrow=2, ncol=3, byrow=T)
    l3 <- matrix(c(0,0,1, 1,0,0), nrow=2, ncol=3, byrow=T)
    
    colnames(rpa) <- c("pa", "pb", "pc")
    colnames(rpb) <- c("pa", "pb", "pc")
    colnames(rpc) <- c("pa", "pb", "pc")
    
    colnames(mpa) <- c("pa", "pb", "pc")
    colnames(mpb) <- c("pa", "pb", "pc")
    colnames(mpc) <- c("pa", "pb", "pc")
    
    
    
    ########################################
    ### Elipsóide de confiança 3D 1-alpha ###
    ########################################
    
    eabc <- cbind(eab, 1-(eab[,1]+eab[,2]))
    colnames(eabc) <- c("pa", "pb", "pc")
    
    
    ######################################
    ### Elipses de confiança 2D 1-alpha ###
    ######################################
    
    # Atribuindo um vetor de zeros ao eixo faltante
    
    eabn <- cbind(eab, rep(0, dim(eab)[1]))
    eacn <- cbind(eac[,1], rep(0, dim(eac)[1]), eac[,2])
    ebcn <- cbind(rep(0, dim(ebc)[1]), ebc)
    
    colnames(eabn) <- c("pa", "pb", "pc")
    colnames(eacn) <- c("pa", "pb", "pc")
    colnames(ebcn) <- c("pa", "pb", "pc")
    
    
    ###############################
    ### Plotando os gráficos 3D ###
    ###############################
    
    rgl::plot3d(simplex, type="p", xlim=c(0,1), ylim=c(0,1), zlim=c(0,1), top=F)
    rgl::plot3d(l1, type="l", col="black", add=T)
    rgl::plot3d(l2, type="l", col="black", add=T)
    rgl::plot3d(l3, type="l", col="black", add=T)
    rgl::plot3d(rpa, type="l", col="red", add=T)
    rgl::plot3d(rpb, type="l", col="red", add=T)
    rgl::plot3d(rpc, type="l", col="red", add=T)
    rgl::plot3d(mpa, type="l", col="green3", add=T)
    rgl::plot3d(mpb, type="l", col="green3", add=T)
    rgl::plot3d(mpc, type="l", col="green3", add=T)
    
    rgl::plot3d(eabc, type="l", col="blue", add=T)
} 
