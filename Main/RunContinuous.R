source("init.all.R")
init.all()

run <- function(fitFunction = "Weierstrass",indsize = 10,popSize = 25,select = 30,min=(-5),max=5,elitism=0,sample="GaussianSHR") {

newEDA <- new("EDA")

	fitFun = new(fitFunction)
	init = "Continuous"
	popSize = popSize
	indSize = indsize
	selSize = select 
	varMin = c(rep(min,indsize))
	varMax = c(rep(max,indsize))
	elit=elitism
	select = "Truncation"
	learn = new("COVSHRINK")
	samp = sample
	stop <- new("Optimum",optimum=0,epsilon=0.00000001,iterations=10000)
	
configure(newEDA,fitFun,init,popSize,indSize,selSize,elit,varMin,varMax,select,learn,samp,stop)

}

cat(paste("Ejecucion del experimento Vecindad ONE"),"\n", file = "console.txt", sep = " ", fill = FALSE, labels = NULL,
    append = FALSE)
for (i in 1:25) {
  run();
  cat(paste("FIN DE LA CORRIDA ",i),"\n", file = "console.txt", sep = " ", fill = FALSE, labels = NULL,
      append = TRUE)
  print(paste("FIN DE LA CORRIDA ",i))
}