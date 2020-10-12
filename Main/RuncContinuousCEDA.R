source("init.all.R")
init.all()
run <- function(fitFunction = "ShiftedSchwefelProblem1.2Noise",indsize = 10,popSize = 25,select = 30,min=(-100),max=100,elitism=1,sample="GaussianSHR") {
  
  newcEDA <- new("cEDA")
  
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
  stop <- new("Optimum",optimum=(-450),epsilon=0.00000001,iterations=100000)
  
  #Config the cellular EDA
  lattice <-
    new(
      "LatticeConfiguration", totalRows = 5, totalCols = 5, totalRowsPerCell =
        2,
      totalColsPerCell = 2
    )
  
  neighbor = 1;
  if(neighbor==0){
    #one
    xradio <- array(c(0))
    yradio <- array(c(0))  
  }
  else if(neighbor==1){  
    #l5
    xradio <- array(c(0,0,0,-1,1))
    yradio <- array(c(0,-1,1,0,0))
  }
  else if(neighbor==2){   
    #c13
    xradio <- array(c(0,0,0,0,0,-1,-1,-2,1,2,1,-1,1))
    yradio <- array(c(0,1,2,-1,-2,1,0,0,0,0,1,-1,-1))
  }
  else if(neighbor==3){   
    #c25
    
    xradio <- array(c(0,0,0,0,0,0,0,-1,-1,-1,-2,-3,1,2,3,-2,1,-1,-1,-2,1,2,1,1,2))
    yradio <- array(c(0,1,2,3,-1,-2,-3,1,2,0,0,0,0,0,0,1,1,-1,-2,-1,-1,-1,-2,2,1))
  }
  else if(neighbor==4){   
    #c41
    xradio <- array(c(0,0,0,0,0,0,0,-1,-1,-1,-2,-3,1,2,3,-2,1,-1,-1,-2,1,2,1,1,2,0,0,1,1,2,2,3,3,4,-1,-1,-2,-2,-3,-3,-4))
    yradio <- array(c(0,1,2,3,-1,-2,-3,1,2,0,0,0,0,0,0,1,1,-1,-2,-1,-1,-1,-2,2,1,4,-4,3,-3,2,-2,1,-1,0,3,-3,-2,2,-1,1,0))
  }else if(neighbor==5){  
    #l9
    xradio <- array(c(0,0,0,0,0,-2,-1,1,2))
    yradio <- array(c(0,-1,-2,1,2,0,0,0,0))
  }else if(neighbor==6){   
    #c9
    xradio <- array(c(0,0,0,-1,-1,1,1,-1,1))
    yradio <- array(c(0,1,-1,1,0,0,1,-1,-1))
  }
  
  neighborhood <- new("Neighborhood", xradio = xradio,yradio = yradio)
  config(
    newcEDA,fitFun,init,lattice, neighborhood,indSize,selSize,elit,varMin,varMax,select,learn,samp, stop
  )
  
}

cat(paste("Ejecucion del experimento Vecindad ONE"),"\n", file = "console.txt", sep = " ", fill = FALSE, labels = NULL,
    append = FALSE)
for (i in 1:25) {
  run();
  cat(paste("FIN DE LA CORRIDA ",i),"\n", file = "console.txt", sep = " ", fill = FALSE, labels = NULL,
      append = TRUE)
  print(paste("FIN DE LA CORRIDA ",i))
}