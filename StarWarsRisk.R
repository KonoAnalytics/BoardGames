hrug <- function(numsims=100000, seed=42)
{
    if(is.null(seed))
    {
        seed <- runif(1) * 10000
    }
    set.seed(seed)
    
    cat("Early or Late?\n")
    endor_earlyorlate(numsims, fileandpath="c:/temp/earlylate.png", seed)
    
    cat("\nStormtroopers or not?\n")
    endor_stormornot(numsims, fileandpath="c:/temp/stormornot.png", seed)   
}

dice_roll <- function(numbertoroll, seed=NULL)
{
    if(is.null(seed))
    {
        seed <- runif(1) * 10000
    }
    set.seed(seed)

    sample(1:6,numbertoroll,replace=TRUE)
}

endor_roll <- function(nextsteps,numstormtroopers, seed=NULL)
{
    if(is.null(seed))
    {
        seed <- runif(1) * 10000
    }
    set.seed(seed)
    
    roll <- dice_roll(5)
    roll <- roll[order(-roll)]
    finished <- FALSE
    numsteps <- 0
    i <- 0
    while(!finished)
    {
        seed <- seed + 1
        i <- i + 1
        if(i > min(5,length(nextsteps)))
        {
            finished <- TRUE 
        }
        numsteps <- numsteps + as.numeric((i<=min(5,length(nextsteps))) & (roll[i] >= nextsteps[i] + as.numeric(i <= numstormtroopers)))
    }
    numsteps
}

endor_simulation <- function(numsims, nextsteps, numstormtroopers, seed=NULL)
{
    if(is.null(seed))
    {
        seed <- runif(1) * 10000
    }
    set.seed(seed)
    
    numsteps <- as.numeric()
    for (i in 1:numsims)
    {
        seed <- seed + 1
        numsteps <- c(numsteps,endor_roll(nextsteps, numstormtroopers, seed))
    }
    numsteps
}

endor_wholeplanet <- function(planetpath = c(rep(2,5),rep(3,5),rep(4,5), rep(5,5)), numstormtroopers = 9, seed=NULL)
{
    if(is.null(seed))
    {
        seed <- runif(1) * 10000
    }
    set.seed(seed)
    
    finished <- FALSE
    numrolls <- 0
    stormtroopers <- numstormtroopers
    stepnumber <- 1
    while(!finished)
    {
        seed <- seed + 1
        numrolls <- numrolls + 1
        nextsteps <- planetpath[stepnumber:length(planetpath)]
        advance <- endor_roll(nextsteps, stormtroopers,seed)
        stormtroopers <- max(0, stormtroopers-advance)
        stepnumber <- stepnumber + advance
        if(stepnumber > length(planetpath))
        {
            finished <- TRUE
        }
    }
    numrolls
}

endor_wholeplanetsim <- function(numsimulations, numstormtroopers=9, seed=NULL)
{
    if(is.null(seed))
    {
        seed <- runif(1) * 10000
    }

    set.seed(seed)
    
    results <- numeric()
    for(i in 1:numsimulations)
    {
        seed <- seed + 1
        results <- c(results, endor_wholeplanet(numstormtroopers=numstormtroopers, seed=NULL))
    }
    results
}

endor_earlyorlate <- function(n, fileandpath="c:/temp/earlylate.png", seed=NULL)
{
    library(ggplot2)

    if(is.null(seed))
    {
        seed <- runif(1) * 10000
    }
    set.seed(seed)
    
    begintime <- Sys.time()
    cat(paste0("\n","Beginning Time: ",begintime,"\n\n"))
    dfhist <- data.frame(spaces=numeric(), storm=character(), stage=character())
    for (i in 2:5)
    {
        seed <- seed + 1
        nostorm <- endor_simulation(n,rep(i,5),0, seed=seed)
        dfhist <- rbind(dfhist,data.frame(spaces=nostorm, storm="without",stage=paste("stage",i-1)))
        storm <- endor_simulation(n,rep(i,5),5)
        dfhist <- rbind(dfhist,data.frame(spaces=storm, storm="with",stage=paste("stage",i-1)))
        meandiff <- mean(nostorm) - mean(storm)
        cat(paste0("Stage ",i-1," with = ",round(mean(storm),3),", without = ",round(mean(nostorm),3),", average difference = ",round(meandiff,3),"\n"))
    }
    h <- ggplot2::ggplot(dfhist, aes(spaces,fill=storm)) + geom_histogram(alpha=.5, bins = 6) + facet_wrap(stage~storm, ncol=2)
    ggsave(fileandpath,h, width=8, height=8, units="in")
    
    endtime <- Sys.time()
    cat("\n")
    cat(paste0("Ending Time: ",endtime,"\n"))
    print(endtime-begintime)
    cat("\n")
}

endor_stormornot <- function(n, fileandpath="c:/temp/stormornot.png", seed=NULL)
{
    library(ggplot2)

    if(is.null(seed))
    {
        seed <- runif(1) * 10000
    }
    set.seed(seed)
    
    begintime <- Sys.time()
    cat(paste0("\nBeginning Time: ",begintime,"\n\n"))
    storm <- endor_wholeplanetsim(n,9)
    nostorm <- endor_wholeplanetsim(n,0)
    dfhist <- data.frame(rolls=numeric(), storm=character())
    dfhist <- rbind(dfhist,data.frame(rolls=nostorm, storm="without"))
    dfhist <- rbind(dfhist,data.frame(rolls=storm, storm="with"))
    h <- ggplot2::ggplot(dfhist, aes(rolls,fill=storm)) + geom_histogram(alpha=.5, binwidth = 1) + facet_wrap(~storm, ncol=2) + xlim(1,max(dfhist$rolls)+1)
    ggsave(fileandpath,h, width=8, height=8, units="in")
    
    cat(paste0("with = ",round(mean(storm),3),", without = ",round(mean(nostorm),3),", difference = ",round(mean(storm)-mean(nostorm),3),"\n"))
    endtime <- Sys.time()
    cat("\n")
    cat(paste0("Ending Time: ",endtime,"\n"))
    print(endtime-begintime)
    cat("\n")
}
