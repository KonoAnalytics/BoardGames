dice_roll <- function(numbertoroll, seed=NULL)
{

    sample(1:6,numbertoroll,replace=TRUE)
}

endor_roll <- function(nextsteps,numstormtroopers, seed=NULL)
{
    if(!is.null(seed))
    {
        set.seed(seed)
    }
    roll <- dice_roll(5)
    roll <- roll[order(-roll)]
    finished <- FALSE
    numsteps <- 0
    i <- 0
    while(!finished)
    {
        i <- i + 1
        if(i > min(5,length(nextsteps)))
        {
            finished <- TRUE 
        }
        numsteps <- numsteps + as.numeric((i<=min(5,length(nextsteps))) & (roll[i] >= nextsteps[i] + as.numeric(i <= numstormtroopers)))
    }
    numsteps
}

endor_sumulation <- function(numsims, nextsteps,numstormtroopers)
{
    numsteps <- as.numeric()
    for (i in 1:numsims)
    {
        numsteps <- c(numsteps,endor_roll(nextsteps, numstormtroopers))
    }
    numsteps
}

endor_wholeplanet <- function(planetpath = c(rep(2,5),rep(3,5),rep(4,5), rep(5,5)), numstormtroopers = 9, seed = NULL)
{
    if(!is.null(seed))
    {
        set.seed(seed)
    }
    finished <- FALSE
    numrolls <- 0
    stormtroopers <- numstormtroopers
    stepnumber <- 1
    while(!finished)
    {
        numrolls <- numrolls + 1
        nextsteps <- planetpath[stepnumber:length(planetpath)]
        advance <- endor_roll(nextsteps, stormtroopers)
        stormtroopers <- max(0, stormtroopers-advance)
        stepnumber <- stepnumber + advance
        if(stepnumber > length(planetpath))
        {
            finished <- TRUE
        }
    }
    numrolls
}

endor_wholeplanetsim <- function(numsimulations, numstormtroopers=9)
{
    results <- numeric()
    for(i in 1:numsimulations)
    {
        results <- c(results, endor_wholeplanet(numstormtroopers=numstormtroopers))
    }
    results
}

endor_output <- function(n)
{
    for (i in 2:5)
    {
        nostorm <- endor_sumulation(n,rep(i,5),0)
        storm <- endor_sumulation(n,rep(i,5),5)
        meandiff <- mean(nostorm - storm)
        sddiff <- sd(nostorm - storm)
        print(paste0(i,": average difference= ",meandiff,", sd difference = ",sddiff))
    }
}