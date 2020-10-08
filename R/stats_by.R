data("mtcars")

# is there a relationship btwn cyl (number of cylinders) and mpg (fuel efficiency)?

table(mtcars$cyl)
stats <- function(x){
  c(n = length(x), mean = mean(x), sd = sd(x))
}

stats(mtcars$mpg)
## if we want to look at these stats by different values of cylinder
cyl_group <- as.factor(mtcars$cyl)
result <- by(mtcars$mpg, cyl_group, stats) ## by() is generalizable but the downside is
                                    ## when you save it, the class of the object is "by"

result_list<- as.list(result)
class(result_list)<-"list"
result_df <- as.data.frame(result_list)

## alt sol
result2 <- aggregate(mpg~cyl, data = mtcars, stats)

## alt sol 2
result3 <- sapply(split(mtcars$mpg, factor(mtcars$cyl)), stats) #split mpg by levels of cyl
class(result3)

stats_by <- function(x, factor){
  stats <- function(x){
    c(n = length(x), mean = mean(x), sd = sd(x))
  }
  result <- sapply(split(x, factor), stats)
  return(result)
}

stats_by(mtcars$mpg, mtcars$cyl)
