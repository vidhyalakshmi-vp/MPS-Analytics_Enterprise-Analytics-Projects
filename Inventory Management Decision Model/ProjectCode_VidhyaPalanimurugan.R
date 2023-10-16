#PART 1

#Heuristic Decision Modelling

#Defining uncontrollable variable & parameters
annual_demand <- 15000
unit_cost <- 80
order_cost <- 220
annual_carryingCost <- 0.18

#Decision Variables
reorder_thres <- seq(100,1000, by=20)
order_quantity <- 2 * reorder_thres

#Calculating Cost
#Annual Ordering Cost 
annual_orderingCost <- order_cost * (annual_demand/order_quantity)
#Annual Holding Cost
annual_holdingCost <- order_quantity * ((unit_cost*annual_carryingCost)/365)* 
  (order_quantity/(annual_demand/365)) * (annual_demand/order_quantity)
#Total Cost 
total_cost <- annual_orderingCost + annual_holdingCost


# order quantity and total cost
df_qty_cost <- cbind(order_quantity, total_cost)
#View(df_qty_cost)

#Plotting graph
plot(order_quantity, total_cost,
     xlab = "Order Quantity",
     ylab = "Total Inventory Cost (in $)",
     main = "Figure 3: Total Inventory Cost vs Order Quantity")


#Zooming in
reorder_thresh1 <- seq(220,260, by=5)
order_quantity1 <- 2 * reorder_thresh1

#Calculating Cost
#Annual Ordering Cost
annual_OC1 <- order_cost * (annual_demand/order_quantity1)
#Annual Holding Cost
annual_HC1<- order_quantity1 * ((unit_cost*annual_carryingCost)/365)* 
  (order_quantity1/(annual_demand/365)) * (annual_demand/order_quantity1)
#Total Cost
TIC1 <- annual_OC1 + annual_HC1

#Plotting graph
plot(order_quantity1, TIC1,
     xlab = "Order Quantity",
     ylab = "Total Inventory Cost (in $)",
     main = "Figure 4: Total Inventory Cost vs Order Quantity [220-260]",
     col = "red", pch = 19)


which.min(TIC1)
#5 is the position at which minimum cost occurs

reorder_thresh1[which.min(TIC1)]
#240 is the minimum threshold value at which cost is minimum


#Decision model and optimization
#define function to optimize
optimiseCostFun <- function(x){
  annual_D<- 15000
  UC <- 80
  OC <- 220
  annual_CC <- 0.18
  annual_OC2 <- OC * (annual_D/x)
  annual_HC2<- x * ((UC*annual_CC)/365)*
    (x/(annual_D/365)) * (annual_D/x) 
  TIC2 <- annual_OC2 + annual_HC2
}

#using optimise function
order_thresh_interval <- c(100,800)
cost_optimization <- optimise(f= optimiseCostFun,
                              interval = order_thresh_interval,
                              lower = min(order_thresh_interval),
                              upper = max(order_thresh_interval),
                              maximum = FALSE,
                              tol = .Machine$double.eps^0.5)
cost_optimization




#PART II

#Defining uncontrollable variable & parameters
annual_demand <- 15000
unit_cost <- 80
order_cost <- 220
annual_carryingCost <- 0.18

#generate constant random numbers
set.seed(10)


#Setting values for triangular distribution
min_a <- 13000
max_b <- 17000
peak_c <- 15000


#calculating K,M,N
CA <- peak_c-min_a
BA<- max_b-min_a
BC<- max_b-peak_c
K <- CA/BA
M <- BA*CA
N <- BA*BC


#generate random numbers and random demand variables
r_simulation <- runif(1000)
n <- length(r_simulation)

td_x1 <- ifelse(r_simulation <= K,
                round(min_a + sqrt(r_simulation*M), digits = 0),
                round(max_b - sqrt((1-r_simulation)*N)))
random_demand <- cbind(r_simulation,td_x1)

tpd_totalCost <- data.frame(matrix(ncol=5, nrow=0))
colnames(tpd_totalCost) <- c("Random_Number",
                                   "Unit_Demand",
                                   "Order_Quantity",
                                   "Minimum_Cost",
                                   "Annual_Order_Count")

#Using simulation and optimization
for(i in 1:length(td_x1)) {
  cost_func1 <- function(x){
    annual_D<- td_x1[i]
    annual_OC3 <- order_cost * (annual_D/x)
    annual_HC3<- x * ((unit_cost*annual_carryingCost)/365)*
      (x/(annual_D/365)) * (annual_D/x)
    
    TIC3 <- annual_OC3 + annual_HC3
  } 
  
  ord_thresh_interval1<- c(100,8000)
  cost_optimize1 <- optimise(f= cost_func1, interval = ord_thresh_interval1,
                             lower = min(ord_thresh_interval1),
                             upper = max(ord_thresh_interval1),
                             maximum = FALSE,
                             tol = .Machine$double.eps^0.5)
  
  tpd_totalCost[i,"Random_Number"] = r_simulation[i]
  tpd_totalCost[i,"Unit_Demand"] = td_x1[i]
  tpd_totalCost[i,"Order_Quantity"] = round(cost_optimize1$minimum,0)
  tpd_totalCost[i,"Minimum_Cost"] = cost_optimize1$objective
  tpd_totalCost[i,"Annual_Order_Count"] = td_x1[i]/cost_optimize1$minimum
}
#View(tpd_totalCost)


#Part 2.1
#Minimum Cost

x1 <- tpd_totalCost$Minimum_Cost

#expected value of the minimum cost is the mean
exp_min_cost <- mean(x1)
exp_min_cost

#Calculating the confidence interval
#upper limit
ci_upper_x1 <- exp_min_cost + (qnorm(0.975) * sd(x1)/sqrt(n))
ci_upper_x1

#lower limit
ci_lower_x1 <- exp_min_cost - (qnorm(0.975) * sd(x1)/sqrt(n))
ci_lower_x1

#Checking the distribution of the variable
#Method 1 : Formal normality test: Shapiro-Wilk test
swt1 <- shapiro.test(x1)
swt1$p.value > 0.05


#Method 2: Graphical methods: Histogram
bins_x1 <- ceiling(sqrt(length(x1)))
hist(x1,
     breaks = bins_x1,
     main = "Figure 7: Histogram of Minimum Total Cost",
     xlab = "Total Cost (in $)")


#Method 3: Graphical methods:  QQ-Plot chart
qqnorm(x1,main = "Figure 8: Q-Q plot of Minimum Total Cost")
qqline(x1, col="blue", lty=1, lwd=2)


#Part 2.2 
#Order Quantity

x2 <- tpd_totalCost$Order_Quantity

#expected value of the minimum cost is the mean
exp_ord_qty <- round(mean(x2),0)
exp_ord_qty

#Calculating the confidence interval
#upper limit
ci_upper_x2 <- round(exp_ord_qty + (qnorm(0.975) * sd(x2)/sqrt(n)),0)
ci_upper_x2

#lower limit
ci_lower_x2 <- round(exp_ord_qty - (qnorm(0.975) * sd(x2)/sqrt(n)),0)
ci_lower_x2

#Checking the distribution of the variable
#Method 1 : Formal normality test: Shapiro-Wilk test
swt2 <- shapiro.test(x2)
swt2$p.value > 0.05

#Method 2: Graphical methods: Histogram
bins_x2 <- ceiling(sqrt(length(x2)))
hist(x2,
     breaks = bins_x2,
     main = "Figure 9: Histogram of Order Quantity",
     xlab = "Order Quantity")


#Method 3: Graphical methods:  QQ-Plot chart
qqnorm(x2, main = "Figure 10: Q-Q plot of Order Quantity")
qqline(x2, col="blue", lty=1, lwd=2)



#Part 2.3 
#Annual number of orders

x3 <- tpd_totalCost$Annual_Order_Count

#expected value of the minimum cost is the mean
exp_annu_ordno <- round(mean(x3),0)
exp_annu_ordno

#Calculating the confidence interval
#upper limit
ci_upper_x3 <- round(exp_annu_ordno + qnorm(0.975) * sd(x3)/sqrt(n),0)
ci_upper_x3

#lower limit
ci_lower_x3 <- round(exp_annu_ordno - qnorm(0.975) * sd(x3)/sqrt(n),0)
ci_lower_x3

#Checking the distribution of the variable
#Method 1 : Formal normality test: Shapiro-Wilk test
swt3 <- shapiro.test(x3)
swt3$p.value > 0.05


#Method 2: Graphical methods: Histogram
bins_x3 <- ceiling(sqrt(length(x3)))
hist(x3,
     breaks = bins_x3,
     main = "Figure 11: Histogram of Annual number of orders", 
     xlab = "Annual number of orders")


#Method 3: Graphical methods:  QQ-Plot chart
qqnorm(x3, main = "Figure 12: Q-Q plot of Annual number of orders")
qqline(x3, col="blue", lty=1, lwd=2)

