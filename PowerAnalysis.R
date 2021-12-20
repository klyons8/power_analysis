#Apriori Power Analysis that can be modified for different mixed model designs
#By Kathleen Lyons 
#Date: Feb 3, 2021

#'Helpful documents
#'https://www.statmethods.net/stats/power.html
#'https://cran.r-project.org/web/packages/pwr/pwr.pdf
#'https://cran.r-project.org/web/packages/Superpower/vignettes/intro_to_superpower.html
#'https://psyarxiv.com/baxsf/
#'


#'Current break down: Longitudinal study comparing two groups across 4 time points
#'The study is using three measures (not comparing across these measures, but will correct alpha)
#'2 x 4 mixed model ANOVA (2 groups, 4 repeated measure time points)
#'
#'


#'Required packages 
#'First time running on computer - install packages; then you can comment those lines out 

#install.packages('pwr')
library(pwr)
#install.packages("Superpower")
library(Superpower)


### Two Sample T-Test - if you are most interested in the difference between the two groups (collapsed across time, or only last time point)

#Variables you can change
pow = .80 #Your power is set to .8; This means 80% of the time, you will detect your effect if it is real
NumComp = 3 #Number of comparisons you are planning to do - since you have three measures that you will be comparing separately, I corrected alpha for three tests
alpha = .05/NumComp #This is the standard alpha level Bonferroni adjusted for three statistical tests
cohensD = .65 #This is your predicted effect size (cohensD) based on previous literature, small = .2, medium = .5, large = .8)
Tail = c("two.sample") #This is the type of t.test, you can have two.sample, one.sample, or paired

pwr.t.test(n = , d = cohensD , sig.level = alpha , power = pow, type = Tail)


#Mixed model power analysis
#You can play around with the n (number of participants), and the mu and sd (mean and sd) to see how 
#much power you would get for different sizes of effect

Ncontrasts = 3
alpha <- .05/Ncontrasts

#these values give us a cohen's d of .65 (our predicted effect size based on the literature)

means = c(.49, -.49)
stdv = 1.5
Desired_power = 80
p = FALSE
ns = 2

#You can modify this while loop to also vary the xbar1, xbar2, and stdv values to get your desired effect size
while (p == FALSE){
  
  ns = ns + 1

  #This is the design that only compares the two groups
  design_result <- ANOVA_design(
    design = "2b", n = ns,
    mu =  means, sd = stdv,
    labelnames = c("condition",
                   "CA", "IA"),
    plot = TRUE)
  
  
  result_exact <- ANOVA_exact(design_result, alpha_level = alpha)


  
  p = (result_exact$pc_results[1]) > Desired_power
  
  
}

sprintf('To detect a moderate  effect between your two between subject conditions, with %s%% power, You need %s participants in per condition', Desired_power, ns)

#The plot power tells us the number of participants we need each power level
plot_power(design_result,alpha_level = alpha,
           min_n = 10, max_n = 100,
           desired_power = 80, plot = TRUE)





##Within Design (Four time points)
#exact_within - moderate effect size (cohen's d from time 1 to time 4 = .5)
means =  c(1, 0.5, .25, 0)
stdv = 2

Desired_power = 80
p = FALSE
ns = 4

#You can modify this while loop to also vary the x1 and x2 values to get your desired effect size
while (p == FALSE){
  
  ns = ns + 1

  design_within <- ANOVA_design(
    design = "4w", n = ns, mu = means,
    sd = stdv, r = 0.5,
    labelnames = c("condition",
                   "T1",
                   "T2", "T3", "T4"))

  
  #With a corrected p, you would need 92 participants to detect a small change in cognition
  exact_within = ANOVA_exact(design_within, alpha_level  = alpha)
  #Enough power to detect differences across all time points (change p to true)
  #p = any((exact_within$pc_results[1]) < Desired_power) 
  

  p = exact_within$main_results[1] > Desired_power
  
  
}

sprintf('To detect a moderate effect across time with four time points, with %s%% power, You need %s participants at each time point', Desired_power, ns)

plot_power(design_within,alpha_level = alpha,
           min_n = 10, max_n = 100,
           desired_power = 80, plot = TRUE)



#This is the full 2x4 mixed model ANOVA
#One issue is estimating what the interaction effect will be.
#If you don't predict an interaction, then this isn't really an issue

#Based on the literature, we expected a medium effect across time and between groups 
#Currently, cohen's f ~ .3 for condition & time (you can play around with these numbers for a different eff size)
means = c(0, .25, 1, 1.5, 0.5, 1.25, 1.75, 2)
Desired_power = 80
p = TRUE
stdv = 2.2
ns = 8

while (p == TRUE){
  
  ns = ns + 1

  design_result_cross <- ANOVA_design(
    design = "2b*4w", n = ns,
    mu = means, sd = stdv,
    labelnames = c("condition",
                   "CA", "TA",
                   "Time",
                   "T1",
                   "T2", "T3", "T4"))

  result_exact <- ANOVA_exact(design_result_cross, alpha_level  = alpha)
  
  #Enough power to detect both condition and time effects (not interaction)

  p = any(result_exact$main_results[1:2, 1] < Desired_power)
  
  
}

sprintf('To detect both main effect (for moderate effects) in a mixed model ANOVA, with %s%% power, You need %s participants at each time point', Desired_power, ns)


#Plot each 
plot_power(design_result_cross,alpha_level = alpha,
           min_n = 10, max_n = 150,
           desired_power = 80, plot = TRUE)


