# One Way Anova (Completely Randomized Design)
fit <- aov(y ~ A, data=mydataframe) 

# Randomized Block Design (B is the blocking factor) 
fit <- aov(y ~ A + B, data=mydataframe) 

# Two Way Factorial Design 
fit <- aov(y ~ A + B + A:B, data=mydataframe)
fit <- aov(y ~ A*B, data=mydataframe) # same thing.Notice that there are two independent variables in this example, separated by an asterisk *. The asterisk indicates to R that the interaction between the two factors is interesting and should be analyzed. If interactions are not important, replace the asterisk with a plus sign (+)

# Analysis of Covariance 
fit <- aov(y ~ A + x, data=mydataframe) 

# One Within Factor
fit <- aov(y~A+Error(Subject/A),data=mydataframe)

# Two Within Factors W1 W2, Two Between Factors B1 B2 
fit <- aov(y~(W1*W2*B1*B2)+Error(Subject/(W1*W2))+(B1*B2),data=mydataframe) 



