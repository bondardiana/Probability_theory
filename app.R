#FOR UPLOADING TO SERVER
#~~~~~~~~~~~~~
#install.packages('rsconnect')
#library(rsconnect)
#rsconnect::setAccountInfo(name='bondardiana', token='my_token', secret='my_secret')
#rsconnect::deployApp('/Users/Diana_Bondar/Desktop/shiny/probability_theory/')





getmode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
datos = read.csv('avocado.csv')
avocado_data_length =length(datos)
mode_avocado_prices = getmode(datos$AveragePrice)
avocado_prices_q1= as.numeric(quantile(datos$AveragePrice)[2])
avocado_prices_q2= as.numeric(quantile(datos$AveragePrice)[3])
avocado_prices_q3= as.numeric(quantile(datos$AveragePrice)[4])

datosd = read.csv('diabetes.csv')
datosd = subset(datosd, Glucose>0 & Insulin>0 & Age>0 & BMI>0 & BloodPressure>0 & SkinThickness>0)
#~~~~~~~~~~~~~
# TEXTS
#~~~~~~~~~~~~~
avocado_dataset_description_text = "This is a sample dataset.The table below represents weekly 2018 retail scan data for National retail volume (units) and price."
sample_is_description_text = "Aim of statistics is to get information on a total collection of elements, called population. Since it is almost always impossible to obtain complete information about the population (for example, we want to know how many patients with coronavirus, but physically we cannot carry out 8 billion tests), we can try to learn about the population by choosing and then examining a subgroup of its elements called a sample (i.e., a subset) from a total population."
sample_is_description_detail_text = p(em("    'Weekly 2018 retail scan data for National retail' - "), strong("sample"))
population_is_description_text = p("    Avocado prices of all units of time and peoples - ", strong("population"))

avocado_prices_description_text = p("The subject of our research will be the prices of avocados ", strong("(AveragePrice column)"))


library(shiny)
runGitHub( "Probability_theory_shiny_r", "bondardiana")
library("ggplot2")
library("e1071")
library(corrplot)
library(RColorBrewer)
library("BSDA")
library(reactlog)
library(e1071) 
library("PearsonDS")
library(spatstat)
library(RColorBrewer)
library(expm)

ui <- fluidPage(

    navlistPanel( "Select chapter", 
                  tabPanel("Basics of probability", tabsetPanel(
                      tabPanel("Probability space", br(),
                               p(h4("Probability space")),
                               
                               p("Probability space is the triple ", strong("(Q, F, P)")," which provides a model of a random process or 'experiment'"),  
                               hr(),p(h4("Elementary event")),
                               p("Elementary event – event which contains a single outcome in the event space."),
                               hr(),
                               p(h4("Sample space")),
                               p(withMathJax("$$\\Omega $$")),
                               p("Set of all possible outcomes/results of the experiment"),
                               br(),
                               p(h5("Properties")),
                               p("•	Elementary events are indecomposable and pairwise disjoint"),
                               p("•	Elementary event has probability [0; 1] "),
                               p("•	Impossible events have zero probability, certain events has probability one"),
                               p("•	Sum of all probabilities of elementary events from sample space is equal to one "),
                               br(),
                               p(h5("Example")),
                               p("Experiment: we roll dice two times"),
                               p(" Sample space – all possible combinations ((1;1), (1;2) … (6;5), (6;6)) , set of 36 combinations"),
                               p("Elementary event – any of this combination"),
                               p("Event – get 7 as a sum or get at least one ‘one’"),
                                hr(),
                               p(h4("Event family")),
                               p(withMathJax("$${\\mathcal {F}}$$")),
                               p("Event family is a family of subsets Q called events (an event being a set of outcomes in the Q)"),
                               p("An element of F is called an event"),
                               p(withMathJax("$$\\mbox{ , }{\\displaystyle {\\mathcal {F}}\\subseteq 2^{\\Omega }}$$")),
                               p(withMathJax("$${\\mathcal {F}}\\mbox{ contains the sample space:}{\\displaystyle \\Omega \\in {\\mathcal {F}}},$$")),
                               p(withMathJax("$${\\mathcal {F}}\\mbox{ is closed under complements: if }A\\in\\mathcal{F}\\mbox{, then also }{\\displaystyle (\\Omega \\setminus A)\\in {\\mathcal {F}}},$$")),
                               hr(), p(h4("Probability function")),
                               p(withMathJax("$$P$$")),
                               p(withMathJax("$${\\displaystyle P:{\\mathcal {F}}\\to [0,1]} \\mbox{ is a function that assigns each event in the event space a probability, which is a number between 0 and 1.}$$")),
                               p(withMathJax("$$\\mbox{The measure of the entire sample space is equal to one: }{\\displaystyle P(\\Omega )=1}.$$")),
                               br(),
                               p("Example: rolling a dice twice"),
                               p("Sample space – all possible combinations ((1;1), (1;2) … (6;5), (6;6))"),
                               p("Event family –  all subsets of Q  i.e., event : get 7 points or event : get at least one ‘one’"),
                               p("Probability of event from F P((i; j)) = 1/36"),
                               p("Probability of event from F P(i; j) such as i+j = 3 is equal to 2/36")),
                      tabPanel("Classical and geometric probability",
                               br(), p(h4("Classical probability")),
                               p("Classical probability is a simple form of probability that has equal odds of something happening with a finite number of outcomes."),
                               p("Obtained with the formula:"),
                               p(withMathJax("$${\\displaystyle P(A)={\\frac {(\\mbox{number in outcomes in A})}{\\mbox{(total number of outcomes in }\\Omega)}}}$$")),
                               br(),
                               p("Example"),
                               p("Get value < 3 after throwing a dice. There are six total possible outcomes and two of them are less than 3. P(v>2) = 2/6 = 1/3"),
                               hr(),
                               br(), p(h4("Geometric probability")),
                               p("Geometric probability is a tool to deal with the problem of infinite outcomes by measuring the number of outcomes geometrically, in terms of length, area, or volume."),
                               p("Obtained with the formula:"),
                               p(withMathJax("$${\\displaystyle P(A)={\\frac {\\mu(A)}{\\mu(\\Omega)}}}$$")),
                               p("where μ is a measure on Ω (length, area, volume)"),
                               br(),
                               p("Example"),
                               p("Let's imagine that we are a equilateral triangle. We divide it into four equal regular triangles. Then the probability of falling outside the triangle in the middle P(A) = 3/4")
                               
                               ),
                      tabPanel("Combinatorial analysis",
                               br(),
                               p(h4("Combinations")),
                               p(withMathJax("$${C\\binom{n}{k}}$$")),
                               p("Combination is a selection k of items from a collection with size = n, such that the order of selection does not matter."),
                               p(withMathJax("$${C\\binom{n}{k}}={\\frac {n(n-1)\\dotsb (n-k+1)}{k(k-1)\\dotsb 1}}=\\textstyle {\\frac {n!}{k!(n-k)!}} $$")),
                               br(),
                               p("Example"),
                               p("We have n = 5 books, only k = 3 books fit in the bag. How many ways to choose?"),
                               p(withMathJax("$${C\\binom{5}{3}} = \\textstyle {\\frac {5!}{3!(5-3)!}} = 10$$")),
                               br(), hr(),
                               
                               p(h4("Permutations")),
                               p(withMathJax("$${P_{n}}$$")),
                               p("Permutations is a selection n of items from a collection with size = n, such that the order of selection matters. "),
                               p(withMathJax("$${P_{n}}=n*(n-1)*..*1={n!}$$")),
                               br(),
                               p("Example"),
                               p("We have n = 5 books. How many ways to queue for them to read? (all n=5)"),
                               p(withMathJax("$${P_{n}} = {5!} = 120$$")),
                               br(), hr(),
                               
                               p(h4("Allocation")),
                               p(withMathJax("$${A\\binom{n}{k}}$$")),
                               p("Allocation is a selection k of items from a collection with size = n, such that the order of selection matters!)"),
                               p("Allocation is a combitations*permitations."),
                               p(withMathJax("$${A\\binom{n}{k}}={n(n-1)\\dotsb (n-k+1)}=\\textstyle {\\frac {n!}{(n-k)!}} $$")),
                               br(),
                               p("Example"),
                               p("We have n = 5 books, only k = 3 books fit in the bag. How many ways to prioritize reading from all the combinations that we have chosen?"),
                               p(withMathJax("$${A\\binom{5}{3}} = \\textstyle {\\frac {5!}{(5-3)!}} = 60$$")),
                               br(), hr(),
                               p(h4("Permutations with repetitions")),
                               p(withMathJax("$${P(n_{1},..,n_{k})}$$")),
                               
                               p("Permutations with repetitions is a selection n of items from a collection with size = n, such that the order of selection matters. Having n1,..nk number of repeats of different elements. "),
                               p(withMathJax("$${P_{n}}=n*(n-1)*..*1={n!}/{n_{1}!*..*n_{k}!}$$")),
                               br(),
                               p("Example"),
                               p("We have n = 5 books. How many ways to queue for them to read? (all n=5) if we have two same 'Alice in Wonderland' copies and three same 'Abetka'?"),
                               p(withMathJax("$${P_{n}}=n*(n-1)*..*1={n!} = {5!}/{3!*2!} = 10$$"))
                               
                               
                               ),
                      tabPanel("Probability axioms",
                               br(),
                               p(strong("- Non-negativity."), "The probability of an event is a non-negative real number:"),
                               p(withMathJax("$${\\displaystyle P(E)\\in \\mathbb {R} ,P(E)\\geq 0\\qquad \\forall E\\in F}$$")),
                               hr(),p(strong("- Norming."), "The probability that at least one of the elementary events in the entire sample space will occur is 1"),
                               p(withMathJax("$$P(\\Omega) = 1$$")),
                               hr(),p(strong("- σ-additivity.")," Any countable sequence of disjoint sets satisfies"),
                               p(withMathJax("$${\\displaystyle E_{1},E_{2},\\ldots }$$")),
                               hr(), p(strong("-  Monotonicity"),"If A is a subset of, or equal to B, then the probability of A is less than, or equal to the probability of B"),
                               p(withMathJax("$$\\quad {\\text{if}}\\quad A\\subseteq B\\quad {\\text{then}}\\quad P(A)\\leq P(B)$$")),
                               hr(),p(strong("- The probability of the empty set"),"Probability of the ∅ is equal to 0."),
                               p(withMathJax("$$P(\\varnothing )=0$$")),
                               hr(),p(strong("- The complement rule"),""),
                               p(withMathJax("$${\\displaystyle P\\left(A^{c}\\right)=P(\\Omega \\setminus A)=1-P(A)}$$")),
                               hr(),p(strong("- Sum rule"),"The probability that A or B will happen is the sum of the probabilities that A will happen and that B will happen, minus the probability that both A and B will happen"),
                               p(withMathJax("$$P(A\\cup B)=P(A)+P(B)-P(A\\cap B)$$")))
                  )),
                  tabPanel("Discrete random variable",  tabsetPanel(
                      tabPanel("Random variable (descrete)",
                               hr(),
                               p(h4("Random variable")),
                               p(br(), "A ",strong("random variable"),  "ξ on a probability space (Ω, F , P) is a measurable ",strong("function"), " ξ: Ω→R defined on a probability space that maps from the sample space to the real numbers."),
                               p("A random variable X is discrete if it assumes only finite or countable infinite set of values x1, x2, . . ."),
                               p(h5("Example",style="font-family:math"), h5("The possible outcomes for one coin toss can be described by the sample space Ω = {H, T}. We can introduce a real-valued random variable Y that models a $1 payoff for a successful bet on heads as follows:",style="font-family:math")),
                               img(src = "discrete_random_variable.png"),
                               hr(),
                    
                               p(h4(br(), "Probability mass function")),
                               p(br(),"Probability mass function is the probability distribution of a discrete random variable, that gives the probability that a discrete r.v. is exactly equal to some value."),
                               p("It is the function p : R → [0,1],  -∞ < x < ∞ ,defined by:"),
                               p(withMathJax("$${\\displaystyle p_{X}(x_{i})=P(X=x_{i})}$$")),
                               p("The probabilities associated with each possible value must be positive and sum up to 1. For all other values, the probabilities need to be 0."),
                               h6(p(withMathJax("$${\\displaystyle \\sum p_{X}(x_{i})=1}$$"), withMathJax("$${\\displaystyle p(x_{i})>0}$$"))),
                               h5(p("p(x) = 0 for all other x"),style = "text-align:center;font-family:serif"),
                               p(h5("Example", style="font-family:math")),
                               p(h5("If the coin is a fair coin, Y has a probability mass function on ",em("F"),"y given by:", style="font-family:math")),
                               img(src = "probability_mass_function.png"),
                               hr(), br(),
                               p(h4("Cumulative distribution function")),
                               br(),
                               p("The cumulative distribution function (CDF) of a real-valued random variable X, evaluated at x, is the probability that X will take a value less than or equal to x."),
                               p(withMathJax("$${\\displaystyle F_{X}(x)=\\operatorname {P} (X\\leq x)}$$")),
                               p("Properties of CDF (for discrete r.v.)"),
                               p(withMathJax("$${\\displaystyle \\lim _{x\\to -\\infty }F_{X}(x)=0,\\quad \\lim _{x\\to +\\infty }F_{X}(x)=1.}$$")),
                               p(withMathJax("$${\\displaystyle F_{X}(x)=\\operatorname {P} (X\\leq x)=\\sum _{x_{i}\\leq x}\\operatorname {P} (X=x_{i})=\\sum _{x_{i}\\leq x}p(x_{i})}$$")),
                               p(h5("Example", style="font-family:math")),
                               p(h5("If the coin is a fair coin, X has a cumulative distribution function on ",em("F"),"x given by:", style="font-family:math")),
                               img(src = "cumulative_distribution_function.png")
                               
                      ),
                      tabPanel("Binomial distribution",
                               br(),
                               p(h4("Bernoulli distribution")),
                               br(),
                               p("A random variable X has the Bernoulli distribution if it takes only two values: 1 and 0 with probabilities p and q = 1 − p respectively"),
                               p("It is considered that the event X = 1 corresponds to 'success', and the event X = 0 - to 'failure'. These names are conditional, and depending on the specific task, they can be replaced with the opposite ones."),
                               p("The Bernoulli distribution is a special case of the binomial distribution with n = 1"),
                               p(h5("Example", style="font-family:math")),
                               p(h5("If the coin is not a fair coin with probability to get 1 = p and probability 1-p to get 0", style="font-family:math")),
                               img(src = "bernoulli.png"),
                                hr(), br(),
                               p(h4("Binomial distribution")),
                               br(),
                               p("Binomial distribution B (n, p) - distribution of the total number of 'successes' in a sequence of n independent random experiments such that the probability of 'success' in each of them is constant and equal to p."),
                               p(" 0 ≤ p ≤ 1 ; 0 ≤n"),
                               withMathJax("$$P[X=k]={n\\choose k}p^k(1-p)^{n-k}\\quad\\mbox{for}\\ k=0,1,2,\\dots,n$$"),
                               splitLayout(
                               sliderInput("binomial_distribution_attempts", "attemps", 0, 100, 50),
                               sliderInput("binomial_distribution_p_of_succes", "probability % of success", 0, 100, 50)),
                               splitLayout(
                               plotOutput("binomial_distribution"),
                               plotOutput("binomial_distribution_cdf")),
                               splitLayout(
                                   htmlOutput('Binomial_mean'),
                                   htmlOutput('Binomial_var')),
                               br(), hr(), 
                               h4(p("R - code")),
                               br(),
                               p(code("x <- seq(0, size, by = 1)  #Create a vector of values X. Since the distribution is discrete, its length is equal to size (n)")),
                               p(code("y <- dbinom(x, size, p))  #Create the probability mass function")),
                               p(code("y2 <- pbinom(x, size, p)) #Create the cumulative distribution function")),
                               p(code("y3 <- rbinom(n, size, prob) #Generate random deviates")),
                               p(code("plot(x, y)"))),
                      
                      tabPanel("Geometric distribution",
                               br(),
                               p(h4("Geometric distribution")),
                               br(),
                               p("Geometric distribution is a distribution of X Bernoulli trials until the first success happen"),
                               p("The geometric distribution gives the probability that the first occurrence of success requires k independent trials, each with success probability p. If the probability of success on each trial is p, then the probability that the kth trial (out of k trials) is the first success is"),
                               p("Probability mass function:"),
                               p(withMathJax("$${\\displaystyle \\Pr(X=k)=(1-p)^{k-1}p} \\quad\\mbox{for}\\ k=0,1,2\\dots$$")),
                               br(),
                               p(h5("Example", style="font-family:math")),
                               p(h5("The probability of having a girl (success) is p= 0.5 and the probability of having a boy (failure) is q = 1 − p = 0.5.", style="font-family:math")),
                               img(src = "geometric.png", width="330" ,height="150"),
                               splitLayout(
                                   sliderInput("geometric_distribution_attempts", "attemps", 0, 100, 50),
                                   sliderInput("geometric_distribution_p_of_succes", "probability % of succes", 0, 100, 50)),
                               splitLayout(
                               plotOutput("geometric_distribution"),
                               plotOutput("geometric_distribution_cdf")),
                               splitLayout(
                               htmlOutput('Geometric_mean'),
                               htmlOutput('Geometric_var')),
                               br(), hr(),
                               h4(p("R - code")),
                               br(),
                               p(code("x <- seq(0, size, by = 1)  #Create a vector of values X. Since the distribution is discrete, its length is equal to size (n)")),
                               p(code("y <- dgeom(x, p))  #Create the probability mass function")),
                               p(code("y2 <- pgeom(x, p)) #Create the cumulative distribution function")),
                               p(code("y3 <- rgeom(n, prob) #Generate random deviates")),
                               p(code("plot(x, y)"))),
                      
                      tabPanel("Poisson distribution",
                               br(),
                               p(h4("Poisson distribution")),
                               br(),
                               p("Poisson distribution is a discrete probability distribution that expresses the probability of a given number of events occurring in a fixed interval of time or space if these events occur with a known constant mean rate and independently of the time since the last event."),
                               p("The Poisson r.v. X is the limit of binomial r.v.’s when n · p → λ as n → ∞"),
                               p("The Poisson distribution can be applied to systems with a large number of possible events, each of which is rare (big n, small p). The number of such events that occur during a fixed time interval is, under the right circumstances, a random number with a Poisson distribution."),
                               br(),
                               p(h5("Example", style="font-family:math")),
                               img(src = "poisson.png", width="600" ,height="80"),
                               br(),
                               p("Probability mass function:"),
                               p(withMathJax("$$\\!f(k;\\lambda )=\\Pr(X=k)={\\frac {\\lambda ^{k}e^{-\\lambda }}{k!}},$$")),
                               splitLayout(
                               sliderInput("poisson_distribution_p_of_succes", "lambda", 0, 100, 10),
                               sliderInput("poisson_distribution_attempts", "attemps", 0, 100, 50)),
                               splitLayout(
                               plotOutput("poisson_distribution"),
                               plotOutput("poisson_distribution_cdf")),
                               splitLayout(
                                   htmlOutput('Poison_mean'),
                                   htmlOutput('Poison_var')
                               ),
                               br(), hr(),
                               h4(p("Poisson vs Binomial")),
                               br(),
                               splitLayout(
                                   sliderInput("poisson_vc_Binomial_p_of_succes", "probability of succes", 0, 100, 10),
                                   sliderInput("poisson_vc_Binomial_attempts", "attemps", 0, 100, 50)),
                               plotOutput("Poisson_vc_Binomial"),
                               br(), hr(),
                               h4(p("R - code")),
                               br(),
                               p(code("x <- seq(0, size, by = 1)  #Create a vector of values X. Since the distribution is discrete, its length is equal to size (n)")),
                               p(code("y <- dpois(x, lambda))  #Create the probability mass function")),
                               p(code("y2 <- dgeom(x, lambda)) #Create the cumulative distribution function")),
                               p(code("y3 <- rgeom(n, lambda) #Generate random deviates")),
                               p(code("plot(x, y)"))
                               )
                      
                    
                  )),
                  tabPanel("Variance and moments", tabsetPanel(
                      tabPanel("Expected value",
                               br(),
                               p(h4("Expected value")),
                               br(),
                               p("Let X be a random variable with a finite number of finite outcomes x1, x2,…, xk occurring with probabilities p1, p2,…, pk, The expectation of X is defined as"),
                               p(withMathJax("$${\\displaystyle \\operatorname {E} [X]=\\sum _{i=1}^{k}x_{i}\\,p_{i}=x_{1}p_{1}+x_{2}p_{2}+\\cdots +x_{k}p_{k}.}$$")),
                               p("The expected value is also known as the ",strong("expectation, mathematical expectation, mean, average, or first moment.")),
                               br(), hr(),
                               p(h5("Example",style="font-family:math"), h5("An illustration of the convergence of sequence averages of rolls of a die to the expected value of 3.5 as the number of rolls",style="font-family:math")),
                               splitLayout(
                               radioButtons("dice_n_rolls", "Number of rolls",
                                            choices = c(1, 10, 100, 1000, 10000),
                                            selected = 100),
                               radioButtons("dice_plot_type_rolls", "Type of plot",
                                            choices = c("line", "points","both", "histogram"),
                                            selected = "points"),
                               radioButtons("dice_biased", "dice fairness", choices=(c("fair", "biased, P(X=1)=1/2")), selected ="fair")),
                               splitLayout(
                               plotOutput("dice_valuesplot"),
                               plotOutput("dice_meansplot")),
                               br(), hr(),
                               p(h4("Properties of the expectation")),
                               br(),
                               p(strong(" - Non-negativity"),": If X ≥ 0, then E[X]≥0"),
                               p(strong(" - Linearity of expectation:"),". For any r.v. X and Y, and a constant a"),
                               p(withMathJax("$${\\displaystyle {\\begin{aligned}\\operatorname {E} [X+Y]&=\\operatorname {E} [X]+\\operatorname {E} [Y],\\operatorname {E} [aX]&=a\\operatorname {E} [X],\\end{aligned}}}$$")),
                               p(strong(" - CDF"),"If F(x) is the cumulative distribution function of the probability measure P, and X is a r.v."),
                               p("$${\\displaystyle \\operatorname {E} [X]=\\int _{\\overline {\\mathbb {R} }}x\\,dF(x) = \\int \\limits _{0}^{\\infty }(1-F(x))\\,dx-\\int \\limits _{-\\infty }^{0}F(x)\\,dx,}$$"),
                                p(strong(" - Monotonicity:")," If X ≤ Y almost surely (happens with probability = 1 ) and both E[X] and E[Y] exist , then  E[X] ≤ E[Y]"),
                                p(strong(" - Non-multiplicativity:")," If X and Y are independent, than E[XY] = E[X]*E[Y]. If the random variables are dependent, then generally E[XY] ≠ E[X]*E[Y],  although in special cases of dependency the equality may hold. "),
                                 br(), hr(),
                                 p(h4("Type of means")),
                                 br(),
                      p(h5("Arithmetic mean:")),
                      p("The arithmetic mean (or simply mean) of a list of numbers, is the sum of all of the numbers divided by the amount of numbers"),
                      p(withMathJax("$${\\displaystyle A={\\frac {1}{n}}\\sum _{i=1}^{n}a_{i}={\\frac {a_{1}+a_{2}+\\cdots +a_{n}}{n}}}$$")),
                    p("For example, the arithmetic mean of five values: 4, 36, 45, 50, 75 is:"),
                       p(withMathJax("$$:{\\displaystyle {\\frac {4+36+45+50+75}{5}}={\\frac {210}{5}}=42.}$$")),
                                p(h5("Weighted arithmetic mean:")),
                    p("Instead of each data point contributing equally to the final mean, some data points contribute more “weight” than others. If all the weights are equal, then the weighted mean is the same as the arithmetic mean"),
                                p(withMathJax("$${\\bar {x}}={\\frac {\\sum \\limits _{i=1}^{n}w_{i}x_{i}}{\\sum \\limits _{i=1}^{n}w_{i}}},{\\bar {x}}={\\frac {w_{1}x_{1}+w_{2}x_{2}+\\cdots +w_{n}x_{n}}{w_{1}+w_{2}+\\cdots +w_{n}}}.$$")),
                                p("Given two school classes, one with 20 students, and one with 30 students.The mean for the morning class is 80 and the mean of the afternoon class is 90. The unweighted mean of the 80 and 90 is 85, so the unweighted mean of the two means is 85. And weighted mean:"),
                      p(withMathJax("$${\\bar {x}}={\\frac {(20\\times 80)+(30\\times 90)}{20+30}}=86.$$")),
                    p(h5("Expected value:")),
                    p("The expected value of a random variable is a generalization of the weighted average."),
                    p(withMathJax("$${\\displaystyle \\operatorname {E} [X]=\\sum _{i=1}^{k}x_{i}\\,p_{i}=x_{1}p_{1}+x_{2}p_{2}+\\cdots +x_{k}p_{k}.}$$")),
                    p("Since the sum of all probabilities pi is 1 (p1+p2+⋯+pk=1, the expected value is the weighted sum of the xi values, with the pi values being the weights."),
                    p("Let X represent the outcome of a roll of a fair six-sided die"),
                    p(withMathJax("$${\\displaystyle \\operatorname {E} [X]=1\\cdot {\\frac {1}{6}}+2\\cdot {\\frac {1}{6}}+3\\cdot {\\frac {1}{6}}+4\\cdot {\\frac {1}{6}}+5\\cdot {\\frac {1}{6}}+6\\cdot {\\frac {1}{6}}=3.5.}$$")),
                    br(), hr(), 
                    h4(p("R - code")),
                    br(),
                    p(code("x <- seq(1, 6, by = 1)  #Generate a sequance from 1 to 6")),
                    p(code("> x")),
                    p(code("[1] 1 2 3 4 5 6")),
                    p(code("> mean(x) #Find it's mean")),
                    p(code("[1] 3.5"))),
                    tabPanel("Variance",
                             br(),
                             p(h4("Variance")),
                             br(),
                             p("Variance is the expectation of the squared deviation of a random variable from its mean μ = E[X]. Informally, it measures how far a set of numbers is spread out from their average value."),
                            p("The variance is the square of the standard deviation, the second central moment of a distribution. Represented by:"),
                            p(withMathJax("$$\\operatorname {Var} (X), \\mbox{ }\\sigma ^{2} \\mbox{ for population, }s^{2}\\mbox{ for samples}$$")),
                            p(withMathJax("$$\\operatorname {Var} (X)=\\operatorname {E} \\left[(X-\\mu )^{2}\\right]$$")),
                    p(withMathJax("$${\\displaystyle {\\begin{aligned}\\operatorname {Var} (X)&=\\operatorname {E} \\left[(X-\\operatorname {E} [X])^{2}\\right]=\\operatorname {E} \\left[X^{2}-2X\\operatorname {E} [X]+\\operatorname {E} [X]^{2}\\right]=\\operatorname {E} \\left[X^{2}\\right]-2\\operatorname {E} [X]\\operatorname {E} [X]+\\operatorname {E} [X]^{2}=\\operatorname {E} \\left[X^{2}\\right]-\\operatorname {E} [X]^{2}\\end{aligned}}}$$"))
                   ,br(), hr(),
                   p(h4("Properties of the variance")),
                   br(),
                   p(strong(" - Non-negativity"),": Var(X) ≥ 0"),
                   p("The variance of a constant is zero. Var(a) = 0. If the variance of a random variable is 0, then it is a constant"),
                   p("Variance is invariant with respect to changes in a location parameter Var(a+X) = Var(X)"),
                   p(withMathJax("$$\\operatorname {Var} (aX)=a^{2}\\operatorname {Var} (X)$$")),
                   p("The variance of a sum of two random variables"),
                   p(withMathJax("$$\\operatorname {Var} (aX+bY)=a^{2}\\operatorname {Var} (X)+b^{2}\\operatorname {Var} (Y)+2ab\\,\\operatorname {Cov} (X,Y)$$")),
                    p(withMathJax("$$\\operatorname {Var} (aX-bY)=a^{2}\\operatorname {Var} (X)+b^{2}\\operatorname {Var} (Y)-2ab\\,\\operatorname {Cov} (X,Y)$$")),
                   br(), hr(), 
                   h4(p("R - simulation")),
                   br(),
                   p(code("x1 = sample(seq(-10, 10), 50, replace = TRUE) #Generate 50 variables in range (-10, 10) (red)")),
                   p(code("x2 = sample(seq(-50, 50), 50, replace = TRUE) #Generate 50 variables in range (-50, 50) (blue)")),
                   p(code("x3 = sample(seq(-80, 80), 50, replace = TRUE)) #Generate 50 variables in range (-80, 80) (green)")),
                   p(code("hist(x1) #visualisation of a sample")),
                   p(code("sd(x1) #function to get standard deviation of a sample")),
                   p(code("var(x1)  #function to get variance of a sample")),
                   br(),
                   p("We have thee samples of different ranges (x1, x2, x3) with almost equal means. Now we can see what the different option looks like."),
                   plotOutput("variance_example")
                      ),
                   
                   
                   tabPanel("Covariance and Correlation",
                    br(), hr(), p(h4("Covariance")), br(),  
                    p("Covariance is a measure of the joint variability of two random variables.If the greater values of one variable mainly corresponds with the greater values of the other variable, and the same holds for the lesser values, the covariance is positive. In the opposite case, when the greater values of one variable mainly correspond to the lesser values of the other, the covariance is negative. The sign of the covariance shows the tendency in the linear relationship between the variables (positive sign - positive relationships, negative - negative)."),
                    p("For two jointly distributed real-valued random variables X and Y, the covariance is defined as the expected value (or mean) of the product of their deviations from their individual expected values"),
                    p("Usually denoted as σXY or σ(X, Y)"),
                    p(withMathJax("$${\\displaystyle {\\begin{aligned}\\operatorname {cov} (X,Y)&=\\operatorname {E} \\left[\\left(X-\\operatorname {E} \\left[X\\right]\\right)\\left(Y-\\operatorname {E} \\left[Y\\right]\\right)\\right]\\&=\\operatorname {E} \\left[XY-X\\operatorname {E} \\left[Y\\right]-\\operatorname {E} \\left[X\\right]Y+\\operatorname {E} \\left[X\\right]\\operatorname {E} \\left[Y\\right]\\right]\\&=\\operatorname {E} \\left[XY\\right]-\\operatorname {E} \\left[X\\right]\\operatorname {E} \\left[Y\\right]-\\operatorname {E} \\left[X\\right]\\operatorname {E} \\left[Y\\right]+\\operatorname {E} \\left[X\\right]\\operatorname {E} \\left[Y\\right]\\&=\\operatorname {E} \\left[XY\\right]-\\operatorname {E} \\left[X\\right]\\operatorname {E} \\left[Y\\right]\\end{aligned}}}$$")),
                    p(h5("Properties")),
                    br(),
                    p("The variance is a special case of the covariance in which the two variables are identical"),
                    p(withMathJax("$${\\displaystyle \\operatorname {cov} (X,X)=\\operatorname {var} (X)\\equiv \\sigma ^{2}(X)\\equiv \\sigma _{X}^{2}}$$")),
                    p("For real-valued r.v X, Y and a random constant a:"),
                    p("Cov(X, a) = 0"),
                    p("Cov(X, Y) = Cov(Y, X)"),
                    p("Cov(aX, bY) = abCov(X, Y)"),
                    p("Cov(X+a, Y+b) = Cov(X, Y)"),
                    p("If X and Y are independent random variables, then their covariance is zero. E[XY] = E[X]*E[Y]"),
                    
                    br(), hr(), p(h4("Correlation")), br(),
                    p("Correlation is a statistical relationship between two random variables, refers to the degree to which a pair of variables are linearly related"),
                    p("It is obtained by taking the ratio of the covariance of the two variables from our numerical dataset, normalized to the square root of their variances in the open interval (-1,1)."),
                    p(withMathJax("$${\\displaystyle \\rho _{X,Y}=\\operatorname {corr} (X,Y)={\\operatorname {cov} (X,Y) \\over \\sigma _{X}\\sigma _{Y}}={\\operatorname {E} [(X-\\mu _{X})(Y-\\mu _{Y})] \\over \\sigma _{X}\\sigma _{Y}}}$$")),
                    p(withMathJax("$${\\displaystyle \\rho _{X,Y}={\\operatorname {E} (XY)-\\operatorname {E} (X)\\operatorname {E} (Y) \\over {\\sqrt {\\operatorname {E} (X^{2})-\\operatorname {E} (X)^{2}}}\\cdot {\\sqrt {\\operatorname {E} (Y^{2})-\\operatorname {E} (Y)^{2}}}}}$$")),
                    p(h5("Properties")),
                    p("Corr(X, Y) = Corr(Y, X)"),
                    p("The correlation coefficient is +1 in the case of a perfect direct linear relationship and −1 in the case of a perfect inverse linear relationship (anticorrelation) ", strong("Y =a*X + b")),
                    img(src="correlation2.png"),
                    p("X and Y can not independent, for example, when Y = X*X, but dependent."),
                     br(), hr(), p(h4("Examples")), br(), 
                    splitLayout(
                        verticalLayout(
                   p("no correlation"),
                   p(code("x = rnorm(1000)")),
                     p(code("y = rnorm(1000)")),
                     p(code("plot(x, y) + abline(lm(y~x))")),
                   plotOutput("zero_correlation_example")),
                   verticalLayout(
                   p("perfect correlation"),
                   p(code("x = rnorm(1000)")),
                   p(code("y = x*05 + 11")),
                   p(code("plot(x, y) + abline(lm(y~x))")),
                   plotOutput("perfect_corelation_example")),
                   verticalLayout(
                   p("perfect negative corelaion"),
                   p(code("x = rnorm(1000)")),
                   p(code("y = -x * 1.3 ")),
                   p(code("plot(x, yy + abline(lm(y~x))")),
                   plotOutput("perfect_negative_corelation_example"))),
                   br(),hr(), 
                   p(h4("Real data example")),
                   br(),
                   
                    p("The table below represents a dataset. It contains various medical records of people."),
                    p(a(href="https://www.kaggle.com/johndasilva/diabetes", "By this link you can examine data more detail")),
                   br(),
                    DT::dataTableOutput("diabetes_data"),
                   br(),
                   p("Let's explore the relationships between these variables. This can be done either by selecting two variables or by looking at the correlation matrix of all variables."),
                   p(strong("Correlation matrix")),
                   p("A correlation matrix is a table showing correlation coefficients between variables. Each cell in the table shows the correlation between two variables. "),
                   p("They are very useful as you can see the whole range of dependencies / independences at once!"),
                   br(),
                  splitLayout(      
                      verticalLayout(
                   selectInput("diabetes_x", "first variable (x)", list("Glucose", "Insulin","Age","BMI", "BloodPressure","SkinThickness", "Pregnancies", "DiabetesPedigreeFunction")),br(), br(), br()),
                   verticalLayout(selectInput("diabetes_y", "second variable (y)", list("Glucose", "Insulin","Age","BMI", "BloodPressure","SkinThickness", "Pregnancies", "DiabetesPedigreeFunction")),br(), br(), br())
                  ),
                  
                  splitLayout(
                   plotOutput("diabetes_corelation_user"), plotOutput("corelation_table")),
                  textOutput("diabetes_corelation_user_res"),
                  br(), hr(), 
                  h4(p("R - code")),
                  br(),
                  p(code("data = read.csv('diabetes.csv') # after downloading data from sourse and putting it in a local repository")),
                  p(code("glu = data$Glucose # extract the Glucose column")),
                  p(code("ins = data$Insulin # extract the Insulin column")),
                  p(code("cor(glu, ins) # to get the correlation of Glucose and Insulin")),
                  p(code("cov(glu, ins) # to get the covariarion of Glucose and Insulin")),
                  p(code("cor(data) # to get the correlation table of the all data")))
                  
                  , tabPanel("Moments", 
                             br(),
                             p(h4("Moment, raw moment")),
                             p("The kth moment of X is:"),
                             p(withMathJax("$${\\mu _{k}=\\frac {1}{n}}\\sum _{i=1}^{n}X_{i}^{k}$$")),
                             p("First moment is a mean."),
                             br(),
                             p(h4("Central moment")),
                             p("The kth central moment of X is:"),
                             p(withMathJax("$$\\sigma _{k}=\\operatorname {E} \\left[(X-\\operatorname {E} [X])^{k}\\right]$$")),
                             p("The 0th c.m. is 1. The 1st c.m is 0. The 2nd c.m. is called variance."),
                             br(),
                             p(h4("Factorial moment")),
                             p("The kth factorial moment is:"),
                             p(withMathJax("$$\\operatorname {E}{\\bigl [}(X)_{r}{\\bigr ]}=\\operatorname {E}{\\bigl [}X(X-1)(X-2)\\cdots (X-r+1)]$$")),
                             br(),
                             p(h4("Standardised moment")),
                             p("The normalized k-th central moment or standardised moment is the k-th central moment divided by σk"),
                             p("The kth standardised moment is:"),
                             p(withMathJax("$${\\displaystyle {\\frac {\\mu _{n}}{\\sigma ^{n}}}={\\frac {\\operatorname {E} \\left[(X-\\mu )^{n}\\right]}{\\sigma ^{n}}}}$$")),
                             p("The 3rd standardised moment is a skewness. The 4th standardised moment is a kurtosis. These two parameters are properties of the distributions that will be studied later."),
                             br(),
                             img(src="moments.png"),
                             br(), br(),
                             hr(),
                             p("R-simulation"),
                             p(code("library('moments') #package moments (need to be installed)")),
                             p(code("x = sample(-100, 100) #generate sample")),
                             p(code("all.moments(x,  order.max = 4, central = FALSE, absolute = FALSE) # Print first 4 raw moments of x")),
                             p(code("all.moments(x,  order.max = 5, central = TRUE, absolute = FALSE) # Print first 5 central moments of x"))
                             
                             ))),
                  tabPanel("Continuous random variable", tabsetPanel(
                      tabPanel("Continuous random variable",
                               br(),
                               p(h4("Continuous random variable")),
                               p("Formally, a continuous random variable is a random variable whose cumulative distribution function is continuous everywhere.There are no 'gaps', which would correspond to numbers that have a finite probability of occurring. Instead, continuous random variables almost never take an exactly prescribed value c ∀c∈R : Pr(X=c)=0 but there is a positive probability that its value will lie in particular intervals which can be arbitrarily small."),
                               p("Example"),
                               p("Let's say we think of any positive number and an infinite number of decimal places. If we fall into the interval from a to b, we get a reward of 1 / (a-b). If we don't get it, we get zero. Continuous random variable X represents this experiment."),
                               img(src="c_r_x.png"),
                               br(),hr(),
                               p(h4("Probability density function")),
                               p("The distribution density of a one-dimensional continuous random variable ξ is a numerical function f (x), the ratio f (x1) / f (x2)
values of which at points x1 and x2 set the ratio of the probabilities of hitting the value ξ in narrow intervals of equal width [x1, x1 + Δx] and [x2, x2 + Δх] near these points. The distribution density is non-negative for any x and is normalized"),
                               p(withMathJax("$${\\displaystyle \\int _{-\\infty }^{+\\infty }f(x)\\,{\\mbox{d}}x=1}$$")),
                               p("As x tends to ± ∞, the function f (x) tends to zero"),
                               p("The dimension of the distribution density is always inverse to the dimension of a random variable - if ξ is calculated in meters, then the dimension off will be m-1."),
                               p(withMathJax("$${\\displaystyle P(\\xi \\in [a,b])=\\int _{a}^{b}f(x)\\,{\\mbox{d}}x}$$")),
                               p("Example"),
                               p("We think of a number (it can be with an infinite number of decimal places) between a and b. Then the probability density is determined by the formula."),
                               img("src"="pdf.png"),
                               br(), hr(),
                               p(h4("C.D.F.")), br(),
                               p(withMathJax("$${\\displaystyle F_{X}(b)-F_{X}(a)=\\operatorname {P} (a<X\\leq b)=\\int _{a}^{b}f_{X}(x)\\,dx}$$")),
                               p("The function fx is equal to the derivative of Fx almost everywhere, and it is called the probability density function of the distribution of X")
                               ),
                      tabPanel("Uniform distribution", 
                               br(),
                               p(h4("Uniform distribution")),
                               br(),
                               p("The continuous uniform distribution U(a, b) is a family of symmetric probability distributions. The distribution describes an experiment where there is an arbitrary outcome that lies between certain bounds.The bounds are defined by the parameters, a and b, which are the minimum and maximum values."),
                               p("The probability density function:"),
                               p(withMathJax("$$f(x)={\\begin{cases}{\\frac {1}{b-a}}&\\mathrm {for} \\ a\\leq x\\leq b\\mathrm \\ \\ {and}\\ 0\\ \\ { for} \\ x<a\\ \\mathrm {or} \\ x>b\\end{cases}}$$")),
                                hr(), p(h4("Example")), br(),
                               p("On the left side we set Unifrom distribution with parameters a = min , b = max and visualize it on the given range of X."),
                               p("On the right side we generate n samples for the adjusted distribution. We can see, the more values are generated - the more the right side fits the left."),
                               splitLayout(
                               sliderInput("uniform_min", "a (from)", -100, 100, -4, step = 4),
                               sliderInput("uniform_max", "b (to)", -100, 100, 4, step = 4),
                               sliderInput("uniform_range", "x range", -100, 100, value =c(-10,20))),
                              splitLayout(
                                  verticalLayout(
                                plotOutput("uniform_distribution"),
                               plotOutput("uniform_distribution_cdf")
                               ),
                               verticalLayout(
                                   radioButtons("uniform_n", "number of generated values", c(10,50,100,500,1000,5000,10000), 100),
                                   plotOutput("uniform_simulation"),
                                   htmlOutput('uniform_mean'),
                                   htmlOutput('uniform_var'))),
                              br(), hr(), 
                               p(h4('R- simulation')),
                              br(), 
                              p(code("x = runif(n = n, min = min, max = max) # simulate n r.v from  X ~ U(min, max)")),
                              p(code("plot(x)"))
                               ),
                      
                      tabPanel("Exponential distribution",
                               br(),
                               p(h4("Exponential distribution")),
                               br(),
                               p("The exponential distribution is the probability distribution of the time between events in a Poisson point process, i.e., a process in which events occur continuously and independently at a constant average rate = λ"),
                               br(),
                               p("A Poisson point process is characterized via the Poisson distribution. Сollection of random points in some space forms a Poisson process, then the number of points in a region of finite size is a random variable with a Poisson distribution. It will be examined more detail later."),
                               p("Exponential distribution is a special case of a Gamma distribution"),
                               p("The probability density function:"),
                               p(withMathJax("$$ f(x;\\lambda) = \\begin{cases}\\lambda e^{-\\lambda x} \\ \\ { for} \\ \\  x \\ge 0, & \\ {0} \\ \\ { for} \\ \\ x < 0.\\end{cases}$$")),
                               hr(), p(h4("Example")), br(),
                               p("On the left side we set Unifrom distribution with parameters a = min , b = max and visualize it on the given range of X."),
                               p("On the right side we generate n samples for the adjusted distribution. We can see, the more values are generated - the more the right side fits the left."),
                               
                               splitLayout(
                               sliderInput("exponential_l", "lambda", 0, 10, 1, step = 0.01),
                               sliderInput("exponential_range", "range of X", -10, 100, c(0,50)))
                               ,
                               splitLayout(
                                   verticalLayout(
                               plotOutput("exponential_distribution"),
                               plotOutput("exponential_distribution_cdf")),
                               verticalLayout(
                                   radioButtons("exponential_n", "number of generated values", c(10,50,100,500,1000,5000,10000), 100 ),
                                   plotOutput("exponential_simulation"),
                                   htmlOutput('exponential_mean'),
                                   htmlOutput('exponential_var')
                                   
                               )),
                               br(), hr(), 
                               p(h4('R- simulation')),
                               br(), 
                               p(code("x = rexp(n = n, rate = l) # simulate n r.v from  X ~ Exp(λ)")),
                               p(code("plot(x)"))
                               ),
                      tabPanel("Normal distribution",
                      
                      br(),
                      p(h4("Normal distribution")),
                      br(),
                      p("Normal distribution N(μ,σ) is a type of continuous probability distribution for a real-valued random variable. "),
                      p("The parameter μ is the mean or expectation of the distribution (and also its median and mode), while the parameter σ is its standard deviation, squared σ - variance."),
                      p("A normal distribution is sometimes informally called a ", strong("bell curve")),
                      p("The general form of the probability density function:"),
                      p(withMathJax("$${\\displaystyle f(x)={\\frac {1}{\\sigma {\\sqrt {2\\pi }}}}e^{-{\\frac {1}{2}}\\left({\\frac {x-\\mu }{\\sigma }}\\right)^{2}}}$$")),
                      p("Probability density function for the standard normal form (special case, when  μ=0 and σ=1)"),
                      p(withMathJax("$${\\displaystyle \\varphi (x)={\\frac {1}{\\sqrt {2\\pi }}}e^{-{\\frac {1}{2}}x^{2}}}$$")),
                      hr(), p(h4("Example")), br(),
                      p("On the left side we set Unifrom distribution with parameters a = min , b = max and visualize it on the given range of X."),
                      p("On the right side we generate n samples for the adjusted distribution. We can see, the more values are generated - the more the right side fits the left."),
                      
                      splitLayout(
                          sliderInput("normal_mean", "mean", -50, 50, 0),
                          sliderInput("normal_sd", "standard_deviation", 0, 100, 1))
                      ,
                      splitLayout(
                          verticalLayout(
                              plotOutput("normal_distribution"),
                              plotOutput("normal_distribution_cdf")),
                          verticalLayout(
                              radioButtons("normal_n", "number of generated values", c(10,50,100,500,1000,5000,10000), 100 ),
                              plotOutput("normal_simulation"),
                              htmlOutput('normal_mean'),
                              htmlOutput('normal_variance')
                              
                          )),
                      br(), hr(), 
                      p(h4('R- simulation')),
                      br(), 
                      p(code("x = rnorm(n = n, mean= mean, sd= sd) # simulate n r.v from  X ~ Exp(λ)")),
                      p(code("plot(x)")))
                  )),
                  tabPanel("Law of Large Numbers and Central Limit Theorem", tabsetPanel(
                      
                      tabPanel("Markov_inequality",
                               br(),
                               p(h4("Markov_inequality")), br(),
                               p("Markov's inequality gives an upper bound for the probability that a non-negative function of a random variable is greater than or equal to some positive constant."),
                               p("If X is a nonnegative random variable and a > 0, then the probability that X is at least a is at most the expectation of X divided by a."),
                               p("$${\\displaystyle \\operatorname {P} (X\\geq a)\\leq {\\frac {\\operatorname {E} (X)}{a}}}$$"),
                               br(), hr(),
                               p(h5("Visualisation")),
                               p("Two plots below represent inequality. Black line is a P(X ≥ a) or (1 - c.d.f.) Red line is an E(X)/a."), br(),
                               splitLayout(
                                 verticalLayout(
                               p("Markov inequality VS Poisson distribution with rate = lambda"),
                               br(),
                               sliderInput("Markov_l", "lambda", 0, 100, 10),
                               plotOutput("markov_inequality1")),
                               verticalLayout(
                               p("Markov inequality VS Binomial distribution with probability of success = p"),
                               sliderInput("Markov_p", "p", 0, 1, step =0.01, value = 0.5),
                               plotOutput("markov_inequality2")
                               
                               ),
                               radioButtons("Markov_scale", "x limit", c(1,5,10,50,100,500), selected = 50))
                               
                      ),tabPanel("Chebyshev’s inequality",
                                 br(),
                                 p(h4("Chebyshev’s inequality")),
                                 p("Let X (integrable) be a random variable with finite expected value μ and finite non-zero variance σ2. Then for any real number k > 0:"),
                                 p(withMathJax("$$\\Pr(|X-\\mu |\\geq k\\sigma )\\leq {\\frac {1}{k^{2}}}$$")),
                                 p("Chebyshev's inequality is about confidence interval for mu +- sigma * sqrt(k). But it is weaker than  68–95–99.7 rule."),
                                  p("A table for determinate % of the standard deviations of the mean for different k:"),
                                 img(src = 'Chebishev.png'),
                                 p("As an example, using k= sqrt(2) shows that the probability that values lie outside the interval (μ−2σ,μ+2σ) does not exceed 1/2"),
                                 
                                 br(),
                                 p(h4("Chebyshev’s one-sided inequality")),
                                 p("If X is a r.v. with mean μ and variance σ2. Then for any real number k > 0:"),
                                 p(withMathJax("$$\\Pr(X-\\mu \\geq k )\\leq {\\frac {\\sigma}{\\sigma+k^{2}}}$$"))
                      ),
                      tabPanel("LLN", 
                               br(), p(h4("Weak Law of large numbers")),
                               p("The sample average converges in probability towards the expected value"),
                               p("Let X1, X2, . . . be independent identically distributed r.v.’s with mean μ. For every ε > 0, we have, as n → ∞"),
                               p(withMathJax("$$\\lim _{n\\to \\infty }\\Pr \\!\\left(\\,|{\\overline {X}}_{n}-\\mu |>\\varepsilon \\,\\right)=0$$")),
                               br(), p(h5("Example")),
                               splitLayout(
                               radioButtons("wlln_n", "number of elements in sample", choices = c(10,50, 100, 500, 1000, 5000, 10000), selected = 10),
                               verticalLayout(
                               sliderInput("wlln_mean", "mean", -100, 100, 20),
                               sliderInput("wlln_var", "variance", 0, 100, 20))),
                               
                               splitLayout(
                               plotOutput("wlln_plot"),
                               plotOutput("wlln_plot2")), br(),
                               textOutput("wlln_t"),
                               
                      br(), hr(), p(h4("Strong Law of large numbers")),
                      p("The sample average converges almost surely to the expected value"),
                      p(withMathJax("$$\\Pr \\!\\left(\\lim _{n\\to \\infty }{\\overline {X}}_{n}=\\mu \\right)=1.$$"))
                      
                  ))),
                  tabPanel("Markov Chain", tabsetPanel(
                      tabPanel("Markov Chain",
                               br(),
                               p(h4("Markov chain")),
                               p("Markov chain is a sequence of random events with a finite number of outcomes, where the probability of each event occurring depends on the state achieved in the previous event"),
                               p(withMathJax("$${\\displaystyle \\Pr(X_{n+1}=x\\mid X_{1}=x_{1},X_{2}=x_{2},\\ldots ,X_{n}=x_{n})=\\Pr(X_{n+1}=x\\mid X_{n}=x_{n})} $$")),
                               p(strong("Markov property , memorylessness- "), " conditional on the present state of the system, its future and past states are independent. It is a process for which predictions can be made regarding future outcomes based solely on its present state.")
                               
                               ),
                      tabPanel("Classification of states",
                               br(),
                               p(h4("Accessible")),
                               p("State j is accessible from i if, for some n, Pij(n) > 0. If it is not impossible to get from i and to j."),
                               hr(), p(h4("Recurrent")),
                               p("State i is recurrent if it is accessible from every j ∈ A(i). If we can if it is possible to return from all the states that we can leave."),
                               hr(), p(h4("Transient")),
                               p("State i is transient if it is accessible not from every j ∈ A(i). If we can if it is possible to return from not all the states that we can leave."),
                               hr(), p(h4("Absorbing")),
                               p("State i is absorbing if it Pjj = 1. The state, from which we can never be left."),
                               br(),
                               p("For example, j is an accessible state from i. If j is absorbing, i is transient. If j is not absorbing and no other accessible from i states are not absorbing - i is a recurrent state.")
                      ),
                      tabPanel("Transition probabilities",
                               br(),
                               p(h4("Transition probabilities")),
                               p("N-stage transition probability that a system presently in state i will be in state j after two steps"),
                               p(withMathJax("$$\\left(P ^{n}\\right)_{i,j}.$$")),
                               
                               p(h4("Large n limit")),
                               p(withMathJax("$$\\lim_{k\\rightarrow\\infty}\\left(P^k \\right)_{i,j}=\\boldsymbol{\\pi}_j,$$")),
                               br(), hr(), 
                               p(h4("Examples")),
                               p("On the examples below se can see that no matter what values are set, positions acquire an equilibrium over time"),
                               br(), hr(), 
                               p(h4("Example 1")),
                               p("This example randomly generates a table of probabilities. We can choose the initial state and see how the equilibrium will be formed. The results were calculated by raising the product of the original vector with the probability matrix to the required power."),
                               
                               br(),
                               splitLayout(
                                 verticalLayout(
                                   splitLayout(
                                   radioButtons("markov_initial_state", "initial_state", c(1,2,3,4) , 1),
                                   actionButton("gobutton", "Rebuilt")),
                                   tableOutput("markov_plot2_table"),
                                   sliderInput("markov_plot2_n", "get info about n-stage =", 0, 20, 20)),
                               plotOutput("markov_plot2")
                               ),
                               textOutput("markov_plot2_info"),
                               textOutput("markov_plot2_limiting"),
            
                               br(), hr(),
                               p(h4("R-simulation")),
                               p(code("tmat <- matrix(rnorm(16)^2, ncol=n) #generate random matrix with size = n")),
                               p(code("tmat <- tmat/rowSums(tmat) #normalaze it to row sums = 1")),
                               p(code("v = c(0,0,0,1) # set initial vector fourth vector")),
                               p(code("y  = array(dim = c(21,4)) #set an array of future values on each time interval")),
                               p(code("for (i in 1:20){")),
                               p(code("y[i, ] = matrix(v, ncol=4) %*% (tmat() %^% i)} # make a matrixes multiplications")),
                               p(code("plot(y[,1] # visualize a first state probability")),
                                    
                               
                               br(), hr(), 
                               p(h4("Example 2")),
                               br(),
                               p("In this example, we set the initial values for x and y, the probability of getting from one state to another, the simulation number."),
                               p("In state x, at each iteration, there will be the number of units coming from the state multiplied by the probability of arrival plus the number of units in state x multiplied by the probability of staying in it."),
                               fluidPage(splitLayout(
                                 sliderInput("markov_plot_initial_x", "initial x", 0, 1000, 100),
                                 sliderInput("markov_plot_initial_y", "initial y", 0, 1000, 100)
                               )),
                               fluidPage(splitLayout(
                                 sliderInput("markov_p_from_x_to_y", "probability to get from x to y", 0, 1, 0.5, step=0.01),
                                 sliderInput("markov_p_from_y_to_x", "probability to get from y to x", 0, 1, 0.5, step=0.01),
                                 sliderInput("makov_n_of_simulations", "number of simulations simulations", 0, 100, 10)
                               )),
                               plotOutput("markov_plot"),
                               
                               br(), hr(),
                               p(h4("R-simulation")), br(),
                               p(code("a_x = c(x) #list for n-stages for x state")),
                               p(code("a_y = c(y) #list for n-stages for y state")),
                               p(code("for (i in 1:n){")),
                               p(code("new_x = (1-p_from_x_to_y)*x + p_from_y_to_x*y")),
                               p(code("new_y = p_from_x_to_y*x + y*(1-p_from_y_to_x)")),
                               p(code("a_x= append(a_x, new_x)")),
                               p(code("a_y = append(a_y, new_y)")),
                               p(code("x = new_x")),
                               p(code("y = new_y}"))
          
                               )
                  )),
                  tabPanel("Bernoulli and Poisson processes", tabsetPanel(
                      
                      tabPanel("Bernoulli process",
                               br(),  p(h4("Bernoulli process")),
                               p("A Bernoulli process is a finite or infinite sequence of independent random variables X1, X2, X3, ..., such that for each i, the value of Xi is either 0 or 1"),
                               p("In other words, a Bernoulli process is a sequence of independent identically distributed Bernoulli trials."),
                                br(), 
                               p(h5("Properties")),
                               p('- Independense."For every n, the process Xn+1, Xn+2, . . . is a Bernoulli process identically distributed with X1, X2, . . . .This future process is independent of the past (X1 , . . . , Xn '),
                               p("- Yk is the time of the kth arrival (Y0 = 0)"),
                               p("- Tk := Yk − Yk−1 is the kth interarrival time"),
                               p("- T1 is a Geometric distribution."),
                               br(), hr(), 
                               p(h4("Negative binomial distribution")),
                               p("Negative binomial distribution X ~ NB(r,p) is a discrete probability distribution that models the number of failures in a sequence of independent and identically distributed Bernoulli trials before k number of success occurs."),
                               p("Geometric distribution is a special case on a Negative binomial distribution with number of successes = 1."),
                               p("Binomial distribution is about total numer of success distribution, Negative Binomial distribution is about kth number of succes time distribution."),
                               p("Probability mass function"),
                               p(withMathJax("$${\\displaystyle f(n;k,p)\\equiv \\Pr(X=n)={\\binom {n+k-1}{k}}p^{n}(1-p)^{n}}$$")),
                               br(), p(h5("Example")),
                               splitLayout(
                                 sliderInput("binomial_negative_n", "target numer of success", 0, 100, 50),
                                 sliderInput("binomial_negative_p", "probability % of success", 0, 100, 50)),
                               splitLayout(
                                 plotOutput("binomial_negative"),
                                 plotOutput("binomial_negative_cdf")),
                               splitLayout(
                                 htmlOutput('binomial_negative_mean'),
                                 htmlOutput('binomial_negative_var')),
                               br(), hr(), 
                               h4(p("R - code")),
                               br(),
                               p(code("x <- seq(0, size, by = 1)  #Create a vector of values X. Since the distribution is discrete, its length is equal to size (n)")),
                               p(code("y <- dnbinom(x, size, p))  #Create the probability mass function")),
                               p(code("y2 <- pnbinom(x, size, p)) #Create the cumulative distribution function")),
                               p(code("y3 <- rnbinom(n, size, prob) #Generate random deviates")),
                               p(code("plot(x, y)")), br(), hr(), 
                               p(h4("Practical example")),
                               p("Pat Collis is required to sell candy bars to raise money for the 6th grade field trip. There are thirty houses in the neighborhood, and Pat is not supposed to return home until five candy bars have been sold. So the child goes door to door, selling candy bars. At each house, there is a 0.6 probability of selling one candy bar and a 0.4 probability of selling nothing. What's the probability of selling the last candy bar at the nth house?"),
                               p(withMathJax("$${\\displaystyle f(n)={(n-5)+5-1 \\choose n-5}\\;(1-0.4)^{5}\\;0.4^{n-5}={n-1 \\choose n-5}\\;3^{5}\\;{\\frac {2^{n-5}}{5^{n}}}}$$"))
                               ),
                      tabPanel("Poisson process",
                               br(),  p(h4("Poisson process")),
                               p("A Poisson process is a finite or infinite sequence of independent random variables N1, N2, N3, ..., such that for each i, the value of Xi is a a Poisson random variable."),
                               p("In other words, a Poisson process is a sequence of independent identically distributed N(t) Poisson trials of rate λ."),
                               br(), 
                               p(h5("Properties")),
                               p('- Independense."For every n, the process Nn+1, Nn+2, . . . is a Bernoulli process identically distributed with X1, X2, . . . .This future process is independent of the past (X1 , . . . , Xn '),
                               p("- Yk is the time of the kth arrival (Y0 = 0)"),
                               p("- Tk := Yk − Yk−1 is the kth interarrival time"),
                               p("- T1 has an Exponential distribution."),
                               br(), hr(), 
                               p(h4("Gamma distribution")),
                               p("Gamma distribution X ~ Γ(a,b) ~ Γ(n, λ) is a discrete probability distribution that models the number of failures in a sequence of independent and identically distributed Poisson trials before k number of success occurs."),
                               p("Gamma distribution is a distribution of sum of n interarrival times Sn := T1 +··· + Tn is the occurrence time of the nth event"),
                               p("Exponential distribution is a special case on a Gamma binomial distribution with number of successes = 1."),
                               p("Probability mass function:"),
                               p(withMathJax("$${{\\displaystyle {\\begin{aligned}f(x;\\alpha ,\\beta )&={\\frac {\\beta ^{\\alpha }x^{\\alpha -1}e^{-\\beta x}}{\\Gamma (\\alpha )}}\\quad {\\text{ for }}x>0\\quad \\alpha ,\\beta >0\\end{aligned}}}}$$")),
                               br(), p(h5("Example")),
                               splitLayout(
                                 sliderInput("gamma_n", "target numer of success (shape,  k )", 0, 100, 50),
                                 sliderInput("gamma_p", "rate λ of success (1/scale, 1/θ)", 0, 10, 2)),
                               splitLayout(
                                 plotOutput("gamma"),
                                 plotOutput("gamma_cdf")),
                               splitLayout(
                                 htmlOutput('gamma_mean'),
                                 htmlOutput('gamma_var')),
                               br(), hr(), 
                               h4(p("R - code")),
                               br(),
                               p(code("x <- seq(0, size, by = 1)  #Create a vector of values X. Since the distribution is discrete, its length is equal to size (n)")),
                               p(code("y <- dgamma(x, shape, rate))  #Create the probability mass function")),
                               p(code("y2 <- pgamma(x, shape, rate)) #Create the cumulative distribution function")),
                               p(code("y3 <- rgamma(n, shape, rate) #Generate n random deviates")),
                               p(code("plot(x, y)")), br(), hr(), 
                               p(h4("Practical example")),
                               p("Pat Collis is required to sell candy bars to raise money for the 6th grade field trip. There are thirty houses in the neighborhood, and Pat is not supposed to return home until five candy bars have been sold. So the child goes door to door, selling candy bars. At each house, there is a 0.6 probability of selling one candy bar and a 0.4 probability of selling nothing. What's the probability of selling the last candy bar at the nth house?"),
                               p(withMathJax("$${\\displaystyle f(n)={(n-5)+5-1 \\choose n-5}\\;(1-0.4)^{5}\\;0.4^{n-5}={n-1 \\choose n-5}\\;3^{5}\\;{\\frac {2^{n-5}}{5^{n}}}}$$"))
                               
                               ),
                      tabPanel("Other random processes",
                               br(), p(h4("Random walk")),
                               p("Random walk is a stochastic or random process Sn, that describes a path that consists of a succession of random steps."),
                               p("To define this walk formally, take independent random variables Z1,Z2,.., where each variable is either 1 or −1, with a 50% probability for either value, and set S0 = c."),
                               p(withMathJax("$$S_{n}=\\sum _{j=1}^{n} c + Z_{j}\\mbox{ The expectation is equal to: }E(S_{n})= c + \\sum _{j=1}^{n}E(Z_{j})=c$$")),
                               br(),p(h5("Example")),
                               splitLayout(
                                 sliderInput("random_walk_y0", "initial coordinate", -100, 100, 10),
                                 sliderInput("random_walk_p", "probability to move forward", 0, 1, 0.6, step = 0.05),
                                 sliderInput("random_walk_n", "number of steps", 0, 1000, 50, step =10)),
                               
                               plotOutput("random_walk"),
                               br(), hr(),
                               p("Wiener process"), br(),
                               
                               p("Wiener process is a real valued continuous-time stochastic process , also calledBrownian motion"),
                               p("The Wiener process Wt is characterised by the following properties:"),
                               p(withMathJax("$${\\displaystyle W_{0}=0}$$")),
                               p(withMathJax("$$\\mbox{W has independent increments: for every t>0 the future increments} {\\displaystyle W_{t+u}-W_{t},} \\mbox{ u>0 are independent of the past values} {\\displaystyle s\\leq t.}$$")),
                               p(withMathJax("$$\\mbox{W has Gaussian increments: }{\\displaystyle W_{t+u}-W_{t}} \\mbox{ is normally distributed with mean 0 and variance u, }{\\displaystyle W_{t+u}-W_{t}\\sim {\\mathcal {N}}(0,u).}$$")),
                               p("Probability density function:"),
                               p(withMathJax("$${\\displaystyle f_{W_{t}}(x)={\\frac {1}{\\sqrt {2\\pi t}}}e^{-x^{2}/(2t)}}$$")),
                               br(),p(h5("Example")),
                               splitLayout(
                                 sliderInput("wiener_process_n", "number of simulations", 0, 1000, 100)),
                               plotOutput("wiener_process")
                               )
                  )),
                  tabPanel("Descriptive analysis of data", tabsetPanel(
                      tabPanel("Population and samples",
                               br(),
                               p(h4("Population and samples")),
                               p(sample_is_description_text),
                               p("Example:"),
                               splitLayout(
                                   
                                   img(src="population_sample_mean.png"),
                                   verticalLayout(
                               p(sample_is_description_detail_text),
                               p(population_is_description_text))
                              ),
                               br(), hr(),
                              p(h4("Dataset")),
                               p(avocado_dataset_description_text),
                              p(a(href="https://www.kaggle.com/neuromusic/avocado-prices", "By this link you can examine data more detail")),
                               br(),fluidRow(column(DT::dataTableOutput("avocado_data"), width = 12)),
                              br()),
                      tabPanel("Descriptive statistics",
                               br(), 
                               p(h4("Descriptive statistics")),
                               br(),
                              p(avocado_prices_description_text),
                               br(),
                              #splitLayout(p("distributon is distribution "),
                              #            plotOutput("avocado_prices_plot")),
                              splitLayout(verticalLayout(
                                  p("Select a plot type:"),
                                  radioButtons("statistics_visualisation_type", "Select a visualisation type", choices=c("histogram", "frequency polygon", "plot a table", "area plot", "density plot"), selected="histogram")
                                  ),
                                  plotOutput("avocado_prices_hist")
                                  ),
                              br(), hr(),
                              h4(p("R-Simulation")),
                              p(code("datos = read.csv('avocado.csv') #read file after download to local repository folder")),
                              textOutput("plot_types_description"),
                              br(),
                              p(code("# create an appropriate plot"))
                      
                              ),
                      tabPanel("Center location characteristics",
                      br(), hr(),
                              p(h5("Center location characteristics")),

                               splitLayout(verticalLayout(
                                   h5(p("The red vertical line represents the mean"),style="color:red"),
                                   h5(p("The blue vertical line represents the median"),style="color:blue"),
                                   h5(p("The green vertical line indicates the mode"),style="color:lime"),
                                   
                                   h6(p("Mean (Arithmetic Average)" ,
                                        br(),"• The most common measure of central tendency",
                                        br(),"• Mean = sum of values divided by the number of values",
                                        br(),"• Affected by extreme values (outliers)",
                                        br(), code("mean(datos$AveragePrice)"))),
                                   
                                   h6(p("Median",
                                        br(), "• Not affected by extreme values" , 
                                        br(), "• In an ordered array, the median is the “middle” number",
                                        br(), "• If n or N is odd, the median is the middle number",
                                        br(), "• If n or N is even, the median is the average of the two middle numbers",
                                        br(), code("median(datos$AveragePrice)")),
                                   
                                   h6(p("Mode",
                                        br(), "• A measure of central tendency",
                                        br(), "• Value that occurs most often",
                                        br(), "• Not affected by extreme values",
                                        br(), "• Used for either numerical or categorical data",
                                        br(), "• There may be no mode, there may be several modes",
                                        br(), code("getmode <- function(v){"),
                                      br(), code("uniqv <- unique(v)"),
                                      br(), code(" uniqv[which.max(tabulate(match(v, uniqv)))]}"),
                                      br(), code("result = getmode(datos$AveragePrice)")))
                                      )),
                                   plotOutput("avocado_prices_hist_with_mmm"))),
                      tabPanel("Spread characteristics",
                              br(),hr(),
                            p(h4("Min and max")),
                            p("Min is the minimum value from a sample, max is a maximum value from a sample"),
                            p("Data span is a difference between maximum and minimum"),
                            br(),
                            p(h5("R-simulation")),
                            p(code("min = min(datos$AveragePrice) #get minimum")),
                            p(code("max = max(datos$AveragePrice) # get maximum")),
                            p(code("data_span = abs(max - min) $get data span (module max-min)")),
                            br(), hr(),
                              p(h5("standard deviation and Variance")),
                              br(),
                            splitLayout(
                                verticalLayout(
                                p("Population variance"),
                                p(withMathJax("$$\\displaystyle \\sigma^{2}={\\frac {1}{N}}\\sum _{i=1}^{N}(x_{i}-\\mu )^{2}$$")),
                                p("Population standard deviation"),
                                p(withMathJax("$$\\displaystyle \\sigma= {\\sqrt {{\\frac {1}{N}}\\sum _{i=1}^{N}(x_{i}-\\mu )^{2}}}$$"))
                                ),
                                verticalLayout(
                                    p("Sample variance"),
                                    p(withMathJax("$${\\displaystyle s^{2}={\\frac {1}{n-1}}\\sum _{i=1}^{n}\\left(x_{i}-{\\overline {x}}\\right)^{2}}$$")),
                                    p("Sample standard deviation"),
                                    p(withMathJax("$${\\displaystyle s= {\\sqrt { {\\frac {1}{n-1}}\\sum _{i=1}^{n}\\left(x_{i}-{\\overline {x}}\\right)^{2}}}}$$"))
                                    
                                    )
                            ),
                            br(), hr(),
                            p(h5("Percentile")),
                            br(), 
                            splitLayout(verticalLayout(
                                p("A percentile (or a centile) is a measure used in statistics indicating "),
                                p("the value below which a given percentage of observations in a group of "),
                                p("observations falls. For example, the 20th percentile is the value "),
                                p("(or score) below which 20% of the observations may be found.  "),
                                p("Equivalently, 80% of the observations are found above the 20th percentile."),
                                p("select a percentile"),
                                sliderInput("percentile_n", "percentile_n", 0, 100, 1)
                            ),
                            plotOutput("avocado_prices_hist_with_percentile")),
                            br(), hr(),
                            p(h5("Quantile")),
                            br(),
                               splitLayout(verticalLayout(
                                   p("Quantiles are cut points dividing the range of a probability distribution"),
                                   p("into continuous intervals with equal probabilities, or dividing the"),
                                   p("observations in a sample in the same way. Thus quartiles are the three cut"),
                                   p("points that will divide a dataset into four equal-sized groups."),
                                   p("Lower quartile or 25th percentile (−∞,Q1), 25% of data is below this point"),
                                   p("Upper quartile or 75-percentile (−∞,Q3), 75% of data is below this point")
                               ),
                               plotOutput("avocado_prices_hist_with_quartiles")),
                            br(), hr()
                            ),
                      tabPanel("Shape characteristics",
                               br(), hr(),
                               p(h5("Skewness")),
                               br(),
                               p("Skewness is a measure of the asymmetry of the probability distribution of a real-valued random variable about its mean."),
                               p("Negative skew implies that the left tail is longer, positive implies that the right tail is longer, the closer skew to zero, the more symmetric the distribution is."),
                               p("The skewness of a random variable X is the third standardized moment μ3, defined as:"),
                               p(withMathJax("$${\\displaystyle {\\tilde {\\mu }}_{3}=\\operatorname {E} \\left[\\left({\\frac {X-\\mu }{\\sigma }}\\right)^{3}\\right]={\\frac {\\mu _{3}}{\\sigma ^{3}}}={\\frac {\\operatorname {E} \\left[(X-\\mu )^{3}\\right]}{(\\operatorname {E} \\left[(X-\\mu )^{2}\\right])^{3/2}}}={\\frac {\\kappa _{3}}{\\kappa _{2}^{3/2}}}}$$")),
                               img(src = "skew.png", width = 600, height = 200),
                               br(), hr(),
                               p(h5("Kurtosis")),
                               br(),
                               p("Kurtosis is a measure of the 'tailedness' of the probability distribution of a real-valued random variable, describes the shape of a probability."),
                               p("Kurtosis measures extreme values in either tail. Distributions with large kurtosis exhibit tail data exceeding the tails of the normal distribution (e.g., five or more standard deviations from the mean). Distributions with low kurtosis exhibit tail data that are generally less extreme than the tails of the normal distribution."),
                               p("The kurtosis is the fourth standardized moment, defined as"),
                               p(withMathJax("$${\\displaystyle \\operatorname {Kurt} [X]=\\operatorname {E} \\left[\\left({\\frac {X-\\mu }{\\sigma }}\\right)^{4}\\right]={\\frac {\\operatorname {E} \\left[(X-\\mu )^{4}\\right]}{\\left(\\operatorname {E} \\left[(X-\\mu )^{2}\\right]\\right)^{2}}}={\\frac {\\mu _{4}}{\\sigma ^{4}}}}$$")),
                               br(),
                               p("Types of kurtosis:"),
                               img(src="kurt.png", width = 450, height = 250),
                               br(), hr(),
                               p(h5("Example:")),
                               p("Select parameters for the Normal distribution"),
                               br(),
                               splitLayout(verticalLayout(
                               sliderInput("makemenormal_mean", "mean", -100, 100, 0),
                               sliderInput("makemenormal_sd", "sd", 0, 100, 1),
                               sliderInput("makemenormal_ku", "kurtosis", -3,10, 4, step =0.1),
                               sliderInput("makemenormal_sc", "skewness", -3, 3, 1, step =0.1)),
                               plotOutput("makemenormal_plot")),
                               br(), hr(),
                               h5(p("R-simulation")),
                               splitLayout(
                                   verticalLayout(
                                       p(code("library('e1071')")),
                                       p(code("skewness = skewness(datos$AveragePrice)")),
                                       p(code("kurtosis = kurtosis(datos$AveragePrice)")),
                                       p(code("paste('skewness =', skewness, ', kurtosis = ', kurtosis)")),
                                       br(),
                                       p("The graph on the right displays the real distribution of prices (histogram)"),
                                       p("simulation of the normal distribution with the same parameters, but"),
                                       p("with zero skewness")
                                       ),
                                   verticalLayout(
                               plotOutput("avocado_prices_hist_with_skewness"),
                               textOutput("avocado_prices_skewness"))
                               )
                               
                               ),
                      tabPanel("Empirical c.d.f",
                              br(),
                              p(h4("Empirical cumulative distribution function")),
                              p("The empirical distribution function is an estimate of the cumulative distribution function that generated the points in the sample."),
                              p("This cumulative distribution function is a step function that jumps up by 1/n at each of the n data points. Ecdf converges with probability 1 to that underlying distribution cdf."),
                              p(withMathJax("$${\\displaystyle {\\widehat {F}}_{n}(t)\\ {\\xrightarrow {\\text{a.s.}}}\\ F(t);}$$")),
                              p(withMathJax("$${\\displaystyle {\\widehat {F}}_{n}(t)={\\frac {{\\mbox{number of elements in the sample}}\\leq t}{n}}={\\frac {1}{n}}\\sum _{i=1}^{n}\\mathbf {1} _{X_{i}\\leq t},}$$")),
                              br(),
                              splitLayout(
                                  verticalLayout(
                                  p("Initial distribution is Normal with  mean = 0, sd =  1 "),
                                  radioButtons("ecdf_n", "number of elements in sample", choices = c(10,50, 100, 500, 1000, 5000, 10000), selected = 10),
                                  br(),
                                  p(h5("R-simulation")),
                                    p(code("x = seq(-10, 10, by = 0.01)")),
                                  p(code("plot(ecdf(rnorm(n , 0, 1)), xlim = c(-5,5))")),
                                    p(code("lines(x, pnorm(x, 0, 1 ), type='l', col = 'red')"))
                                  ),
                                  plotOutput("ecdf")
                              )
                                )
                      
                      
                      )),
                  tabPanel("Parameter estimation", tabsetPanel(
                      tabPanel("Estimation and estimators",
                               br(),
                               p(h4("Use of estimators")),
                               p("An estimator attempts to approximate the unknown parameters using the measurements."),
                               p("For example, we want to know the proportion of people who smoke in the UCF, but we cannot survey thousands of people, we can only 50. That proportion is the parameter sought and the estimate is based on a small random sample of students."),
                               br(),hr(),
                               p(h4("Estimator")),
                               p("Suppose a fixed parameter θ needs to be estimated. Then an 'estimator' is a", strong("function"), " that maps the sample space to a set of sample estimates. An estimator of θ is usually denoted by the symbol θ^"),
                               p("For example, a sample mean is an estimator for a population mean"),
                               p(withMathJax("$${\\displaystyle T(X)={\\frac {1}{n}}\\sum _{i=1}^{n}x_{i}={\\frac {x_{1}+x_{2}+\\cdots +x_{n}}{n}}}$$")),
                               br(),hr(),
                               p(h4("Estimate")),
                               p("We substitute the sample into formula T(x) and get an estimate of the parameter μ."),
                               p(withMathJax("$$\\bar{X}=\\frac{1}{N}\\sum_{i=1}^{N}x_{i} $$")),
                               
                               br(), p("Example"),
                               splitLayout(verticalLayout(
                                p("We toss a coin and want to guess the average value that "),
                                p("it produces. To do this, we use the estimate of the mean after "),
                                p("flipping it n the number of times."),
                                p("Red line - real expected value =0.5"),
                                p("Green line - estimated expected value"),
                                br(),
                                p("If the green and red lines coincide, then the estimate is unbiased"),
                                p("If lines dont coincide, then the estimate is biased."),
                                br(),
                               p("Select a number of a fair coin throwing"),
                               radioButtons("coin_n", "number of throwing", c(1,2,3,4,5,10))),
                               plotOutput("coin_estimator")
                               )
                               ),
                      tabPanel("Moment methos estimators",
                               br(),
                               p("Method of moments - a method for estimating unknown parameters of distributions in mathematical statistics and econometrics, based on the assumed properties of moments"),
                               p("The idea of the method is to replace the true relationships between the distribution parameters and its moments with sample analogs of moments."),
                                br(),
                                p("Suppose a sample of size n is drawn, resulting in the values w1,…,wn. For j=1,…,k, let"),
                                p(withMathJax("$${\\displaystyle {\\widehat {\\mu }}_{j}={\\frac {1}{n}}\\sum _{i=1}^{n}w_{i}^{j}}$$")),
                                p("be the j-th sample moment, an estimate of μj"),
                      br(),
                      p("The method of moments is fairly simple and yields consistent estimators (under very weak assumptions), though these estimators are often biased."),
                      br(),
                      p("Example:"),
                      img(src = "m_m_e.png", width = 1000, height= 250)),
                      
                      tabPanel("Maximum likelihood estimator",
                               br(), p(h4("Maximum likelihood estimation")),
                               p("Maximum likelihood estimation is a method of estimating the parameters of a probability distribution by maximizing a likelihood function, so that under the assumed statistical model the observed data is most probable. The point in the parameter space that maximizes the likelihood function is called the maximum likelihood estimate"),
                          
                               p(withMathJax("$${\\displaystyle {\\hat {\\theta }}={\\underset {\\theta \\in \\Theta }{\\operatorname {arg\\;max} }}\\ {\\widehat {L}}_{n}(\\theta \\,;\\mathbf {y} )}$$")),
                               br(), hr(), 
                               p(h4("Likelihood function")),
                               p("Likelihood function measures the goodness of fit of a statistical model to a sample of data for given values of the unknown parameters."),
                               p("Let X be a discrete random variable with probability mass function p depending on a parameter θ."),
                               p("The likelihood is equal to the probability that a particular outcome x is observed when the true value of the parameter is θ"),
                               p(withMathJax("$${\\displaystyle {\\mathcal {L}}(\\theta \\mid x)=p_{\\theta }(x)=P_{\\theta }(X=x)}$$")),
                               br(),
                               p(h5("Example")),
                               p("We have obtatined number of tosses and number of Heads. Likelihood function shows the probability for each probability of rhe coin. MLE finds point where it reaches its maximum."), br(), 
                               splitLayout(
                                 sliderInput("mle_total", "total number of tosses", 0, 1000, 50),
                                 sliderInput("mle_h", "nuber of H got", 0, 1000, 25),
                                 radioButtons("mle_log", "function type", c("likelihood", "log-likelihood"))),
                               plotOutput("mle_binomial"),
                               textOutput("mle_Binomial_res")),
                      br(), p(h4("Log- likelihood")),
                      p("Log-likelihood function is a logarithmic transformation of the likelihood function. The log-likelihood is, as the term suggests, the natural logarithm of the likelihood."),
                      p("It is usefull because sums are instead on product more stable from a numerical standpoin. The likelihood obtained from likelihood may be so small that problems with counting MLE may arise."),
                      p(withMathJax("$${\\displaystyle \\log {\\frac {L(A)}{L(B)}}=\\log L(A)-\\log L(B)=\\ell (A)-\\ell (B)}$$")),
                       br(), hr(), 
                       p(h5("R-simulation")),
                       p(code("mle.results <- optimize(function(p) {likelihoodd(sequence, p)},interval = c(0, 1), maximum = TRUE)")),
                       p(code("# to find MLE and optimize function 'likelihoodd' with its parameters"))
                      
                  )),
                  tabPanel("Bayesian and Interval estimation", tabsetPanel(
                    tabPanel("Bayesian estimation",
                             br(),
                             p(h4("Prior and posterior distribution")),
                             p("The posterior probability is the probability of the parameters θ given the evidence X (prior) X: p(θ|X)"),
                             p(withMathJax("$${\\displaystyle p(\\theta |x)={\\frac {p(x|\\theta )}{p(x)}}p(\\theta )}$$")),
                             p(withMathJax("$${\\text{Posterior probability}}\\propto {\\text{Likelihood}}\\times {\\text{Prior probability}}$$")),
                             br(), hr(),
                             p(h5("Example")), br(),
                             p('We have two coin with some probabilily to get H each. We have a probability to get the first coin (prior distribution) and a number of H got after 10 coins. Which coin did we chose?'),
                            splitLayout(
                              sliderInput("guessing_a_coin_n_h", "number of H got", 0, 10, 6),
                              sliderInput("guessing_a_coin_pc", "probability to get the first coin (prior distribution)", 0, 1, 0.3, step=0.01)),
                             splitLayout(
                             sliderInput("guessing_a_coin_p1", "Heads probability of the first coin", 0, 1, 0.5, step=0.01),
                             sliderInput("guessing_a_coin_p2", "Heads probability of the second coin", 0, 1, 0.6, step=0.01)),
                            br(), p("The black line on the chart is the first coin, the red line is the second coin. The distributions show the probability of getting a particular amount of H, the binomial distribution function is multiplied by the probability of pulling a coin. The vertical lines represent the selected coin probability amount. We will consider the values obtained at the intersection."),
                             plotOutput("guessing_a_coin"),
                             textOutput("guessing_a_coin_inf"),
                            br(), hr(),
                            p(h5("R-simulation")),
                            p(code("posterior1 = pc1* ((1-p1)^(10-h))*(p1^h)")),
                            p(code("posterior2 = pc2* ((1-p2)^(10-h))*(p2^h)")),
                            p(code("if (posterior1>posterior2){")),
                            p(code("} # coin 1 is more likely, else - coin 2 is more likely"))
   
                             ),
                     
                      tabPanel("Confidence interval",
                               br(), 
                               p(h4("Confidence interval")),
                               p("Confidential is the interval that covers the unknown parameter with a given reliability."),
                               p("The confidence interval of the parameter θ of the distribution of the random variable X with the level of confidence p is the interval with the boundaries L and U such that"),
                               p(withMathJax("$$\\mathbb {P} (L \\leqslant \\theta \\leqslant U) = p$$")),
                               br(), hr(),
                               p(h5("Practical example")),
                               p("A machine fills cups with a liquid, and is supposed to be adjusted so that the content of the cups is 250 g of liquid. As the machine cannot fill every cup with exactly 250.0 g, the content added to individual cups shows some variation, and is considered a random variable X. +- 2.5 g is a confidence interval for X."),
                               img(src='conf_interval.png'),
                               br(), hr(),
                               p("Notion"),
                               p("A 95% confidence level does not mean that for given realized interval there is a 95% probability that the population parameter lies within the interval."),
                               p("The 95% probability relates to the reliability of the estimation procedure, not to a specific calculated interval.")
                               
                               ),
                      tabPanel("Confidence intervals for mean from Normal distribution",
                               br(),
                               p(h4("Confidence intervals for mean from Normal distribution with known variance")),
                               p("Assume X1, X2, . . . are i.i.d. r.v.’s with normal distribution N (μ, σ2)"),
                               p("We have identified a sample mean and we want to know the confidence interval at the 1-a level. The confidence interval is defined on:"),
                               p(withMathJax("$${\\displaystyle \\left({\\bar {x}}-z^{*}{\\sigma  \\over {\\sqrt {n}}},{\\bar {x}}+z^{*}{\\sigma  \\over {\\sqrt {n}}}\\right)}$$")),
                               p(withMathJax("$${\\mbox{where }\\displaystyle z^{*}=\\Phi ^{-1}\\left(1-{\\frac {\\alpha }{2}}\\right)=-\\Phi ^{-1}\\left({\\frac {\\alpha }{2}}\\right)}$$")),
                               hr(), p(h5("Example")),
                               p("As an example, we enter the parameters for the normal distribution, the percentage of the confidence interval, and the number of simulations. The blue line represents the sample mean for the generated sample, the green lines represent the confidence interval for the sample mean, which will exactly fit the given percentage of means."),
                               actionButton("gobutton2", "Rebuild"),
                               splitLayout(
                               sliderInput("conf_interval_mean", "mean", -20, 20, 8),
                               sliderInput("conf_interval_sd", "sd", 0, 30, 20),
                               radioButtons("conf_interval_a", "confidence interval", c(80,90,95,99) ),
                               radioButtons("conf_interval_n", "number", c(1,10,100,1000, 10000))),
                               plotOutput("conf_interval"),
                               br(), hr(),
                               p(h4("Confidence intervals for mean from Normal distribution with unknown variance")),
                               p("Assume X1, X2, . . . are i.i.d. r.v.’s with normal distribution N (μ, σ2)"),
                               p("We have identifiesd a sample mean , a sample standert deviation and we want to know the confidence interval at the C = 100*(1-a)% level. The confidence interval is defined on:"),
                               
                               p(withMathJax("$${\\displaystyle \\left({\\bar {x}}-t^{*}{s \\over {\\sqrt {n}}},{\\bar {x}}+t^{*}{s \\over {\\sqrt {n}}}\\right)}$$")),
                               p(withMathJax("$$\\mbox{where }{\\displaystyle t^{*}=t_{\\alpha }(r)}\\mbox{ is the critical value from Student distribution table with r degrees of freedom and }{\\displaystyle \\alpha ={1-C \\over 2}}.$$")),
                               hr(), p(h5("Example")),
                               br(),
                               p("As an example, we enter the parameters for the normal distribution, the percentage of the confidence interval, and the number of simulations. The blue line represents the sample mean for the generated sample, the green lines represent the confidence interval for the sample mean, which will exactly fit the given percentage of means."),
                               actionButton("gobutton3", "Rebuild"),
                               splitLayout(
                               sliderInput("conf_interval_mean_without", "mean", -20, 20, 18),
                               radioButtons("conf_interval_a_without", "confidence interval", c(80,90,95,99) ),
                               radioButtons("conf_interval_n_without", "number", c(1,10,100,1000, 10000))),
                               plotOutput("conf_interval_without_v")
                      ),
                      
                      tabPanel("Student distribution",
                               br(), 
                               p(h4("Student distribution")),
                               p("Student's distribution or t-distribution is a  continuous probability distribution."),
                               p("Student's distribution arises when estimating the mean of a normally distributed population in situations where the sample size is small and the population ",strong("standard deviation is unknown.")),
                               p("Probability density function: (ν is the number of degrees of freedom and Γ is the gamma function"),
                               p(withMathJax("$$\\textstyle\\frac{\\Gamma \\left(\\frac{\\nu+1}{2} \\right)} {\\sqrt{\\nu\\pi}\\,\\Gamma \\left(\\frac{\\nu}{2} \\right)} \\left(1+\\frac{x^2}{\\nu} \\right)^{-\\frac{\\nu+1}{2}}\\!$$")),
                               br(), hr(),
                               splitLayout(
                                   verticalLayout(
                               radioButtons("student_distribution_n", "n-1 degrees of freedom", c(1,2,5,10,100,1000, 10000)),
                               checkboxInput("student_distribution_normal", "normal distribution"),
                               br(),
                               p("R-simulation"),
                               p(code("distribution = rt(n, df) # generate n values")),
                               p(code("plot(density(distribution))"))),
                               plotOutput("student_distribution")),
                               p("The t-distribution is symmetric and bell-shaped, like the normal distribution, but has heavier tails, meaning that it is more prone to producing values that fall far from its mean. The t-distribution becomes closer to the normal distribution as ν increases."),
                               p("Z /(S/σ) → T in distribution!"),
                               hr(),
                               p(h4("Student's distribution and sampling")),
                               p("Let X1,…,Xn be independently and identically drawn from the distribution N(μ,σ2) i.e., this is a sample of size n from a normally distributed population with expected mean value μ and variance σ2"),
                      p(withMathJax("$${\\displaystyle {\\bar {X}}={\\frac {1}{n}}\\sum _{i=1}^{n}X_{i}}\\mbox{ is a sample mean;  }{\\displaystyle S^{2}={\\frac {1}{n-1}}\\sum _{i=1}^{n}(X_{i}-{\\bar {X}})^{2}\\mbox{ is a sample variance}}$$")),
                      p(withMathJax("$${\\mbox{Than, a r.v. X = }{\\displaystyle {\\frac {{\\bar {X}}-\\mu }{S/{\\sqrt {n}}}}}\\mbox{ has a Students t-distribution with n-1 degrees of freedom}}$$"))     ,
                      br(), hr(),
                      p(h4("Student distribution values table")),
                      p("For estimation t-value"),
                      img(src = "student.png")
                      ),
                      tabPanel("Confidence interval for the variance of the Normal distribution",
                               br(),
                               p(h4("Confidence intervals for a variance from Normal distribution with known variance")),
                               p("Assume X1, X2, . . . are i.i.d. r.v.’s with normal distribution N (μ, σ2)"),
                               p("We have identifiesd a sample mean, sample variance and we want to know the confidence interval for a variance at the 1-a level. The confidence interval is defined on:"),
                               img(src = 'c_i_var.png'),
                               p("")
                               
                               
                               ),
                      tabPanel("Chi-squared distribution",
                                br(), 
                               p(h4("Chi-squared distribbution")),
                               p("The chi-squared distribution (also chi-squared or χ2-distribution) with k degrees of freedom is the distribution of a sum of the squares of k independent standard normal random variables"),
                               p("The chi-square distribution is a special case of the gamma distribution with k = k/2 and theta = 2."),
                               p("If Z1, ..., Zk are independent, standard normal random variables, then"),
                               p(withMathJax("$${\\displaystyle \\sum _{i=1}^{k}(Z_{i}-{\\overline {Z}})^{2}\\sim \\chi _{k-1}^{2}\\mbox{  where  }\\displaystyle {\\overline {Z}}={\\frac {1}{k}}\\sum _{i=1}^{k}Z_{i}.}$$")),
                               p("Probability density function: (where Γ(k/2) denotes the gamma function"),
                               p(withMathJax("$${\\displaystyle f(x;\\,k)={\\frac {1}{2^{k/2}\\Gamma (k/2)}}\\;x^{k/2-1}e^{-x/2}\\; \\mbox{for x>0 and 0 otherwise}}$$")),
                               br(), hr(),
                               splitLayout(
                                   verticalLayout(
                                       p("Enter n degrees of freedom parameter:"),
                                       radioButtons("chi_squared_n", "n degrees of freedom", c(1,2,5,10,100,1000, 10000)),
                                        br(),
                                       p("R-simulation"),
                                       p(code("distribution = rchisq(n, df) # generate n values")),
                                       p(code("plot(density(distribution))"))),
                                   plotOutput("chi_squared")
                               ),hr(),
                               p(h4("Chi-square distribution and sampling")),
                               p("Let X1,…,Xn be independently and identically drawn from the distribution N(μ,σ2) i.e., this is a sample of size n from a normally distributed population with expected mean value μ and variance σ2"),
                               p(withMathJax("$${\\displaystyle {\\bar {X}}={\\frac {1}{n}}\\sum _{i=1}^{n}X_{i}}\\mbox{ is a sample mean;  }{\\displaystyle S^{2}={\\frac {1}{n-1}}\\sum _{i=1}^{n}(X_{i}-{\\bar {X}})^{2}\\mbox{ is a sample variance}}$$")),
                               p(withMathJax("$${\\mbox{Than, a r.v. X = }\\sum _{i=1}^{k}\\left({\\frac {X_{i}-\\bar {X}}{\\sigma _{i}}}\\right)^{2} = {\\frac {(n-1)*S^{2}}{\\sigma^{2}}}\\mbox{ has a Chi-square distribution with n-1 degrees of freedom}}$$"))    ,     
                               br(),
                               p("For estimation x2 with n-1 df for c.l. a"),
                               img(src="chi_square.png")
                               ),
                      tabPanel("standard normal distribution",
                               br(), p(h4("standard normal distribution")),
                               p("The standard normal distribution is a special case of a Normal distribution with mu = 0 and σ = 1"),
                               p("Probability density function"),
                               p(withMathJax("$${\\displaystyle \\varphi (x)={\\frac {1}{\\sqrt {2\\pi }}}e^{-{\\frac {1}{2}}x^{2}}}$$")),
                               img(src = "standard_n_d.png"), br(), hr(), br(),
                               p(h4("Thee sigma rule")),
                               p("The 68–95–99.7 rule, or the empirical rule is a shorthand used to remember the percentage of values that lie within a band around the mean in a normal distribution "),
                               br(),
                               splitLayout(
                                 verticalLayout(
                               p(h5("If a data set is approximately normal with")),
                               p(h5("sample mean x and sample standard deviation s, then")),
                               p("Approximately 68.27 percent of the observations lie within x±s"),
                               p("Approximately 95.45 percent of the observations lie within x±2s"),
                               p("Approximately 99.73 percent of the observations lie within x±s")),
                               img(src = 'three_sigma.png'), br(), hr()
                               ),
                               p(h4("Z-score")), br(),
                               p("A z-table, also called the standard normal table, is a mathematical table that allows us to know the percentage of values below (to the left) a z-score in a standard normal distribution (SND)."),
                               p("A z-score, also known as a standard score, indicates the number of standard deviations a raw score lays above or below the mean. When the mean of the z-score is calculated it is always 0, and the standard deviation (variance) is always in increments of 1."),
                               p("The Central Limit Theorem assers that Zn converge in law to the standard Gaussian r.v. Z ∼ N (0, 1)"),
                               br(), img(src = "z_score.png")
                               )
                      
                      
                  )),
                  tabPanel("Hypothesis testing", tabsetPanel(
                      tabPanel("Statistical hypothesis",
                               br(),
                               p(h4("Statistical hypothesis")),
                               p("Statistical hypothesis is an assumption about the type of distribution and properties of a random variable, which can be confirmed or refuted by applying statistical methods to the sample data "),
                              p("Let in a (statistical) experiment a random variable X whose distribution P is completely or partially unknown is available for observation. Then any statement about P is called a statistical hypothesis"),
                              p("Hypothesis types:"),
                              p("H0 or Null hypothesis - for the outcome we expect"),
                              p("H1 or alternative hypothesis - for an alternative outcome"),
                              p("Errors types:"),
                              p("Type | error - reject true hypothesis"),
                              p("Type || error - accept false hypothesis"),
                              br(),
                              splitLayout(
                                img(src = 'error_type.png'),
                                img(src= "type_error.png")
                              ),
                              img(src = "h3.png", width = 600, height = 300),
                              br(),
                              p("Confidence interval for H0 is C = 100*(1-a)%"),
                              p("The probability of the type | error = a is called size or significance level of the test."),
                              p("Confidence interval for H1 is C = 100*(1-b)%"),
                              p("The probability of the type || error = b , 1-b is called size or power of the test of the test.")
                              
                      )
                              
                  )),
                  
                  tabPanel("Testing hypotheses for normal distribution", tabsetPanel(
                      tabPanel("z-test",
                               br(), p(h4("Z-test")),
                      p("Z-test a class of methods for statistical testing of hypotheses (statistical tests) based on a normal distribution. It is usually used to test the equality of means with a known population variance."),
                      p("formula for a standard score :"),
                      p(withMathJax("$${\\displaystyle z_{\\overline {X}}={\\frac {{\\overline {X}}-\\,m_{H_{o}}}{\\mathrm {\\sigma /{\\sqrt {n}}} }}}$$")),
                      br(), p("For Null hypothesis H0: μ≥μ0 vs alternative hypothesis H1: μ<μ0 , it is upper/right-tailed (one tailed)."),
                      p("For Null hypothesis H0: μ≤μ0 vs alternative hypothesis H1: μ>μ0 , it is lower/left-tailed (one tailed)."),
                      p("For Null hypothesis H0: μ=μ0 vs alternative hypothesis H1: μ≠μ0 , it is two-tailed."),
                      br(), p(h4("Method of application")),
                      p("It is necessary that the data have a normal distribution and that the variance of the population is known. The Z-test is used to test the null hypothesis that the mathematical expectation of a random variable is equal to some value m. H0: Mx = m"),
                      p("If the critical value zX is exceeded (for example, zX <−1.96 or zX> 1.96 at a 5% significance level), the null hypothesis is rejected and the value of the random value is considered statistically significant.") ,
                      br(), hr(),
                      
                      p(h4("Example one sample")),
                      splitLayout(
                        
                        verticalLayout(
                          sliderInput("z_test_mean", "mean", -20, 20, value = 0),
                          sliderInput("z_test_sd", "standard deviation", 0, 10, value = 1),
                          sliderInput("z_test_mu", "mu", -20, 20, value = 1),
                          splitLayout(
                          radioButtons("z_test_method", "method of comparing", c( "greater", "less" ,"two.sided") , selected = "two.sided"),
                          
                          radioButtons("z_test_a", "condidence interval", c(90,95,99, 99.9), selected = 95 ),
                          radioButtons("z_test_n", "number of values generated", c(1,10, 100, 1000, 10000), selected = 10 )),
                          br(), p(h4("R-simulation")),
                          p(code("dist = rnorm(n, mean , sd)")),
                          p(code("z = z.test(dist, alternative = method, mu = mu, sigma.x = sd, conf.level = a)"))
                        ),
                        
                        verticalLayout(
                          plotOutput("z_test_plot"),
                          verbatimTextOutput("z_test_summary")
                        )),
                      
                      br(), hr(),
                      p(h4("Example two sample")),
                      splitLayout(
                        
                        verticalLayout(
                          sliderInput("z2_test_mean1", "mean1", -20, 20, value = 0),
                          sliderInput("z2_test_sd1", "standard deviation1", 0, 10, value = 1),
                          sliderInput("z2_test_mean2", "mean2", -20, 20, value = 0),
                          sliderInput("z2_test_sd2", "standard deviation2", 0, 10, value = 1),
                          splitLayout(
                            radioButtons("z2_test_method", "method of comparing", c( "greater", "less" ,"two.sided") , selected = "two.sided"),
                            radioButtons("z2_test_a", "condidence interval", c(90,95,99, 99.9), selected = 95 ),
                            radioButtons("z2_test_n1", "number of values generated for  the first distribution", c(1,10, 100, 1000, 10000), selected = 10 ),
                            radioButtons("z2_test_n2", "number of values generated the second distribution", c(1,10, 100, 1000, 10000), selected = 10 )),
                          br(), p(h4("R-simulation")),
                          p(code("dist1 = rnorm(n1, mean1 , sd1)")),
                          p(code("dist2 = rnorm(n2, mean2 , sd2)")),
                          p(code("z = z.test(dist1,dist2, alternative = method,  sigma.x = sd1,sigma.y = sd2 , conf.level = a)"))
                        ),
                        
                        verticalLayout(
                          plotOutput("z2_test_plot"),
                          verbatimTextOutput("z2_test_summary")
                        ))
                              ),
                      tabPanel("p-value",
                               br(),
                               p(h4("p-value")),
                               p("p-level of significance, p-criterion - the probability of obtaining for a given probabilistic model of the distribution of values of a random variable the same or more extreme value of statistics (arithmetic mean, median, etc.), compared to the previously observed, provided that the H0 is true."),
                               p("p-value  of the test is the smallest size α at which H0 can be rejected for given observation x"),
                               p("For example, if the p-value is equal to 0.015 , we can reject H0 at the 0.01 level (p ≤ a) and accept at 0.05 level (p<a)."),
                               br(),
                               p(h4("Calculation")),
                               img(src ="p_value.png")
                               
                               
                               ),
                    
                      tabPanel("t-test",
                               br(), p(h4("t-test")),
                                       p("T-test is a class of methods for statistical testing of hypotheses (statistical tests) based on a normal distribution. It is usually used to test the equality of means with an unknown population variance."),
                                       p("formula for a standard score :"),
                                       p(withMathJax("$${\\displaystyle t={\\frac {Z}{s}}={\\frac {{\\bar {X}}-\\mu }{{\\widehat {\\sigma }}/{\\sqrt {n}}}}}$$")),
                                       br(), p("For Null hypothesis H0: μ≥μ0 vs alternative hypothesis H1: μ<μ0 , it is upper/right-tailed (one tailed)."),
                                       p("For Null hypothesis H0: μ≤μ0 vs alternative hypothesis H1: μ>μ0 , it is lower/left-tailed (one tailed)."),
                                       p("For Null hypothesis H0: μ=μ0 vs alternative hypothesis H1: μ≠μ0 , it is two-tailed."),
                                       br(), p(h4("Method of application")),
                                       p("It is necessary that the data have a normal distribution and that the variance of the population is known. The Z-test is used to test the null hypothesis that the mathematical expectation of a random variable is equal to some value m. H0: Mx = m"),
                                       p("If the critical value zX is exceeded (for example, zX <−1.96 or zX> 1.96 at a 5% significance level), the null hypothesis is rejected and the value of the random value is considered statistically significant.") ,
                                       br(), hr(),
                                       
                                       p(h4("Example one sample")),
                                       splitLayout(
                                         
                                         verticalLayout(
                                           sliderInput("t_test_mean", "mean", -20, 20, value = 0),
                                           sliderInput("t_test_sd", "standard deviation", 0, 10, value = 1),
                                           sliderInput("t_test_mu", "mu", -20, 20, value = 1),
                                           splitLayout(
                                             radioButtons("t_test_method", "method of comparing", c( "greater", "less" ,"two.sided") , selected = "two.sided"),
                                             
                                             radioButtons("t_test_a", "condidence interval", c(90,95,99, 99.9), selected = 95 ),
                                             radioButtons("t_test_n", "number of values generated", c(1,10, 100, 1000, 10000), selected = 10 )),
                                           br(), p(h4("R-simulation")),
                                           p(code("dist = rnorm(n, mean , sd)")),
                                           p(code("t = t.test(dist, alternative = method, mu = mu, conf.level = a)"))
                                         ),
                                         
                                         verticalLayout(
                                           plotOutput("t_test_plot"),
                                           verbatimTextOutput("t_test_summary")
                                         )),
                                       
                                       br(), hr(),
                                       p(h4("Example two sample")),
                                       splitLayout(
                                         
                                         verticalLayout(
                                           sliderInput("t2_test_mean1", "mean1", -20, 20, value = 0),
                                           sliderInput("t2_test_sd1", "standard deviation1", 0, 10, value = 1),
                                           sliderInput("t2_test_mean2", "mean2", -20, 20, value = 0),
                                           sliderInput("t2_test_sd2", "standard deviation2", 0, 10, value = 1),
                                           splitLayout(
                                             radioButtons("t2_test_method", "method of comparing", c( "greater", "less" ,"two.sided") , selected = "two.sided"),
                                             radioButtons("t2_test_a", "condidence interval", c(90,95,99, 99.9), selected = 95 ),
                                             radioButtons("t2_test_n1", "number of values generated for  the first distribution", c(1,10, 100, 1000, 10000), selected = 10 ),
                                             radioButtons("t2_test_n2", "number of values generated the second distribution", c(1,10, 100, 1000, 10000), selected = 10 )),
                                           br(), p(h4("R-simulation")),
                                           p(code("dist1 = rnorm(n1, mean1 , sd1)")),
                                           p(code("dist2 = rnorm(n2, mean2 , sd2)")),
                                           p(code("t = t.test(dist1,dist2, alternative = method, conf.level = a)"))
                                         ),
                                         
                                         verticalLayout(
                                           plotOutput("t2_test_plot"),
                                           verbatimTextOutput("t2_test_summary")
                                         ))
                                       
                                       
                                       
                                       ),
                      tabPanel("Kolmogorov–Smirnov test",
                               br(),
                               p(h4("Kolmogorov–Smirnov statistics")),
                               p("Kolmogorov's goodness-of-fit criterion for testing the hypothesis about the criteria for choosing a certain distribution law, that is, checking that the empirical correspondence corresponds to the proposed model."),
                               p("H0 :F=F0 vs H1 :F̸=F0"),
                               p(withMathJax("$$D_{n}=\\sup _{x}|F_{n}(x)-F(x)|$$")),
                               br(),
                               p(h4("Kolmogorov–Smirnov goodness-on-fit test")),
                               p("The goodness-of-fit test or the Kolmogorov–Smirnov test can be constructed by using the critical values of the Kolmogorov distribution. It rejects the null hypothesis at level α if:"),
                               p("{\\sqrt {n}}D_{n}>K_{\\alpha },"),
                               br(), p("Values can be obtaines from table of k-s distribution"),
                               img(src = 'k_s.png'),
                               br(), hr(), 
                               p(h4("R-simulation")),
                               p(code("x = rnorm(100, 10, 1")),
                               p(code("y = rbinom(100, 20, 0.5")),
                               p(code("ks.test(x, y, alternative = 'two.sided')"))
                               )
                    
                  )),
                  
                  tabPanel("Regression models", tabsetPanel(
                      tabPanel("Linear regression",
                               br(),
                               p(h4("Linear regression")),
                                 p("Linear regression is a linear approach to modeling the relationship between a scalar response (or dependent variable) and one or more explanatory variables (or independent variables)."),
                                 p("Having a dataset with y, x1, x2 ,..xn ,linear regression model assumes that the relationship between the dependent variable y and regressors x is linear and described by formula:"),
                                 p(withMathJax("$${\\displaystyle y_{i}=\\beta _{0}+\\beta _{1}x_{i1}+\\cdots +\\beta _{p}x_{ip}+\\varepsilon _{i} ,\\qquad i=1,\\ldots ,n}$$")),
                                 br(), hr(),
                                 p(h4("Example")),
                                 p("The overall grade in English consists of these parameters. The student gets 10 points just like that, 40 points for essays, 30 for the exam and 40 for the activity at the seminars. Also, let's say that a student can randomly gain or lose a couple of points."),
                                 p("y = b0 + b1*x1 + b2*x2+ b3*x3 + e = 10 + 0.4*x1 + 0.3*x2 + 0.4*x3 + e"),
                                 p("Where y is a total grade, x1, x2, x3 are points for essays, exam and seminars respectively (x1, x2, x3 are in the interval (0, 100). And e is an unobserved effect, let's call it a success or a failure by a few points"),
                                 br(),
                                 p(" ε ∼ N (0, σ2) is the random error with mean 0"),
                                 p("The case of one explanatory variable (y = b0 + b1*x1 + e) is called simple linear regression"),
                                 p("For more than one explanatory variable, (y = b0 + b1*x1 + b2*x2 + ... + bn*xn + e) the process is called multiple linear regression")
                                 ),
                      
                      tabPanel("Sum of squared residuals",
                               br(),
                               p(h4("RSS")),
                               p("Residual sum of squares (RSS) or sum of squared residuals (SSR) or the sum of squared estimate of errors (SSE) is the sum of the squares of residuals (deviations predicted from actual empirical values of data)"),
                               p("total sum of squares = explained sum of squares + residual sum of squares. TSS = ESS + RSS"),
                               p(withMathJax("$${\\mathrm  {TSS}}=\\sum _{{i=1}}^{{n}}\\left(y_{{i}}-{\\bar  {y}}\\right)^{2}$$")),
                               p(withMathJax("$${\\text{ESS}}=\\sum _{{i=1}}^{n}\\left({\\hat  {y}}_{i}-{\\bar  {y}}\\right)^{2}.$$")),
                               p(withMathJax("$${\\displaystyle \\operatorname {RSS} =\\sum _{i=1}^{n}(y_{i}-f(x_{i}))^{2}}=\\sum _{i}e_{i}^{2}$$")),
                               p("Example:"),
                               p("The sum of the blue squares is the total sum of the squares, and the sum of the red squares is the sum of the squares of the residuals."),
                               splitLayout(
                               img(src = "r_m_s.png", width= 380, height = 250),
                               img(src = "errors.png")
                               ),
                               br(), hr(),
                               p(h4("Least square method")),
                               p("What is the best cooficents for y = b0 + b1*x1 + e ?"),
                               p("MLE b0 and b1 are estimate, obtained by least squares method."),
                               p("The aim of OLS is to minimize sum of the squared errors (RSS)."),
                               p(withMathJax("$${\\displaystyle \\operatorname {RSS} =\\sum _{i=1}^{n}(\\varepsilon _{i})^{2}=\\sum _{i=1}^{n}(y_{i}-(\\alpha +\\beta x_{i}))^{2}}$$")),
                               br(), hr(),
                               p(h4("R-simulation")),
                               p(a(href="https://www.kaggle.com/johndasilva/diabetes", "We exmine data from this dataset.")),
                               p("'lm' function in R-studio uses least squares method to estimate coofecents of the linear regression."),
                                       p(code("datos = read.csv('diabettes.csv')")),
                                       p(code("linear_model = lm(Insulin ~ BloodPressure + SkinThickness + BMI + Pregnancies + Age + Glucose + Insulin, data = datosd)")),
                                       p(code("summary(linear_model)")),
                                           verbatimTextOutput("ols"),
                               br(), hr(),
                               p(h4("Interpretation")),
                               p("We set H0 that regressor variable is not significant and H1 that it is significant. So, the less p-value is, the bigger confidence interval to accept its significance."),
                               p("- Row 'Intercept' represents b0"),
                               p("- Column 'Estimate' represents each parameter cooficent"),
                               p("- Positive 'Estimate' implies poisitive impact on the dependent variable accordingly, a negative ratio means a negative impact"),
                               p("- Signiicance codes means conf interval 1-a on which this given variable can be taken as statically significant"),
                               p("- For example, 'BMO' has '*' , this means that it is significant on 1 - 00.1 level (99%)")
                               
                                       
                               
                               ),
                      tabPanel("Coefficient of determination",
                               br(),
                               p(h4("Coefficient of determination")),
                               p("Coefficient of determination, denoted R2 or r2 or 'R squared', is the proportion of the variance in the dependent variable that is predictable from the independent variable."),
                               p(withMathJax("$${\\displaystyle R^{2}\\equiv 1-{SS_{\\rm {RSS}} \\over SS_{\\rm {TSS}}}\\,}$$")),
                               p("In the best case, the modeled values exactly match the observed values, RSS = 0 and R2 = 1 - 0 = 1."),
                               p("The better the linear regression (on the right) fits the data in comparison to the simple average (on the left graph), the closer the value of R2 is to 1."),
                               
                               br(), hr(),
                               p(h4("Adjusted R2")),
                               p("Adjusted R2 is an attempt to account for the phenomenon of the R2 automatically and spuriously increasing when extra explanatory variables are added to the model."),
                               p("Adjusted R2 can be interpreted as an unbiased (or less biased) estimator of the population R2, whereas the observed sample R2 is a positively biased estimate of the population value."),
                               p(withMathJax("$${\\displaystyle {\\bar {R}}^{2}=1-(1-R^{2}){n-1 \\over n-p-1} \\mbox{, p is the total number of explanatory variables in the model, n is the sample size}}$$")),
                               
                               br(), hr(),
                               p(h4("Example")),
                               p("Examining from the same 'diabetes' dataset"),
                               p("Three variants of linear models are shown below. The first includes all the variables and has R-squared = 0.3564. The two most significant variables (BMI and Glucose) were removed from the second model, as we can see, its R-squared square is already lower = 0.07702. Similarly, from the second model, we also remove the two most significant variables SkinThickness and Age and get R-squared = 0.01322 in the third model."),
                               splitLayout(
                                   verbatimTextOutput("ols1"),
                                   verbatimTextOutput("ols2"),
                                   verbatimTextOutput("ols3")
                               )
                               ),
                      tabPanel("Predictions with linear regression",
                               br(),
                               p(h4("Predictions with linear regression")),
                               p("Knowing regression coefficients, we can estimate the y"),
                               p("The is a function 'predict' in R-studio, it can help to do it."), br(),hr(),
                               p(h4("R-simulation")),
                               
                                       p(code(" datosd = read.csv('diabettes.csv')")),
                                       p(code("linear_model = lm(Insulin ~ BloodPressure + SkinThickness + BMI + Pregnancies + Age + Glucose , data = datosd) #train lm")),
                                       p(code("test_data = data.frame(BloodPressure , SkinThickness ,BMI ,Pregnancies ,Age ,Glucose ) #make a dataframe with test values")),
                                       p(code("Insulin_predict = predict(linear_model, test_data) #substitute them to a model")),
                               br(),hr(),
                               p("Enter regessor values to predict an 'Insulin'"),
                                   splitLayout(
                                       numericInput("lm_BloodPressure", 'BloodPressure', 120, 0, 200),
                                       numericInput("lm_SkinThickness", 'SkinThickness', 30, 0, 50),
                                       numericInput("lm_BMI", 'BMI', 43, 0, 60),
                                       numericInput("lm_Pregnancies", 'Pregnancies', 2, 0, 5),
                                       numericInput("lm_Age", 'Age', 34, 0, 90),
                                       numericInput("lm_Glucose", 'Glucose', 134, 0, 250)),
                                       br(),
                                       textOutput("prediction")
                                   
                               
                               )))
                  
                  ))
                      




server <- function(input, output) {
    
    output$binomial_distribution <- renderPlot({
        n = input$binomial_distribution_attempts
        x <- seq(0,n,by = 1)
        p = input$binomial_distribution_p_of_succes/100
        y <- dbinom(x, n, p)
        plot(x,y, xlab = "Total number of successes", ylab = "P(X=x)", main = "Probability mass fuction of the Binomial distribution")
    })
    
    output$binomial_distribution_cdf <- renderPlot({
            # Create a sample of 50 numbers which are incremented by 1.
            n = input$binomial_distribution_attempts
            x <- seq(0,n,by = 1)
            p = input$binomial_distribution_p_of_succes/100
            y <- pbinom(x, n, p)
            plot(x,y, xlab = "Total number of successes", ylab = "P(X≤x)", main = "Cumulative distribution function of the Binomial distribution")
        })
    output$Binomial_mean <- renderUI({
        n = input$binomial_distribution_attempts
        prob = input$binomial_distribution_p_of_succes/100
        geom.mean <- n*prob
        geom.mean_str <- "$$np$$"
        mean = paste("Mean", withMathJax(sprintf("$$np = %.03f$$",geom.mean)))
        HTML(mean)
    })
    output$Binomial_var <- renderUI({
        n = input$binomial_distribution_attempts
        prob = input$binomial_distribution_p_of_succes/100
        geom.variance <- n*prob*(1-prob)
        var = paste("Variance", withMathJax(sprintf("$$np(1-p)= %.03f$$", geom.variance)))
        HTML(var)
    })
    
    output$geometric_distribution <- renderPlot({
        # Create a sample of 50 numbers which are incremented by 1.
        n = input$geometric_distribution_attempts
        x <- seq(0,n,by = 1)
        p = input$geometric_distribution_p_of_succes/100
        y <- dgeom(x, p)
        plot(x,y, xlab= "Number of trials until the first success", ylab = "P(X=x)", main = "Cumulative distribution fuction of the Geometric distribution")
    })
    
    output$geometric_distribution_cdf <- renderPlot({
        # Create a sample of 50 numbers which are incremented by 1.
        n = input$geometric_distribution_attempts
        x <- seq(0,n,by = 1)
        p = input$geometric_distribution_p_of_succes/100
        y <- pgeom(x, p) 
        plot(x,y ,xlab= "Number of trials until the first success", ylab = "P(X≤x)", main = "Probability mass fuction of the Geometric distribution")
        
    })
    
    output$Geometric_mean <- renderUI({
        n = input$geometric_distribution_attempts
        prob = input$geometric_distribution_p_of_succes/100
        geom.mean <- 1 / prob
        geom.mean_str <- "$$\\frac{1}{p}$$"
        mean = paste("Mean", withMathJax(sprintf("$$\\frac{1}{p} = %.03f$$",geom.mean)))
        HTML(mean)
    })
    output$Geometric_var <- renderUI({
        n = input$geometric_distribution_attempts
        prob = input$geometric_distribution_p_of_succes/100
        geom.variance <- (1 - prob) / prob ** 2
        var = paste("Variance", withMathJax(sprintf("$$\\frac{1-p}{p^2} = %.03f$$",geom.variance)))
        HTML(var)
    })
    
    output$poisson_distribution <- renderPlot({
        n = input$poisson_distribution_attempts
        x <- seq(0,n,by = 1)
        p = input$poisson_distribution_p_of_succes
        y <- dpois(x, p)
        y2 = dbinom(x, n, p/n)
        
        plot(x,y, xlab = "Total number of successes", ylab = "P(X=x)", main = "Probability mass function of the Poisson distribution")
        })
    
    output$poisson_distribution_cdf <- renderPlot({
        n = input$poisson_distribution_attempts
        x <- seq(0,n,by = 1)
        p = input$poisson_distribution_p_of_succes
        y <- ppois(x, p)
        plot(x,y, xlab = "Total number of successes", ylab = "P(X≤x)", main = "Cumulative distribution function of the Poisson distribution")
    })
    output$Poison_mean <- renderUI({
        p = input$poisson_distribution_p_of_succes
        pois.mean <- p
        mean = paste("Mean", withMathJax(sprintf("$$\\lambda = %.03f$$",pois.mean)))
        HTML(mean)
    })
    output$Poison_var <- renderUI({
        p = input$poisson_distribution_p_of_succes
        pois.var <- p
        var = paste("Variance", withMathJax(sprintf("$$\\lambda= %.03f$$", pois.var)))
        HTML(var)
    })
    output$Poisson_vc_Binomial <- renderPlot({
        
        n = input$poisson_vc_Binomial_attempts
        x <- seq(0,n,by = 1)
        p = input$poisson_vc_Binomial_p_of_succes/100
        yb <- dbinom(x, n, p)
        l= n*p
        yp <- dpois(x, l)
        sub1 = paste("Red- Binomail distribution with n = ", as.character(n), "and p =", as.character(p))
        sub2 = paste("Green- Poisson distribution with λ = ", as.character(n*p))
        sub3 = paste("Mean is equal to n*p = λ = ",  as.character(n*p))
        subres =paste(sub1, sub2, sub3, sep="\n")
        plot(x,yb, type ="l", col="red", xlab = "Total number of successes", ylab = "P(X=x)", main = "Red is binomial, green is Poisson")
        par(new=TRUE)
        plot( x, yp, type="l", col="green" , xlab ="", ylab ="", axes=FALSE)
        text(n-n/2, 0.01, sub1, col="red")
        text(n-n/2, 0.02, sub2, col="green")
        text(n-n/2, 0.03, sub3, col="black")
    })
    
    dice_plot_type_rolls <- reactive({
        if (input$dice_plot_type_rolls=='line'){
            return ('l')}
        if (input$dice_plot_type_rolls=='points'){
            return ('p')}
        if (input$dice_plot_type_rolls=='both'){
            return ('o')}
        if (input$dice_plot_type_rolls=='histogram'){
            return ('h')}
    })
    
    dice_n = reactive({input$dice_n_rolls}) 
    from = 1.0
    to = 6.0
    means = c()
    
    dice_res = reactive({
        b = input$dice_biased
        values=c()
        means = c()
        for (i in (1:dice_n()))
        {   if (b=="fair"){
            v = sample(from:to, 1)
        }
            else{
                v=sample(c(1,1,1,1,1,2,3,4,5,6), 1)
            }
            values= append(values, v)
            means = append(means,mean(values))
        }
        res = c(values, means)
        return(res)
    })
    output$dice_valuesplot <- renderPlot({
        
        end = length(dice_res())/2
        plot(dice_res()[1:end], type=dice_plot_type_rolls(), ylab="Value", xlab = "Trials", main = "Values obtained on each die roll")
        
    })
    output$dice_meansplot<- renderPlot({
        
        start = length(dice_res())/2 +1
        end = length(dice_res())
        plot(dice_res()[start:end], type = dice_plot_type_rolls(), ylim =c(0,6), ylab= "Mean", xlab = "Trials", main = "The average obtained at each index\n E = sum(values)/index")
    })
   
    output$variance_example <- renderPlot({
        
        x1 = sample(seq(-10, 10), 50, replace = TRUE)
        t1 = paste("E(red distribution) =", mean(x1), ", Var = ", round(var(x1),3))
        x2 = sample(seq(-50, 50), 50, replace = TRUE)
        t2 = paste("E(blue distribution) =", mean(x2), ", Var = ", round(var(x2),3))
        x3 = sample(seq(-80, 80), 50, replace = TRUE)
        t3 = paste("E(green distribution) =", mean(x3), " , Var =", round(var(x3),3))
        xx = data.frame("x1"= x1, "x2"=x2, "x3"= x3)
        ggplot(data = xx) + geom_histogram(aes(x=x1), alpha=0.2, col="red") + geom_histogram(aes(x=x2),alpha=0.2, col="blue")+ geom_histogram(aes(x=x3), alpha=0.2, col="green") +xlim(-100,100) +  annotate(geom ="text",x=60, y=15, label=t1, col="red") +  annotate(geom="text",x=60, y=14, label=t2, col="blue") +  annotate(geom="text",x=60, y=13, label=t3, col="green")
         })
    
    output$zero_correlation_example <- renderPlot({
        x = rnorm(1000)
        y = rnorm(1000)
        plot(x, y) + abline(lm(y~x))
    })
    
    output$perfect_corelation_example <- renderPlot({
        x = rnorm(1000)
        y = x * 0.5 + 11
        plot(x, y) + abline(lm(y~x))
    })
    
    output$perfect_negative_corelation_example <- renderPlot({
        x = rnorm(1000)
        y = -x * 1.3 
        plot(x, y) + abline(lm(y~x))
    })
    
    output$diabetes_corelation_user <- renderPlot({
        x = datosd[[input$diabetes_x]]
        y = datosd[[input$diabetes_y]]
        plot(x, y) + abline(lm(y~x))
    })
    
    output$diabetes_corelation_user_res <- renderText({
        x = datosd[[input$diabetes_x]]
        y = datosd[[input$diabetes_y]]
        c= cor(x, y)
        cov = cov(x,y)
        if (c>0.5){
            t = paste("Corelation is equal to " ,toString(c[1]),", covariance is equal to" , cov,". ", input$diabetes_x , " and " , input$diabetes_y , "has strong positive relationships.")
            return (t)
        }
        if (c>0.1)
        {t = paste("Corelation is equal to " ,toString(c[1]),", covariance is equal to" , cov,". ", input$diabetes_x , " and " , input$diabetes_y , "has weak positive relationships.")
        return (t)}
        t = paste("Corelation is equal to " ,toString(c[1]),", covariance is equal to" , cov,". ", input$diabetes_x , " and " , input$diabetes_y , "has no relationships.")
        return (t)
    }) 
    output$diabetes_data <- DT::renderDataTable(DT::datatable({datosd}))
    output$corelation_table <- renderPlot({
        M <-cor(datosd[1:8])
        corrplot(M, type="upper", order="hclust",
                 col=brewer.pal(n=8, name="RdYlBu"))
    })
    output$uniform_distribution <- renderPlot({
        
        range = input$uniform_range
        min = input$uniform_min
        max = input$uniform_max
        xvals <- data.frame(x = range) #Range for x-values
        
        ggplot(data.frame(x = xvals), aes(x = x)) + 
            
            stat_function(fun = dunif, args = list(min = min, max = max), geom = "area", 
                          fill = "green", alpha = 0.35) + 
            labs(x = "\n u", y = "f(u) \n", title = "Uniform Distribution With a = -2 and b = 2 \n") +
            geom_vline(xintercept = min,
                       linetype = "dashed", colour = "red") +
            geom_vline(xintercept = max, linetype = "dashed", colour = "red")
        })
    
    output$uniform_distribution_cdf <- renderPlot({
        
        a = input$uniform_min
        b = input$uniform_max
        n = input$uniform_range
        
        aa = as.numeric(a - abs(a/3))
        bb = as.numeric(b + b/3)
        
        x = seq(n[1], n[2],by = 0.01)
        
        y <- punif(x, min = a , max=b)
        
        plot(x,y, type = "l")
    })
    
    output$uniform_simulation <- renderPlot({
        n = input$uniform_n
        min = input$uniform_min
        max = input$uniform_max
        range = input$uniform_range
        
        unifs <- runif(n = n, min = min, max = max)
        
        ggplot(data = NULL, aes(x = unifs)) + 
            geom_histogram(binwidth = 0.25, boundary = 2) + 
            xlim(range) +
            labs(x = "\n u", y = "f(u) \n", title = "Uniform Distribution With a = -2 and b = 2 \n") +
            theme(plot.title = element_text(hjust = 0.5), 
                  axis.title.x = element_text(face="bold", colour="brown", size = 12),
                  axis.title.y = element_text(face="bold", colour="brown", size = 12))
    })
    
    output$uniform_mean <- renderUI({
        min = input$uniform_min
        max = input$uniform_max
        mean = (min+max)/2
        str_mean <- "$${\\tfrac {1}{2}}(a+b)$$"
        mean = paste("Mean", withMathJax(sprintf("$${\\tfrac {1}{2}}(a+b) = %.03f$$",mean)))
        HTML(mean)
    })
    output$uniform_var <- renderUI({
        min = input$uniform_min
        max = input$uniform_max
        var = (max-min)*(max-min)/12
        str_var <- "$${\\tfrac {1}{12}}(b-a)^{2}$$"
        var = paste("Variance", withMathJax(sprintf("$${\\tfrac {1}{12}}(b-a)^{2} = %.03f$$",var)))
        HTML(var)
    })
    
    output$exponential_distribution <- renderPlot({
        
        range = input$exponential_range
        l = input$exponential_l
        
        xvals <- data.frame(x = range) #Range for x-values
        
        ggplot(data.frame(x = xvals), aes(x = x)) + 
            
            stat_function(fun = dexp, args = (lambda=l), geom = "area", 
                          fill = "green", alpha = 0.35) + 
            labs(x = "\n u", y = "f(u) \n", title = "Exponential distribution") 
            
        })
    
    output$exponential_distribution_cdf <- renderPlot({
        # Create a sample of 50 numbers which are incremented by 1.
        l = input$exponential_l
        n = input$exponential_range
        
        x = seq(n[1], n[2],by = 0.01)
        
        y <- pexp(x, l)
        
        plot(x,y, type = "l")
    })
    
    output$exponential_simulation <-renderPlot({
        n = input$exponential_n 
        l = input$exponential_l
        range = input$exponential_range
        
        unifs <- rexp(n = n, rate= l)
        
        ggplot(data = NULL, aes(x = unifs)) + 
            geom_histogram(binwidth = 0.25, boundary = 2) + 
            xlim(range) +
            labs(x = "\n u", y = "f(u) \n", title = "Exponential distribution") +
            theme(plot.title = element_text(hjust = 0.5), 
                  axis.title.x = element_text(face="bold", colour="brown", size = 12),
                  axis.title.y = element_text(face="bold", colour="brown", size = 12))
    })
    
    output$exponential_mean <- renderUI({
        l = input$exponential_l
        mean = 1/l
        mean = paste("Mean", withMathJax(sprintf("$$\\frac{1}{\\lambda} = %.03f$$",mean)))
        HTML(mean)
    })
    output$exponential_var <- renderUI({
        var = 1/(l*l)
        var = paste("Variance", withMathJax(sprintf("$${\\displaystyle {\\frac {1}{\\lambda ^{2}}}} = %.03f$$",var)))
        HTML(var)
    })
    
    output$normal_distribution <- renderPlot({
        
        mean = input$normal_mean
        sd= input$normal_sd
        
        xvals <- data.frame(x = c(mean-4*sd*sd, mean+4*sd*sd)) #Range for x-values
        
        ggplot(data.frame(x = xvals), aes(x = x)) + 
            
            stat_function(fun = dnorm, args = list(mean=mean, sd= sd), geom = "area", 
                          fill = "green", alpha = 0.35) + 
            labs(x = "\n u", y = "f(u) \n", title = "Exponential distribution") 
        
    })
    
    output$normal_distribution_cdf <- renderPlot({
        # Create a sample of 50 numbers which are incremented by 1.
        mean = input$normal_mean
        sd= input$normal_sd
        
        x = seq(mean-4*sd*sd, mean+4*sd*sd,by = 0.01)
        
        y <- pnorm(x, mean, sd)
        
        plot(x,y, type = "l")
    })
    
    output$normal_simulation <-renderPlot({
        mean = input$normal_mean
        sd= input$normal_sd
        range = c(mean-4*sd*sd, mean+4*sd*sd)
        n = input$normal_n
        unifs <- rnorm(n = n, mean = mean, sd= sd)
        
        ggplot(data = NULL, aes(x = unifs)) + 
            geom_histogram(binwidth = 0.25, boundary = 2) + 
            xlim(range) +
            labs(x = "\n u", y = "f(u) \n", title = "Normal distribution") +
            theme(plot.title = element_text(hjust = 0.5), 
                  axis.title.x = element_text(face="bold", colour="brown", size = 12),
                  axis.title.y = element_text(face="bold", colour="brown", size = 12))
    })
    
    output$normal_mean <- renderUI({
        mean = input$normal_mean
        mean = paste("Mean", withMathJax(sprintf("$$\\mu = %.03f$$",mean)))
        HTML(mean)
    })
    output$normal_variance <- renderUI({
        var = input$normal_sd
        var = var*var
        var = paste("Variance", withMathJax(sprintf("$$\\sigma ^{2} = %.03f$$",var)))
        HTML(var)
    })
    
    output$avocado_data <- DT::renderDataTable(DT::datatable({datos}))
    output$avocado_prices_plot <- renderPlot(
        plot(datos$AveragePrice, type ='l', x_lab ="avocados index", y_lab = "avocados price", main = 'plot of prices')
    )
    #histogram 
    output$avocado_prices_hist <- renderPlot({
        type = input$statistics_visualisation_type
        if (type == "histogram"){
            
            ggplot(datos, aes(AveragePrice)) + geom_histogram() +xlab("prices")+ ylab('count')
        }
        else if (type == "frequency polygon"){
            
            ggplot(datos, aes(AveragePrice)) + geom_freqpoly() +xlab("prices")+ ylab('count')
        }
        else if (type == "plot a table"){
            
            plot(datos$AveragePrice)
        }
        else if (type == "area plot"){
            
            ggplot(datos, aes(AveragePrice)) + geom_area(stat ="bin") +xlab("prices")+ ylab('count')
        }
        else if (type == "density plot"){
            
            ggplot(datos, aes(AveragePrice)) + geom_density(kernell="gaussian")  +xlab("prices")+ ylab('relative frequency')
        }
    })
    
    output$avocado_prices_hist_with_mmm <- renderPlot(
        ggplot(datos, aes(x = AveragePrice)) + geom_histogram() + xlab("prices") + ylab('count') +
            geom_vline(xintercept=mean(datos$AveragePrice), color = "red") +
            geom_vline(xintercept=median(datos$AveragePrice), color = "blue") +
            geom_vline(xintercept=mode_avocado_prices, color = "green"))
    
    output$avocado_prices_hist_with_quartiles <- renderPlot({
        ggplot(datos, aes(x = AveragePrice)) + geom_histogram() + xlab("prices") + ylab('count') +
            geom_vline(xintercept=avocado_prices_q1, color = "red" ) +
            geom_vline(xintercept=avocado_prices_q2, color = "red") +
            geom_vline(xintercept=avocado_prices_q3, color = "red")+
        geom_text( aes(x=avocado_prices_q1, y=0, label="Q1"))+
            geom_text( aes(x=avocado_prices_q2, y=0, label="Q2"))+
            geom_text( aes(x=avocado_prices_q3, y=0, label="Q3"))
        })
    
    output$avocado_prices_hist_with_percentile <- renderPlot({
        percentile = input$percentile_n/100
        p = quantile(datos$AveragePrice, percentile)
        ggplot(datos, aes(x = AveragePrice)) + geom_histogram() + xlab("prices") + ylab('count') +
            geom_vline(xintercept=p, color = "black") }) 
    
    output$avocado_prices_hist_with_skewness <- renderPlot({
        
        ggplot(datos, aes(x = AveragePrice)) + xlab("prices") + ylab('count') + geom_histogram(aes(y = ..density..), fill='white', color = "grey30")+
            geom_density(alpha= 0.5) + stat_function(fun = dnorm, color='red', args = list(mean =mean(datos$AveragePrice), sd=sd(datos$AveragePrice)))
    }) 
    
    output$avocado_prices_skewness <- renderText({
        
        skewness = skewness(datos$AveragePrice)
        kurtosis = kurtosis(datos$AveragePrice)
        result = paste("skewness =", skewness, ", kurtosis = ", kurtosis)
        
    })
    
    output$makemenormal_plot <- renderPlot({
        
        sc = input$makemenormal_sc
        ku = input$makemenormal_ku +3
        mean = input$makemenormal_mean
        sd= input$makemenormal_sd
        moments <- c(mean = mean,variance = sd,skewness =sc, kurtosis = ku)
        p = rpearson(1000, moments = moments)
        hist(p)
    })
    
    output$kurtosis <- renderImage({
        list(
            src = "/kurtosis.png",
            contentType = "image/png",
            alt = "Face"
        )
        
    }, deleteFile = FALSE)
    
    output$plot_types_description <- renderText({
        type = input$statistics_visualisation_type
        if (type == "histogram"){
            
            ">> ggplot(datos, aes(AveragePrice)) + geom_histogram() +xlab('prices')+ ylab('count')"
        }
        else if (type == "frequency polygon"){
            
            ">> ggplot(datos, aes(AveragePrice)) + geom_freqpoly() +xlab('prices')+ ylab('count')"
        }
        else if (type == "plot a table"){
            
            ">> plot(datos$AveragePrice)"
        }
        else if (type == "area plot"){
            
            ">> ggplot(datos, aes(AveragePrice)) + geom_area(stat ='bin'') +xlab('prices')+ ylab('count')"
        }
        else if (type == "density plot"){
            
            ">> ggplot(datos, aes(AveragePrice)) + geom_density(kernell='gaussian')  +xlab('prices')+ ylab('relative frequency'')"
        }
    })
    output$ecdf <- renderPlot ({
        n = input$ecdf_n
        x = seq(-10, 10, by = 0.01)
        plot(ecdf(rnorm(n , 0, 1)), xlim = c(-5,5))
        lines(x, pnorm(x, 0, 1 ), type="l", col = "red")
    })
    output$coin_estimator <- renderPlot ({
        n = input$coin_n
        list_of_val = c()
        for (i in (1:n)){
            val = sample(c(0,1), 1)
            list_of_val = append(list_of_val, val)
        }
        xl= paste("values = ", paste(as.character(list_of_val) , collapse=" "))
        hist(list_of_val,xlab= xl) 
        abline(v=0.5, col="red")
        abline(v=mean(list_of_val), col="green")
    })
    
    output$mle_binomial <- renderPlot({
        type = input$mle_log
        h = input$mle_h
        t = input$mle_total
        p.parameter <- h/t
        sequence <- rbinom(t, 1, p.parameter)
        
        likelihoodd <- function(sequence, p.parameter){
            likelihood <- 1
            for (i in 1:length(sequence)){
                if (sequence[i] == 1){ 
                    likelihood <- likelihood * p.parameter}
                else {
                    likelihood <- likelihood * (1 - p.parameter)}}
            return(likelihood)
        }
        
        log.likelihood <- function(sequence, p){
            log.likelihood <- 0
            for (i in 1:length(sequence)){
                if (sequence[i] == 1){
                    log.likelihood <- log.likelihood + log(p)}
                else {
                    log.likelihood <- log.likelihood + log(1 - p)}}
            return(log.likelihood)}
        
        possible.p <- seq(0, 1, by = 0.001)
        
        if (type == "likelihood"){
            qplot(possible.p,
                  sapply(possible.p, function (p) {likelihoodd(sequence, p)}),
                  geom = 'line',
                  main = 'Likelihood as a Function of P',
                  xlab = 'P',
                  ylab = 'Likelihood')
        }
        else {
            qplot(possible.p,
                  sapply(possible.p, function (p) {log.likelihood(sequence, p)}),
                  geom = 'line',
                  main = 'Log Likelihood as a Function of P',
                  xlab = 'P',
                  ylab = 'Log Likelihood')
        }
    })
    
    output$mle_Binomial_res <- renderText({
        type = input$mle_log
        h = input$mle_h
        t = input$mle_total
        p.parameter <- h/t
        sequence <- rbinom(t, 1, p.parameter)
        
        likelihoodd <- function(sequence, p.parameter){
            likelihood <- 1
            for (i in 1:length(sequence)){
                if (sequence[i] == 1){ 
                    likelihood <- likelihood * p.parameter}
                else {
                    likelihood <- likelihood * (1 - p.parameter)}}
            return(likelihood)
        }
        
        log.likelihood <- function(sequence, p){
            log.likelihood <- 0
            for (i in 1:length(sequence)){
                if (sequence[i] == 1){
                    log.likelihood <- log.likelihood + log(p)}
                else {
                    log.likelihood <- log.likelihood + log(1 - p)}}
            return(log.likelihood)}
        
        possible.p <- seq(0, 1, by = 0.001)
        if (type =="likelihood"){
            mle.results <- optimize(function(p) {likelihoodd(sequence, p)},
                                    interval = c(0, 1),
                                    maximum = TRUE)
        }
        else {
            mle.results <- optimize(function(p) {log.likelihood(sequence, p)},
                                    interval = c(0, 1),
                                    maximum = TRUE)
        }
        
        val = as.character(mle.results[1])
        paste("Function gets mximum at p = " , val)
    })
    
    output$student_distribution <- renderPlot({
        normal = input$student_distribution_normal
        df = as.numeric(input$student_distribution_n)
        distribution = rt(100000, df)
        normal_distribution = rnorm(100000, 0 , 1)
        d = as.data.frame(distribution, normal_distribution)
        if (normal == TRUE){
            ggplot(data = d, aes(x=distribution)) + geom_density(aes(y=..density..), kernel = "gaussian", adjust = 10) + geom_density(aes(x =normal_distribution, y=..density..), kernel = "gaussian", color = "red", adjust = 10) + xlim(-5,5)
        }
        else{
            ggplot(data = d, aes(x=distribution)) + geom_density(aes(y=..density..), kernel = "gaussian", adjust = 10) +xlim(-5,5)
            
        }
    })
    output$chi_squared <- renderPlot({
        n = as.numeric(input$chi_squared_n)
        distribution = rchisq(10000, n)
        d = as.data.frame(distribution)
        ggplot(data = d, aes(x = distribution)) + geom_density(aes(y=..density..), kernel = "gaussian", adjust = 10) 
    })
    
    output$diabetes_g_i <- renderPlot({
        plot(datosd$Glucose, datosd$Insulin, xlab = "Glucose", ylab = "Insulin")
        abline(lm(Insulin ~ Glucose, data = datosd))
    })
    output$ols <- renderPrint({
        linear_model = lm(Insulin ~ BloodPressure + SkinThickness + BMI + Pregnancies + Age + Glucose , data = datosd)
        summary(linear_model)
    })
    output$ols1 <- renderPrint({
        linear_model = lm(Insulin ~ BloodPressure + SkinThickness + BMI + Pregnancies + Age + Glucose , data = datosd)
        summary(linear_model)
    })
    output$ols2 <- renderPrint({
        linear_model = lm(Insulin ~ BloodPressure + SkinThickness + Pregnancies + Age , data = datosd)
        summary(linear_model)
    })
  
    output$ols3 <- renderPrint({
        linear_model = lm(Insulin ~ BloodPressure +  Pregnancies , data = datosd)
        summary(linear_model)
    })
 
    output$prediction <- renderText({
        BloodPressure = input$lm_BloodPressure
        SkinThickness = input$lm_SkinThickness
        BMI = input$lm_BMI
        Pregnancies = input$lm_Pregnancies
        Age = input$lm_Age
        Glucose = input$lm_Glucose
        linear_model = lm(Insulin ~ BloodPressure + SkinThickness + BMI + Pregnancies + Age + Glucose , data = datosd)
        test_data = data.frame(BloodPressure , SkinThickness ,BMI ,Pregnancies ,Age ,Glucose )
        Insulin_predict = predict(linear_model, test_data)
        paste("Predicted Insulin is equal to", Insulin_predict)
    })
    
    output$conf_interval <- renderPlot ({
        input$gobutton2
        mean = input$conf_interval_mean
        n =  input$conf_interval_n
        sd = input$conf_interval_sd
        a = input$conf_interval_a
        if (a == 80){
            z = 1.282}
        else if (a==90){
            z = 1.645
        }
        else if (a==95){
            z = 1.960
        }
        else{
            z = 2.576
        }
        
        kek = rnorm(n , mean, sd)
        kekk = as.data.frame(kek)
        p = quantile(kekk$kek, 5/100/2)
        pp = quantile(kekk$kek, 97.5/100)
        
        estimated_mean = mean(kek)
        pmr = estimated_mean + z*sd/sqrt(as.numeric(n))
        pml = estimated_mean - z*sd/sqrt(as.numeric(n))
        
        ggplot(kekk , aes(x= kek)) + geom_histogram() + geom_vline(xintercept=p, size=0.5, color="red") + geom_vline(xintercept=pp, size=0.5, color="red") + geom_vline(xintercept=pmr, size=0.5, color="green")+
            geom_vline(xintercept=pml, size=0.5, color="green") +  geom_vline(xintercept=estimated_mean, size=0.5, color="blue") +xlim(-100, 100)
    })
    
    output$conf_interval_without_v <- renderPlot({
        input$gobutton3
        mean = input$conf_interval_mean_without
        n =  as.numeric(input$conf_interval_n_without)
        sd = runif(1, 0 , 20)
        a = input$conf_interval_a_without
        if (a == 80){
            z = 1.282}
        else if (a==90){
            z = 1.645
        }
        else if (a==95){
            z = 1.960
        }
        else{
            z = 2.576
        }
        kek = rnorm(n , mean, sd)
        estimated_mean = mean(kek)
        estimated_sd = sd(kek)*sqrt((n-1)/n)
        
        kekk = as.data.frame(kek)
        p = quantile(kekk$kek, 5/100/2)
        pp = quantile(kekk$kek, 97.5/100)
        pmr = estimated_mean + z*estimated_sd/sqrt(as.numeric(n))
        pml = estimated_mean - z*estimated_sd/sqrt(as.numeric(n))
        
        ggplot(kekk , aes(x= kek)) + geom_histogram() + geom_vline(xintercept=p, size=0.5, color="red") + geom_vline(xintercept=pp, size=0.5, color="red") + geom_vline(xintercept=pmr, size=0.5, color="green")+
            geom_vline(xintercept=pml, size=0.5, color="green") +  geom_vline(xintercept=estimated_mean, size=0.5, color="blue") + xlim(-100,100)
    })
    
    normal_for_z_test <- reactive({
      n = as.numeric(input$z_test_n)
      mean = as.numeric(input$z_test_mean)
      sd = as.numeric(input$z_test_sd)
      dist = rnorm(n, mean , sd)
      
      dist
    })
    
    output$z_test_plot <-renderPlot({
      dist = normal_for_z_test()
      d = as.data.frame(dist)
      ggplot(data = d, aes(x=dist)) + geom_histogram(aes(y = ..density..))
    })
    output$z_test_summary <- renderPrint({
      dist = normal_for_z_test()
      a = as.numeric(input$z_test_a)/100
      mu =  as.numeric(input$z_test_mu)
      method = input$z_test_method
      sd = as.numeric(input$z_test_sd)
      z = z.test(dist, y = NULL, alternative = method, mu = mu, sigma.x = sd, conf.level = a)
      z
    })
    
    normal_for_z_test21 <- reactive({
      n = as.numeric(input$z2_test_n1)
      mean = as.numeric(input$z2_test_mean1)
      sd = as.numeric(input$z2_test_sd1)
      dist = rnorm(n, mean , sd)
      
      dist
    })
    normal_for_z_test22 <- reactive({
      n = as.numeric(input$z2_test_n2)
      mean = as.numeric(input$z2_test_mean2)
      sd = as.numeric(input$z2_test_sd2)
      dist = rnorm(n, mean , sd)
      
      dist
    })
    
    output$z2_test_plot <-renderPlot({
      dist1 = normal_for_z_test21()
      dist2 = normal_for_z_test22()
      length(dist1) = length(dist2)
      length(dist2) = length(dist1)
      d = as.data.frame(dist1, dist2)
      
     ggplot(data = d, aes(x=dist1)) + geom_histogram(aes(y = ..density..), col="green") + geom_histogram(aes(x=dist2, y = ..density..), col="red")
   
      })
    output$z2_test_summary <- renderPrint({
      dist1 = normal_for_z_test21()
      dist2 = normal_for_z_test22()
      a = as.numeric(input$z_test_a)/100
      method = input$z_test_method
      sd1 = as.numeric(input$z2_test_sd1)
      sd2 = as.numeric(input$z2_test_sd2)
      z = z.test(x = dist1, y = dist2, alternative = method,  sigma.x = sd1,  sigma.y = sd2, conf.level = a)
      z
    })
    normal_for_t_test <- reactive({
      n = as.numeric(input$t_test_n)
      mean = as.numeric(input$t_test_mean)
      sd = as.numeric(input$t_test_sd)
      dist = rnorm(n, mean , sd)
      
      dist
    })
    
    output$t_test_plot <-renderPlot({
      dist = normal_for_t_test()
      d = as.data.frame(dist)
      ggplot(data = d, aes(x=dist)) + geom_histogram(aes(y = ..density..))
    })
    output$t_test_summary <- renderPrint({
      dist = normal_for_t_test()
      a = as.numeric(input$t_test_a)/100
      mu =  as.numeric(input$t_test_mu)
      method = input$t_test_method
      sd = as.numeric(input$t_test_sd)
      t = t.test(dist, y = NULL, alternative = method, mu = mu, sigma.x = sd, conf.level = a)
      t
    })
    
    normal_for_t_test21 <- reactive({
      n = as.numeric(input$t2_test_n1)
      mean = as.numeric(input$t2_test_mean1)
      sd = as.numeric(input$t2_test_sd1)
      dist = rnorm(n, mean , sd)
      
      dist
    })
    normal_for_t_test22 <- reactive({
      n = as.numeric(input$t2_test_n2)
      mean = as.numeric(input$t2_test_mean2)
      sd = as.numeric(input$t2_test_sd2)
      dist = rnorm(n, mean , sd)
      
      dist
    })
    
    output$t2_test_plot <-renderPlot({
      dist1 = normal_for_t_test21()
      dist2 = normal_for_t_test22()
      length(dist1) = length(dist2)
      length(dist2) = length(dist1)
      d = as.data.frame(dist1, dist2)
      
      ggplot(data = d, aes(x=dist1)) + geom_histogram(aes(y = ..density..), col="green") + geom_histogram(aes(x=dist2, y = ..density..), col="red")
      
    })
    output$t2_test_summary <- renderPrint({
      dist1 = normal_for_t_test21()
      dist2 = normal_for_t_test22()
      a = as.numeric(input$t_test_a)/100
      method = input$t_test_method
      sd1 = as.numeric(input$t2_test_sd1)
      sd2 = as.numeric(input$v2_test_sd2)
      t = t.test(x = dist1, y = dist2, alternative = method,  sigma.x = sd1,  sigma.y = sd2, conf.level = a)
      t
    })
  
    output$binomial_negative <- renderPlot({
      n = as.numeric(input$binomial_negative_n)
      x <- seq(0,100,by = 0.1)
      p = input$binomial_negative_p/100
      y <- dnbinom(x, n, p)
      plot(x,y, xlab = "Times",type="p", ylab = "P(X=x)", main = "Probability mass fuction of the Negative Binomial distribution")
    })
    
    output$binomial_negative_cdf <- renderPlot({
      # Create a sample of 50 numbers which are incremented by 1.
      n = input$binomial_negative_n
      x <- seq(0,100,by = 0.1)
      p = input$binomial_negative_p/100
      y <- pnbinom(x, n, p)
      plot(x,y, xlab = "Times",type="p", ylab = "P(X≤x)", main = "Cumulative distribution function of the Negative Binomial distribution")
    })
    output$binomial_negative_mean <- renderUI({
      n = input$binomial_negative_n
      prob = input$binomial_negative_p/100
      geom.mean <- n*prob/(1-prob)
      geom.mean_str <- "$$np$$"
      mean = paste("Mean", withMathJax(sprintf("$$k*p/(1-p) = %.03f$$",geom.mean)))
      HTML(mean)
    })
    output$binomial_negative_var <- renderUI({
      n = input$binomial_negative_n
      prob = input$binomial_negative_p/100
      geom.variance <- n*prob/((1-prob)*(1-prob)) 
      var = paste("Variance", withMathJax(sprintf("$${\\frac {pk}{(1-p)^{2}}}= %.03f$$", geom.variance)))
      HTML(var)
    })
    output$gamma <- renderPlot({
      n = as.numeric(input$gamma_n)
      x <- seq(0,100,by = 0.1)
      p = input$gamma_p
      y <- dgamma(x, n, rate = p)
      plot(x,y, xlab = "Times",type="p", ylab = "P(X=x)", main = "Probability mass fuction of the Negative Binomial distribution")
    })
    
    output$gamma_cdf <- renderPlot({
      # Create a sample of 50 numbers which are incremented by 1.
      n = input$gamma_n
      x <- seq(0,100,by = 0.1)
      p = input$gamma_p
      y <- pgamma(x, n, rate = p)
      plot(x,y, xlab = "Times",type="p", ylab = "P(X≤x)", main = "Cumulative distribution function of the Negative Binomial distribution")
    })
    output$gamma_mean <- renderUI({
      n = input$gamma_n
      prob = input$gamma_p
      geom.mean <- n*prob
      geom.mean_str <- "$${\\displaystyle k\\theta }$$"
      mean = paste("Mean", withMathJax(sprintf("$${\\displaystyle k\\theta } = %.03f$$",geom.mean)))
      HTML(mean)
    })
    output$gamma_var <- renderUI({
      n = input$gamma_n
      prob = input$gamma_p
      geom.variance <- n*prob*prob 
      var = paste("Variance", withMathJax(sprintf("$${\\displaystyle k\\theta ^{2}}= %.03f$$", geom.variance)))
      HTML(var)
    })
    
    output$random_walk <- renderPlot({
      # random walk 
      # simulation 
      cur_place  = input$random_walk_y0
      p = input$random_walk_p
      n = input$random_walk_n
      x_plus = 1
      x_minus = -1
      steps = c()
      q = 1 - p
      for ( i in 1:n) {
        step = sample(c(x_plus, x_minus), size = 1,  prob = c(p, q), replace = TRUE)
        cur_place = step + cur_place
        steps = append(steps, cur_place)
      }
      plot(steps, type = 'l')
    }) 
    
    output$wiener_process <- renderPlot({
      n = input$wiener_process_n
      var = 1
      dis = rnorm(n, 0, var)
      dis = cumsum(dis)
      plot(dis, type= "l",main= "Brownian Motion in One Dimension", xlab="time", ylab="displacement")
    })
    
    output$markov_plot <- renderPlot({
      x = input$markov_plot_initial_x
      y = input$markov_plot_initial_y
      p_from_x_to_y = input$markov_p_from_x_to_y
      p_from_y_to_x = input$markov_p_from_y_to_x
      n = input$makov_n_of_simulations
      
      a_x = c(x)
      a_y= c(y)
      
      for (i in 1:n){
        new_x = (1-p_from_x_to_y)*x + p_from_y_to_x*y
        new_y = p_from_x_to_y*x + y*(1-p_from_y_to_x)
        a_x= append(a_x, new_x)
        a_y = append(a_y, new_y)
        x = new_x
        y = new_y
      }
      
      dates = 1:(n+1)
      
      df <- data.frame(dates,a_x,a_y)
      ggplot(df,  aes(dates)) +                    # basic graphical object
        geom_line(aes(y= a_y), colour="red") +  # first layer
        geom_line(aes(y=a_x), colour="green") +ylab("Number of units") +xlab("N-stage") # second layer
    })
    
    tmat <- reactive({
      input$gobutton
      tmat <- matrix(rnorm(16)^2, ncol=4) 
      tmat <- tmat/rowSums(tmat)
      return(tmat)
    })
    
    markov_plot_ar <- reactive({
      kek = input$markov_plot2_rebuilt
      start = input$markov_initial_state
      if (start == 1) {
        v = c(1,0,0,0)
      }
      if (start == 2){
        v = c(0,1,0,0)
      }
      if (start == 3){
        v = c(0,0,1,0)
      }
      if (start == 4){
        v = c(0,0,0,1)
      }
      y  = array(dim = c(21,4))
      for (i in 1:20){
        y[i, ] = matrix(v, ncol=4) %*% (tmat() %^% i)
      }
      y[21, ] = matrix(v, ncol=4) %*% (tmat() %^% 1000)
      return (y)
    })
    
    output$markov_plot2 <- renderPlot({
      matplot(markov_plot_ar(), ylim=c(0,1), type = "l", xlab="state 1-black, state 2- blue, state 3- green, state 4-red", ylab ="Probability to be in state")
      
    })
    
    output$markov_plot2_info <- renderText({
      n = input$markov_plot2_n
      val1 = as.character(markov_plot_ar()[n,1])
      val2 = as.character(markov_plot_ar()[n,2])
      val3 = as.character(markov_plot_ar()[n,3])
      val4 = as.character(markov_plot_ar()[n,4])
      n = as.character(n)
      paste("On the " , n , "th step" , "probability to be in state 1 = ", val1 ,"in state 2 = ", val2, "in state 3 = ", val3, "in state 4 = ", val4)
    })
    output$markov_plot2_limiting <- renderText({
      val1 = as.character(markov_plot_ar()[21,1])
      val2 = as.character(markov_plot_ar()[21,2])
      val3 = as.character(markov_plot_ar()[21,3])
      val4 = as.character(markov_plot_ar()[21,4])
      paste("Limiting probability to be in state 1 = ", val1 ,"in state 2 = ", val2, "in state 3 = ", val3, "in state 4 = ", val4)
    })
    
    output$markov_plot2_table <- renderTable ({
      tmat()
    })
    
    ev <- reactive ({
      eigen(tmat())
    })
    output$markov_plot2_eigenvalues <- renderText ({
      values = ev()$values 
      values = as.character(values)
      paste("Eigenvalues are equal to " , values[1], ", " , values[2] , ", " , values[3] , ", ", values[4])
    })
    
    output$markov_plot2_eigenvectors <- renderTable ({
      vectors = ev()$vectors
      
      as.character(vectors)
    })
    
    output$markov_plot2_ev_limit <- renderPlot ({
      ev_powers = array( dim = c(100, 4))
      for (i in 1:100){
        ev_powers[i, ] =  (ev()$values)^i
      }
      matplot(ev_powers, type = "l")
    })
    
    
    output$markov_inequality1 <- renderPlot({
      lambda = input$Markov_l
      n = as.numeric(input$Markov_scale)
      x = seq(0, n , by = 1)
      y = 1 - ppois(x, lambda)
      yy = rpois(x, lambda)
      d = as.data.frame(x, y)
      mean = mean(yy) / x
      ggplot(d, aes(x, y)) + geom_line() + geom_line(aes(y=mean), col = "red") 
    })
    
    output$markov_inequality2 <- renderPlot({
      probability = input$Markov_p
      n = as.numeric(input$Markov_scale)
      x = seq(0, n, by = 1)
      y = 1 - pbinom(x,n, probability)
      yy = rbinom(x, n, probability)
      d = as.data.frame(x, y)
      mean = mean(yy) / x
      ggplot(d, aes(x, y)) + geom_line() + geom_line(aes(y=mean), col = "red") 
    })
    
    output$chebishev_inequality1 <- renderPlot({
      lambda = input$chebishev_l
      x = seq(0, 20, by = 0.5)
      y = 1 - ppois(x, lambda)
      yy = rpois(x, lambda)
      d = as.data.frame(x, y)
      mean = mean(yy) / x
      ggplot(d, aes(x, y)) + geom_line() + geom_line(aes(y=mean), col = "red") 
    })
    
    output$chebishev_inequality2 <- renderPlot({
      probability = input$chebishev_p/100
      x = seq(0, 20, by = 0.5)
      y = 1 - pbinom(x, 100, probability)
      yy = rbinom(x, 100, probability)
      d = as.data.frame(x, y)
      mean = mean(yy) / x
      ggplot(d, aes(x, y)) + geom_line() + geom_line(aes(y=mean), col = "red") 
    })
    
    
    output$wlln_t <- renderText ({
      mean = input$wlln_mean
      var = input$wlln_var
      n = as.numeric(input$wlln_n)
      sample_mean = as.character(mean(rnorm(n, mean,var)))
      
      mean = as.character(mean)
      result = paste("True mean is equal to" ,  mean,". Sample mean is equal to ", sample_mean)
    })
    
    output$wlln_plot <- renderPlot({
      mean = input$wlln_mean
      var = input$wlln_var
      n = input$wlln_n
      sample = seq(1, n , by = 1)
      list_of_dif = c()
      for (i in sample) {
        sample_mean = mean(rnorm(i , mean , var))
        dif = abs(mean - sample_mean)
        list_of_dif = append(list_of_dif, dif)
      }
      
      l = as.data.frame(list_of_dif)
      
      ggplot(l , aes(x=list_of_dif)) + 
        geom_line(aes(y = 1 - ..y..), stat='ecdf') + ylab("P |Mn − μ| ≥ ε ") + xlab("n")
      
    })
    output$wlln_plot2 <- renderPlot({
      mean = input$wlln_mean
      var = input$wlln_var
      n = input$wlln_n
      sample = seq(1, n , by = 1)
      list_of_dif = c()
      for (i in sample) {
        sample_mean = mean(rnorm(i , mean , var))
        dif = abs(mean - sample_mean)
        list_of_dif = append(list_of_dif, dif)
      }
      plot(list_of_dif, ylab = "Mn − μ",xlab = "n")
    })
    
    output$guessing_a_coin <- renderPlot({
      h = input$guessing_a_coin_n_h
      p1 = input$guessing_a_coin_p1
      p2 = input$guessing_a_coin_p2
      pc1 = input$guessing_a_coin_pc
      pc2 = 1-pc1
      p1 = seq(p1, p1+1)
      p2 = seq(p2, p2+1)
      abline = seq(0, 1)
      sample = seq(0, 1 , 0.01)
      data = c()
      for (i in sample){
        value = (1-i)^(10-h)*(i^(h))
        data = append(data,value )
      }
      data1 = data*pc1
      data2 = data*pc2
      matplot(sample , cbind(data1, data2), type = 'l', xlab = "H got", ylab = "P(x=H)") + lines(p1, abline) + lines(p2, abline, col = "red")
    })
    
    output$guessing_a_coin_inf <- renderText({
      h = input$guessing_a_coin_n_h
      p1 = input$guessing_a_coin_p1
      p2 = input$guessing_a_coin_p2
      pc1 = input$guessing_a_coin_pc
      pc2 = 1-pc1
      
      posterior1 = pc1* ((1-p1)^(10-h))*(p1^h)
      posterior2 = pc2* ((1-p2)^(10-h))*(p2^h)
      if (posterior1>posterior2){
        paste( "Posterior distribution of the first variable =", as.character(posterior1),"Posterior distribution of the second variable =", as.character(posterior2), "It is more likely that we pulled the first coin")
      }
      else {
        paste( "Posterior distribution of the first variable =", as.character(posterior1),"Posterior distribution of the second variable =", as.character(posterior2), "It is more likely that we pulled the second coin")
        
      }
    })
    
    
    
}


shinyApp(ui = ui, server = server)
