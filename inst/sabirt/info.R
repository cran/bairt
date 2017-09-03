.intro <- function(){

  div(
    br(),br(),

    img(src = "SABIRT22.png", height = 150, width = 350),

    br(),br(),

    h1("SABIRT", style = "color:blue"),

    h4("(Shiny App for Bayesian Item Response Theory)"),

    p("This is a web interactive application intended for the
      making of an MCMC (Markov Chain Monte Carlo Methods)
      estimation and model-fit of the item response models
      designed by Johnson and Albert (2pno, 1999) and Glas
      and Beguim (3pno, 2001). The outcome are the items
      parameters (difficulties and discrimination for 2pno,
      and additionally the chance to guess the right answers
      for 3pno) and also the latent abilities of each examinee."),

    p("We hope you enjoy SABIRT as this is useful in the study
      of dichotomous items response models."),

    br(),

    p("The authors."),

    br(),

    fluidRow(

      column(1,""),

      column(6,
             p(strong("Javier Martinez")),
             #p(code("Contact author"), "martinezjavier243@gmail.com"),
             p("Department of Scientific Computing and Statistics"),
             p("Caracas - Venezuela"),

             br(),

             p(strong("Irene Garcia Mosquera")),
             p("Department of Mathematics and Informatics"),
             p("Mallorca - Spain")

      ),

      column(4,
             #br(),
             img(src = "USB.png", height = 65, width = 175),

             br(),br(),br(),

             img(src = "UIB.png", height = 70, width = 200)

      )

    )

    )

}

###############################################################################
###############################################################################
.mcmc2pnoInfo <- function(){

  div(

    h3("Two-Parameter Normal Ogive Item Response Model (2pno)"),

    withMathJax(

      helpText(
        "This function estimates the Two-Parameter normal ogive item
        response model by MCMC (Markov Chain Monte Carlo Methods)
        sampling (Johnson and Albert (1999))."
      ),

      h3("Details:"),

      helpText(
        "For the two-parameter normal ogive item response model, we
        assume that the performance of the", em("i-th"),
        "subject depends on a latent variable \\(\\theta_{i}\\). If
        \\(\\theta_{1}\\), ..., \\(\\theta_{n}\\), denote the
        latent traits for the individuals 1, ...,", em("n"),
        ", we assume that the probability that the",  em("i-th"),
        "subject responds correctly the", em("j-th"),
        "item can be modeled as:
        $$Pr( Y_{ ij } = 1 | \\theta_{i}, a_{j}, b_{j} ) =
        \\Phi( a_{j}\\theta_{i} - b_{j} )$$
        where \\(\\Phi\\), is the standard normal cdf,
        and \\(a_{j}\\) and \\(b_{j}\\), are the discrimination
        and the difficulty parameters associated with the",
        em("j-th"), "item respectively."
      ),

      helpText(
        "Regarding the convergence of the chains,
        we used the Potential Scale Reduction Factor (",
        code("Rhat"), ") proposed by Gelman, Carlin, Stern
        and Rubin (2004).
        Values of Rhat are substantially above 1 indicate
        lack of convergence. But in the practice, values
        under 1.1 are large enough to assume convergence."
        ),

      helpText(
        "Specifically, SABIRT generate a single MCMC and
        evaluate convergence by partitioning  breaking
        the chain in three sub chains and comparing the
        between- and within-subchain variance."
      ),

      helpText("The SABIRT MCMC code was adapted from an R
               script of Alexander Robitzsch."
              )
      ),

    h3("References"),

    p(
      a("Gelman, A., Carlin, J. B., Stern, H. S. & Rubin, B. (2004).
        Bayesian Data Analysis.New York: Chapman & Hall/CRC.",
        href =
          "https://www.crcpress.com/Bayesian-Data-Analysis-
        Third-Edition/Gelman-Carlin-Stern-Dunson-Vehtari-Rubin/p/
        book/9781439840955")
      ),

    p(
      a("Johnson, V. E., & Albert, J. H. (1999).
        Ordinal Data Modeling. New York: Springer.",
        href = "http://www.springer.com/gp/book/9780387987187")
    )

    )

}

###############################################################################
###############################################################################
.mcmc3pnoInfo <- function(){

  div(

    h3("Three-Parameter Normal Ogive Item Response Model (3pno)"),

    withMathJax(

      helpText(
        "This function estimates the Three-Parameter normal ogive
        item response model by MCMC (Markov Chain Monte Carlo Methods)
        sampling (Beguin and Glas (2001) )."
      ),

      h3("Details:"),

      helpText(
        "For the three-parameter normal ogive item response model,
        we assume that the performance of the", em("i-th"),
        "subject depends on a latent variable \\(\\theta_{i}\\).
        If  \\(\\theta_{1}\\), ..., \\(\\theta_{n}\\), denote the
        latent traits for the individuals 1, ...,", em("n"),
        ", we assume that the probability that the", em("i-th"),
        "subject responds correctly the", em("j-th"), "item can be
        modeled as:
        $$Pr( Y_{ ij } = 1 | \\theta_{i}, a_{j}, b_{j},  c_{j}  ) =
        c_{j} + (1 - c_{j} )\\Phi( a_{j}\\theta_{i} - b_{j} )$$
        where \\(\\Phi\\), is the standard normal cdf, and
        \\(a_{j}\\), \\(b_{j}\\) and \\(c_{j}\\), are the item
        discrimination, item difficulty and item guessing parameters
        associated with the", em("j-th"), "item."
      ),


      helpText(
        "Regarding the convergence of the chains,
        we used the Potential Scale Reduction Factor (",
        code("Rhat"), ") proposed by Gelman, Carlin, Stern
        and Rubin (2004).
        Values of Rhat substantially above 1 indicate
        lack of convergence. But in practics,
        values under 1.1 is sufficient for assuming convergence."
        ),

      helpText(
        "Specifically, SABIRT generates a single MCMC and evaluates
        convergence by dividing the chain into three sub chains
        and comparing the between- and within-subchain variance."
      ),

      helpText("The SABIRT MCMC code was adapted from an R
               script of Alexander Robitzsch."
      )

      ),

    h3("References"),

    p(
      a("Beguin, A, A. & Glas, C.A.W. (2001). MCMC Estimation
        and Some Model-Fit Analysis of Multidimensional IRT Models.
        Psychometrika, 66, 541-562.",
        href = "https://link.springer.com/article/10.1007/BF02296195")
    ),

    p(
      a("Gelman, A., Carlin, J. B., Stern, H. S. & Rubin, B. (2004).
        Bayesian Data Analysis.New York: Chapman & Hall/CRC.",
        href = "https://www.crcpress.com/Bayesian-Data-
        Analysis-Third-Edition/Gelman-Carlin-Stern-Dunson-Vehtari-
        Rubin/p/book/9781439840955")
      ),

    p(
      a("Harwell, M. R, & Baker, F. B. (1991). The use of Prior
        Distributions in Marginalized Bayesian Item Parameter
        Estimation: A Didactic. Psychometrika, 15, 375-389.",
        href = "http://journals.sagepub.com/doi/pdf/10.1177/
        014662169101500409")
      )

    )

}

###############################################################################
###############################################################################
.modelSelect <- function(){

  div(
    h3("Please Select a Model"),
    br(),

    p(
      "Select", code("2pno"), "to apply the",
      em("Two-Parameter Normal Ogive Item Response Model"),
      "(Johnson and Albert (1999) )."
    ),

    p(
      "Select", code("3pno"), "to apply the",
      em("Three-Parameter Normal Ogive Item Response Model"),
      "(Beguin and Glas (2001) )."
    ),

    br(),

    p("After selecting the model, choose the number of iteration,
      burn-in and any other necessary values.", "Then press the",
      code("Start MCMC!"), "button.")

  )}

###############################################################################
###############################################################################
.modelData <- function(){

  div(
    fluidRow(
      column(12,

             h3 ("Introduce your dataset."),
             br(),
             p(
               "The format should look like this:"
             ),
             br(),

             fluidRow(
               column(1, ""),

               column(4,

                      img(src = "TABLE.png", height = 400, width = 750)

               ),

               column(7, "")

             ),
             br(),br(),

             p(
               "Where", em("1 = Correct Answer"), "and",
               em("0 = Wrong Answer"),"."
             ),

             p("For the correct operation, the data format is neccesary.
               After selection the data, press", code("Data is Ok!"),
               "to proceed."),

             p(code("REMARK:"), "If yuo select", code("Use MathTest Data"),
               "option, you can use the MathTest data of the bairt package.
               This is a matrix of observed data for an admission math test
               designed by Simon Bolivar University in 2012.")

      )

      )

  )}

###############################################################################
###############################################################################
.rhat_text <- function(){


  div(

    h3("Convergence Diagnostic"),

    withMathJax(

      helpText(
        "Posterior means of { \\(b_j\\) } plotted against
        the posterior means of { \\(a_j\\) }.
        Each point is labeled with the number of the
        corresponding Item. For the Three-Parameter
        Normal Ogive Item Response Model (", code("3pno"), "),
        the size of the numbers refers to the posterior
        means of { \\(c_j\\) }."
      ),

      helpText(
        "The Potential Scale Reduction
        Factor (", code("Rhat"), ") is calculated for each chain",
        code("SABIRT"), "generates a single MCMC and evaluates
        convergence by breaking the chain in three
        sub chains and comparing the between- and
        within-subchain variance."
      ),

      helpText(
        "The", code("black color suggests convergence"),
        ", while", code("red items indicate convergence problems"),
        "(Rhat greater than 1.1)."
      )

      )
      )

}

###############################################################################
###############################################################################
.irc_text <- function(){

  div(

    h3("Plot of Posterior Density of the Item Response Curve (IRC)"),

    withMathJax(

      helpText(
        "The", code("solid line"), "corresponds to the location of
        the posterior medians and the", code("points"),
        "correspond to the 5th and 95th percentiles of the
        posterior density."
      )

      )
  )

}

###############################################################################
###############################################################################
.ItemsPosterior_text <- function(){

  div(
    helpText(
      code("Remark:"), " use for example 1:4 for represent the vector
      1, 2, 3, 4. And also you can use 1,2,5:7 for represent the vector
      1, 2, 5, 6, 7."
    )
  )

}

###############################################################################
###############################################################################
.parameter_text <- function(){

  div(

    h3("Graph of Posterior Densities"),

    withMathJax(

      helpText(
        "Graph of posterior densities of the parameters { \\(a_j\\) }
        and { \\(b_j\\) } for all Items (for the model 3pno
        posterior densities of { \\(c_j\\) } are included).",
        code("The center of an error bar"), "corresponds to the
        posterior mean, and the", code("extremes"), "correspond
        to the 5th and 95th percentiles of the posterior density."
      )

    )
    )

}


###############################################################################
###############################################################################
.chain_text <- function(){

  div(

    h3("Convergence Graphs for the Simulated Values"),

    withMathJax(

      helpText(
        "The", code("top left graph"), "displays the sequence
        of simulated values (the", code("red line"),
        "is the posterior mean) and the", code("top right graph"),
        "displays the lag correlations of the sequence as a function
        of the lag value. The", code("bottom left graph"), "is a
        histogram of the simulated values and the",
        code("bottom right graph"), "is the box plot
        of the simulated values."
        )

      )
      )

}

###############################################################################
###############################################################################
.download_text <- function(){

  div(

    h3("Summary Table"),

    withMathJax(

      helpText(
        "The Summary Table containing the point estimates
        of the Potential Scale Reduction Factor (", code("Rhat"),
        ") and their Upper Confidence Limits (", code("Upper"),
        "). Also, it is possible to get important percentiles
        of the posterior density."
      ),

      helpText(
        "Select to the correct type and press", code("Dowload Table"),
        "to download the information."
      )
    )
  )

}

