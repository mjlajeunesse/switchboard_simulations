<br>

------------------------------------------------------------------------

## December 8, 2022 \| Monte Carlo estimation of pi using circle vs jigglypuff area

> <br> <br> \#\#\# R code with switchboard v. 0.1
>
> ``` r
> library(switchboard)
> library(EBImage)
> library(sp)
>
> set.seed(19)
>
> getPolygonOfImage <- function(anImageFile, rotate = 0) {
>   anImage <- EBImage::resize(EBImage::readImage(anImageFile), w = 201, h = 201)
>   return(EBImage::ocontour(EBImage::rotate(anImage, rotate)))
> }
>
> poly_circle <- getPolygonOfImage("circle.png")
> poly_jigglypuff <- getPolygonOfImage("jigglypuff.png", 180)
> poly_jigglypuffEye1 <- getPolygonOfImage("jigglypuff_eye1.png", 180)
> poly_jigglypuffEye2 <- getPolygonOfImage("jigglypuff_eye1.png", 180)
>
> insideCircle <- 0; insideJiggly <- 0;
>
> for(simulation_length in 1:20000) {
>
>   forgetTimeCircle <- 600; forgetTimeJiggly <- 600;
>   X <- runif(1, min = 0, max = 200); Y <- runif(1, min = 0, max = 200);
>
>   if(sp::point.in.polygon(X, Y, poly_jigglypuff[[1]][, 1], poly_jigglypuff[[1]][, 2])) {
>    if((!sp::point.in.polygon(X, Y, poly_jigglypuffEye1[[1]][, 1], poly_jigglypuffEye1[[1]][, 2])) &&
>       (!sp::point.in.polygon(X, Y, poly_jigglypuffEye2[[1]][, 1], poly_jigglypuffEye2[[1]][, 2]))) {
>      insideJiggly <- insideJiggly + 1; forgetTimeJiggly <- 50000;
>    }
>   }
>
>   if(sp::point.in.polygon(X , Y, polyCircle[[1]][, 1], polyCircle[[1]][, 2])) {
>     insideCircle <- insideCircle + 1; forgetTimeCircle <- 50000;
>   }
>
>   switchboard() %>%
>     caption(c("Monte Carlo estimate of \U03C0",
>               "If you count the number of random points falling within a circle enclosed by a unit square, then 4 * enclosed / total randoms will aproximate \U03C0."),
>             placeOnGrid = c(0, 0), extendRow = 3, size = 1.5) %>%
>     eavesdropper_2D(c(X, Y), minimum = c(0, 0), maximum = c(200, 200), size = 2,
>                     placeOnGrid = c(6, 0),  forget = forgetTimeCircle) %>%
>     eavesdropper_2D(c(X, Y), minimum = c(0, 0), maximum = c(200, 200), size = 2,
>                     placeOnGrid = c(6, 2),  forget = forgetTimeJiggly) %>%
>     number(4.0 * insideCircle / simulation_length, label = "\U03C0 ~ circle",
>            placeOnGrid = c(9, 0)) %>%
>     number(4.0 * insideJiggly / simulation_length, label = "\U03C0 ~ jigglypuff",
>            placeOnGrid = c(9, 2))
>
>
> }
> switchboard_close()
> ```

<br>

------------------------------------------------------------------------

## December 2, 2022 \| correlation without causation via collider bias

> <br> <br> \#\#\# R code with switchboard v. 0.1
>
> ``` r
> library(switchboard)
> collider <- 0
> min_XYC <- c(-10, -10); max_XYC <- c(10, 10);
> N <- 250
>
> for(simulation_length in 1:50000) {
>
>   X <- rnorm(N, 0, 2); Y <- rnorm(N, 0, 2);
>   C <- collider * X + collider * Y + rnorm(N, 0, 2)
>   XYC <- data.frame(X, Y, C)
>   betaXY_lm_XY <- lm(Y ~ X, data = XYC)$coefficients[2]
>   betaXY_lm_XYC <- lm(Y ~ X + C, data = XYC)$coefficients[2]
>
>   switchboard() %>%
>     caption(c("collider bias",
>               "When Y & X influence variation in a collider (C), the estimated slope between X & Y can be lower than the true slope when C is included in the linear model (lm)."), placeOnGrid = c(0,0), size = 2) %>%
>     control_slider("collider", label = "collider", minimum = 0, maximum = 2,
>                    placeOnGrid = c(2,2)) %>%
>     eavesdropper_2D(c(XYC[1, 1], XYC[1, 2]), label = c("Y~X", ""), plotRegression = TRUE,
>                     placeOnGrid = c(2, 1), minimum = min_XYC, maximum = max_XYC) %>%
>     eavesdropper_2D(c(XYC[1, 1], XYC[1, 3]), label = c("X~C", ""), plotRegression = TRUE,
>                     placeOnGrid = c(1, 2), minimum = min_XYC, maximum = max_XYC) %>%
>     eavesdropper_2D(c(XYC[1, 2], XYC[1, 3]), label = c("Y~C", ""), plotRegression = TRUE,
>                     placeOnGrid = c(3, 2), minimum = min_XYC, maximum = max_XYC) %>%
>     eavesdropper_2D(c(betaXY_lm_XY, betaXY_lm_XYC), label = c("slope bias", ""), switch = TRUE,
>                     placeOnGrid = c(3,3), minimum = c(-0.1, -1.0), maximum = c(0.1, 0.5)) %>%
>     eavesdropper(eavesdrop = betaXY_lm_XY, label = c("lm(Y~X)"),
>                  placeOnGrid = c(2,0), minimum = -1, maximum = 0.5) %>%
>     eavesdropper(eavesdrop = betaXY_lm_XYC, label = c("lm(Y~X+C)"),
>                  placeOnGrid = c(2,3),  minimum = -1, maximum = 0.5) %>%
>     progress_image(round(collider / 2 * 100), maximum = 100,
>                   file = "C:\\Users\\lajeunesse\\Desktop\\collider_4.png",
>                   placeOnGrid = c(1,3), fill = "vertical", honest = TRUE, size = 1)
>
> }
> switchboard_close()
> ```

<br>

------------------------------------------------------------------------

## November 18, 2022 \| effects of noise versus sampling error on regression

> <blockquote class="twitter-tweet">
> <p lang="en" dir="ltr">
> in regression, noise is assumed independent—but none of that matters
> when sample sizes are low…<br><br>here the correlation remains
> approximately the same at -0.065 irrespective of noise, but will range
> from -1 to 1 with small
> samples<a href="https://twitter.com/hashtag/rstats?src=hash&amp;ref_src=twsrc%5Etfw">\#rstats</a>
> <a href="https://twitter.com/hashtag/dataviz?src=hash&amp;ref_src=twsrc%5Etfw">\#dataviz</a>
> <a href="https://twitter.com/hashtag/regression?src=hash&amp;ref_src=twsrc%5Etfw">\#regression</a>
> <a href="https://twitter.com/hashtag/statistics?src=hash&amp;ref_src=twsrc%5Etfw">\#statistics</a>
> <a href="https://t.co/XTb7pfu9Oq">pic.twitter.com/XTb7pfu9Oq</a>
> </p>
> — Marc J. Lajeunesse (@LajeunesseLab)
> <a href="https://twitter.com/LajeunesseLab/status/1593684244160688129?ref_src=twsrc%5Etfw">November
> 18, 2022</a>
> </blockquote>
> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

> <br> <br> \#\#\# R code with switchboard v. 0.1
>
> ``` r
> library(switchboard)
> library(MASS)
>
> pop_rho <- 0; pop_X <- 0; pop_Y <- 0; pop_SD <- 0.5;
> pop_N <- 142
> dino_X <- c(55.3846,  51.5385,  46.1538,  42.8205,  40.7692,  38.7179,  35.641,  
>             33.0769,  28.9744,  26.1538,  23.0769,  22.3077,  22.3077,  23.3333,  
>             25.8974,  29.4872,  32.8205,  35.3846,  40.2564,  44.1026,  46.6667, 
>             50,  53.0769,  56.6667,  59.2308,  61.2821,  61.5385,  61.7949,  
>             57.4359,  54.8718,  52.5641,  48.2051,  49.4872,  51.0256,  45.3846,  
>             42.8205,  38.7179,  35.1282,  32.5641,  30,  33.5897,  36.6667,  
>             38.2051,  29.7436,  29.7436,  30,  32.0513,  35.8974,  41.0256,  
>             44.1026,  47.1795,  49.4872,  51.5385,  53.5897,  55.1282,  56.6667,  
>             59.2308,  62.3077,  64.8718,  67.9487,  70.5128,  71.5385,  71.5385,  
>             69.4872,  46.9231,  48.2051,  50,  53.0769,  55.3846,  56.6667,  
>             56.1538,  53.8462,  51.2821,  50,  47.9487,  29.7436,  29.7436,  
>             31.2821,  57.9487,  61.7949,  64.8718,  68.4615,  70.7692,  72.0513,  
>             73.8462,  75.1282,  76.6667,  77.6923,  79.7436,  81.7949,  83.3333,  
>             85.1282,  86.4103,  87.9487,  89.4872,  93.3333,  95.3846,  98.2051,  
>             56.6667,  59.2308,  60.7692,  63.0769,  64.1026,  64.359,  74.359,  
>             71.2821,  67.9487,  65.8974,  63.0769,  61.2821,  58.7179,  55.1282,  
>             52.3077,  49.7436,  47.4359,  44.8718,  48.7179,  51.2821,  54.1026,  
>             56.1538,  52.0513,  48.7179,  47.1795,  46.1538,  50.5128,  53.8462,  
>             57.4359,  60,  64.1026,  66.9231,  71.2821,  74.359,  78.2051,  
>             67.9487,  68.4615,  68.2051,  37.6923,  39.4872,  91.2821,  50,  47.9487,  44.1026 )
> dino_Y <- c(97.1795,  96.0256,  94.4872,  91.4103,  88.3333,  84.8718,  79.8718,
>             77.5641,  74.4872,  71.4103,  66.4103,  61.7949,  57.1795,  52.9487,
>             51.0256,  51.0256,  51.0256,  51.4103,  51.4103,  52.9487,  54.1026, 
>             55.2564,  55.641,  56.0256,  57.9487,  62.1795,  66.4103,  69.1026, 
>             55.2564,  49.8718,  46.0256,  38.3333,  42.1795,  44.1026,  36.4103, 
>             32.5641,  31.4103,  30.2564,  32.1795,  36.7949,  41.4103,  45.641, 
>             49.1026,  36.0256,  32.1795,  29.1026,  26.7949,  25.2564,  25.2564, 
>             25.641,  28.718,  31.4103,  34.8718,  37.5641,  40.641,  42.1795,  
>             44.4872,  46.0256,  46.7949,  47.9487,  53.718,  60.641,  64.4872,
>             69.4872,  79.8718,  84.1026,  85.2564,  85.2564,  86.0256,  86.0256, 
>             82.9487,  80.641,  78.718,  78.718,  77.5641,  59.8718,  62.1795, 
>             62.5641,  99.4872,  99.1026,  97.5641,  94.1026,  91.0256,  86.4103,
>             83.3333,  79.1026,  75.2564,  71.4103,  66.7949,  60.2564,  55.2564, 
>             51.4103,  47.5641,  46.0256,  42.5641,  39.8718,  36.7949,  33.718,  
>             40.641,  38.3333,  33.718,  29.1026,  25.2564,  24.1026,  22.9487, 
>             22.9487,  22.1795,  20.2564,  19.1026,  19.1026,  18.3333,  18.3333,
>             18.3333,  17.5641,  16.0256,  13.718,  14.8718,  14.8718,  14.8718, 
>             14.1026,  12.5641,  11.0256,  9.8718,  6.0256,  9.4872,  10.2564, 
>             10.2564,  10.641,  10.641,  10.641,  10.641,  10.641,  10.641,  8.718, 
>             5.2564,  2.9487,  25.7692,  25.3846,  41.5385,  95.7692,  95,  92.6923)
>
> for(i in 1:1e6) {
>
>   cov_XY <- matrix(c(pop_SD, 0, 0, pop_SD), nrow = 2, ncol = 2)
>   sample_XY <- MASS::mvrnorm(142, mu = c(pop_X, pop_Y), Sigma = cov_XY)
>
>   sample_XY[, 1] <- sample_XY[, 1] + dino_X * 1.2
>   sample_XY[, 2] <- sample_XY[, 2] + dino_Y
>
>   sample_XY <- sample_XY[sample(142, pop_N), ]
>   random_sample <- sample.int(nrow(sample_XY), 1)
>
>   switchboard() %>%
>     caption(c("Monte Carlo Simulation",
>               "Noise is assumed independent in regression, but sampling error will erodes this."),
>             placeOnGrid = c(1,1), size = 2) %>%
>     control_slider_pair(c("pop_SD", "pop_N"), minimum = c(0.5, 10), maximum = c(20, 142),
>                         label = c("noise", "sample size"), placeOnGrid = c(2, 3)) %>%
>     eavesdropper_2D(c(sample_XY[random_sample, 1], sample_XY[random_sample, 2]), inject = c("pop_X", "pop_Y"),
>                 minimum = c(0, 0), maximum = c(120, 100),
>                 plotRegression = TRUE, plotSampleSize = ctrl_N, switch = TRUE,
>                 forget = nrow(sample_XY) * 6, placeOnGrid = c(1, 4), size = 2) %>%
>     eavesdropper(cor(sample_XY[,1], sample_XY[,2]), label = "correlation", minimum = -1, maximum = 1, placeOnGrid = c(1,7), size = 2)
>
> }
> switchboard_close()
> ```

<br>

------------------------------------------------------------------------

## November 9, 2022 \| p-value dance when effect is null

> <blockquote class="twitter-tweet">
> <p lang="en" dir="ltr">
> There is tremendous risk in repeatedly analyzing data as it comes
> in…<br><br>For example, in the absence of any effect, the p-values of
> repeated t-tests on a growing dataset will randomly walk between 1 and
> 0!<br><br>Thanks to
> <a href="https://twitter.com/lakens?ref_src=twsrc%5Etfw">@lakens</a>
> for inspiring this
> simulation!<a href="https://twitter.com/hashtag/RStats?src=hash&amp;ref_src=twsrc%5Etfw">\#RStats</a>
> <a href="https://twitter.com/hashtag/dataviz?src=hash&amp;ref_src=twsrc%5Etfw">\#dataviz</a>
> <a href="https://t.co/eoyLLDkjzb">pic.twitter.com/eoyLLDkjzb</a>
> </p>
> — Marc J. Lajeunesse (@LajeunesseLab)
> <a href="https://twitter.com/LajeunesseLab/status/1590405834059968513?ref_src=twsrc%5Etfw">November
> 9, 2022</a>
> </blockquote>
> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
>
> <br>
> (<a href="https://ecoevo.social/@LajeunesseLab/109315310837150463">Click
> here to also see toot on Mastodon</a>). <br> <br> \#\#\# R code with
> switchboard v. 0.1
>
> ``` r
> library(switchboard)
> library(MASS)
>
> pop_rho <- 0; pop_X <- 0; pop_Y <- 0;
> sample_XY <- 0; cov_XY <- matrix(c(1, pop_rho, pop_rho, 1), nrow = 2, ncol = 2);
>
> for(simulation_length in 1:5000) {
>
>   sample_XY <- rbind(MASS::mvrnorm(1, mu = c(pop_X, pop_Y), Sigma = cov_XY), sample_XY)
>   ttest <- stats::t.test(sample_XY[, 1], sample_XY[, 2])
>
>   switchboard() %>%
>     caption(c("drifting p-values",
>               "Impact of adding more data in absence of an effect."),
>             placeOnGrid = c(1, 1), extendRow = 2) %>%
>     eavesdropper(ttest$p.value, label = "p-value", minimum = 0, maximum = 1,
>                  size = 2, placeOnGrid = c(1, 3)) %>%
>     progress_pikachu(simulation_length, maximum = 5000, closeAtMaximum = FALSE,
>                      fill = "horizontal", placeOnGrid = c(2, 1)) %>%
>     number(simulation_length, label = "sample size",  placeOnGrid = c(2, 2))
>
> }
> switchboard_close()
> ```

<br>

------------------------------------------------------------------------

## November 4, 2022 \| dance of regression with small sample sizes

> <blockquote class="twitter-tweet">
> <p lang="en" dir="ltr">
> Here’s what happens to regression when sample sizes are low—even with
> a strong underlying correlation, sampling error will erode
> everything<br><br>New update to the {switchboard}
> <a href="https://twitter.com/hashtag/RStats?src=hash&amp;ref_src=twsrc%5Etfw">\#RStats</a>
> package for simulation visualizations coming soon! <br><br>See YouTube
> for more:
> <a href="https://t.co/DAGqDD5p5g">https://t.co/DAGqDD5p5g</a>
> <a href="https://t.co/Hcg59YQCSM">pic.twitter.com/Hcg59YQCSM</a>
> </p>
> — Marc J. Lajeunesse (@LajeunesseLab)
> <a href="https://twitter.com/LajeunesseLab/status/1588545221830201344?ref_src=twsrc%5Etfw">November
> 4, 2022</a>
> </blockquote>
> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
>
> <br>
> (<a href="https://mastodon.sdf.org/@LajeunesseLab/109286174501314783">Click
> here to also see toot on Mastodon</a>). <br> <br> \#\#\# R code with
> switchboard v. 0.1
>
> ``` r
> library(switchboard)
> library(MASS)
>
> pop_rho <- 0; pop_X <- 0; pop_Y <- 0;
> pop_forget <- 400 #milliseconds
> ctrl_regression <- FALSE; ctrl_N <- FALSE;
>
> # NOTE: if swichboard window is closed early, the simulation will continue to run in background until i == 1e5
> for(i in 1:1e5) {
>
>   cov_XY <- matrix(c(1, pop_rho, pop_rho, 1), nrow = 2, ncol = 2)
>   sample_XY <- MASS::mvrnorm(1, mu = c(pop_X, pop_Y), Sigma = cov_XY)
>   
>   switchboard() %>%
>     caption(c("Monte Carlo Simulation", 
>               "Slide correlation and sample size to see how regression estimation is impacted."), 
>             placeOnGrid = c(1,1), size = 2) %>%
>     control_switch_pair(c("ctrl_regression", "ctrl_N"), 
>                         label = c("plot slope", "plot N"), placeOnGrid = c(1, 3)) %>%
>     control_slider_pair(c("pop_rho", "pop_forget"), minimum = c(-1, 4), maximum = c(1, 3000),
>                         label = c("correlation", "sample size"), placeOnGrid = c(2, 3)) %>%
>     injector_2D(c(sample_XY[1], sample_XY[2]), inject = c("pop_X", "pop_Y"),
>                 minimum = c(-5, -5), maximum = c(5, 5),
>                 plotRegression = ctrl_regression, plotSampleSize = ctrl_N, switch = TRUE,
>                 forget = pop_forget, placeOnGrid = c(1, 4), size = 2) 
>   
> }
> switchboard_close()
> ```
