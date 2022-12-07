## Quick simulations with the switchboard package for R<img src="inst/images/test_orange3.png" align="right" height = 150 width = 150>


# Where to start?
Here's a YouTube tutorial: 
<blockquote class="twitter-tweet"><p lang="en" dir="ltr"><a href="https://twitter.com/hashtag/correlation?src=hash&amp;ref_src=twsrc%5Etfw">#correlation</a> without <a href="https://twitter.com/hashtag/causation?src=hash&amp;ref_src=twsrc%5Etfw">#causation</a> via <a href="https://twitter.com/hashtag/ColliderBias?src=hash&amp;ref_src=twsrc%5Etfw">#ColliderBias</a><br><br>a spurious relationship between the dependent (Y) &amp; independent (X) variable can occur, when each effect a 3rd variable (C, <a href="https://twitter.com/hashtag/collider?src=hash&amp;ref_src=twsrc%5Etfw">#collider</a>), and this 3rd is included in the linear model lm(Y~X+C) 🤔<a href="https://twitter.com/hashtag/Rstats?src=hash&amp;ref_src=twsrc%5Etfw">#Rstats</a> <a href="https://twitter.com/hashtag/CausalModelling?src=hash&amp;ref_src=twsrc%5Etfw">#CausalModelling</a> <a href="https://twitter.com/hashtag/dataviz?src=hash&amp;ref_src=twsrc%5Etfw">#dataviz</a> <a href="https://t.co/1eyxgrrSp0">pic.twitter.com/1eyxgrrSp0</a></p>&mdash; Marc J. Lajeunesse (@LajeunesseLab) <a href="https://twitter.com/LajeunesseLab/status/1598709472125685760?ref_src=twsrc%5Etfw">December 2, 2022</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

# Also here's a Twitter thread on switchboard's functionalities, but also see functions on CRAN <a href="https://CRAN.R-project.org/package=switchboard">here</a>, and GitHub <a href="https://github.com/mjlajeunesse/switchboard">here</a>.<br><br> </h3>

### December 8, 2022 | Monte Carlo estimation of pi using circle vs jigglypuff area


R code with switchboard v. 0.1

<blockquote class="twitter-tweet"><p lang="en" dir="ltr"><a href="https://twitter.com/hashtag/correlation?src=hash&amp;ref_src=twsrc%5Etfw">#correlation</a> without <a href="https://twitter.com/hashtag/causation?src=hash&amp;ref_src=twsrc%5Etfw">#causation</a> via <a href="https://twitter.com/hashtag/ColliderBias?src=hash&amp;ref_src=twsrc%5Etfw">#ColliderBias</a><br><br>a spurious relationship between the dependent (Y) &amp; independent (X) variable can occur, when each effect a 3rd variable (C, <a href="https://twitter.com/hashtag/collider?src=hash&amp;ref_src=twsrc%5Etfw">#collider</a>), and this 3rd is included in the linear model lm(Y~X+C) ðŸ¤”<a href="https://twitter.com/hashtag/Rstats?src=hash&amp;ref_src=twsrc%5Etfw">#Rstats</a> <a href="https://twitter.com/hashtag/CausalModelling?src=hash&amp;ref_src=twsrc%5Etfw">#CausalModelling</a> <a href="https://twitter.com/hashtag/dataviz?src=hash&amp;ref_src=twsrc%5Etfw">#dataviz</a> <a href="https://t.co/1eyxgrrSp0">pic.twitter.com/1eyxgrrSp0</a></p>&mdash; Marc J. Lajeunesse (@LajeunesseLab) <a href="https://twitter.com/LajeunesseLab/status/1598709472125685760?ref_src=twsrc%5Etfw">December 2, 2022</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 
<iframe src="https://ecoevo.social/@LajeunesseLab/109445048974448685/embed" class="mastodon-embed" style="max-width: 100%; border: 0" width="400" allowfullscreen="allowfullscreen"></iframe><script src="https://ecoevo.social/embed.js" async="async"></script>


``` r
library(switchboard)
library(EBImage)
library(sp)

set.seed(19)




  switchboard() %&gt;%
    caption(c(&quot;Monte Carlo estimate of \U03C0&quot;,
              &quot;If you count the number of random points falling within a circle enclosed by a unit square, then 4 * enclosed / total randoms will aproximate \U03C0.&quot;),
            placeOnGrid = c(0, 0), extendRow = 3, size = 1.5) %&gt;%
    eavesdropper_2D(c(X, Y), minimum = c(0, 0), maximum = c(200, 200), size = 2,
                    placeOnGrid = c(6, 0),  forget = forgetTimeCircle) %&gt;%
    eavesdropper_2D(c(X, Y), minimum = c(0, 0), maximum = c(200, 200), size = 2,
                    placeOnGrid = c(6, 2),  forget = forgetTimeJiggly) %&gt;%
    number(4.0 * insideCircle / simulation_length, label = &quot;\U03C0 ~ circle&quot;,
           placeOnGrid = c(9, 0)) %&gt;%
    number(4.0 * insideJiggly / simulation_length, label = &quot;\U03C0 ~ jigglypuff&quot;,
           placeOnGrid = c(9, 2))


}
switchboard_close()</code></pre>
```
