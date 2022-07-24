[Art from code](https://art-from-code.netlify.app/)
================

### rstudio::conf 2022

by Danielle Navarro

-----

:spiral_calendar: July 25 and 26, 2022  
:alarm_clock:     09:00 - 17:00  
:hotel:           Maryland 1
:writing_hand:    [rstd.io/conf](http://rstd.io/conf)

-----

## Overview

Computer-generated artwork has been around for quite a while. The idea of using R for this purpose, however, is a little more recent. Designed originally as a programming language for academic statistical computing, R is now a mainstream language for data science and analytics. Can it also work as an artistic medium? Is there an overlap between our familiar data science workflows and the artistic process? Perhaps we can become better data scientists through art, and vice versa.

This workshop provides a hands-on introduction to generative art in R. You’ll learn artistic techniques that generative artists use regularly in their work including flow fields, iterative function systems, and more. You’ll also learn about R packages that are specialized for generative art including jasmines, ambient, and ggfx. But more than that, you’ll learn how to reuse skills you already have as part of an artistic process: with a little work, ggplot2, dplyr, and Rcpp can become an artist's best friends. Finally, I’ll talk about how generative art helped me learn new programming skills and turned out to be valuable to me in the workplace.

The assumed background is that you’re reasonably comfortable using R and RStudio, and have experience with tidyverse. There are places where C++, git/github, and blogdown are used, but these aren’t major parts of the workshop.

## Course material

Everything you need for the workshop is (or will be!) posted online at [art-from-code.netlify.app](https://art-from-code.netlify.app). 

## Getting set up 

### Option 1: Local

``` r
# create, download, and open project...
usethis::create_from_github(
  repo_spec = "rstudio-conf-2022/art-from-code", 
  destdir = "wherever/you/would/like"
)

# within the project...
remotes::install_deps()
```

### Option 2: RStudio cloud 

[rstudio.cloud/spaces/250954](https://rstudio.cloud/spaces/250954)


## Schedule

### Day 1

| Time          | Activity                                                                              |
| :------------ | :------------------------------------------------------------------------------------ |
| 09:00 - 10:30 | Session 1: [Get started](https://art-from-code.netlify.com/day-1/session-1)           |
| 10:30 - 11:00 | *Coffee break*                                                                        |
| 11:00 - 12:30 | Session 2: [Spatial noise tricks](https://art-from-code.netlify.com/day-1/session-2)  |
| 12:30 - 13:30 | *Lunch break*                                                                         |
| 13:30 - 15:00 | Session 3: [Polygon tricks](https://art-from-code.netlify.com/day-1/session-3)        |
| 15:00 - 15:30 | *Coffee break*                                                                        |
| 15:30 - 17:00 | Session 4: [Shading tricks](https://art-from-code.netlify.com/day-1/session-4)        |

### Day 2

| Time          | Activity                                                                                  |
| :------------ | :---------------------------------------------------------------------------------------- |
| 09:00 - 10:30 | Session 1: [Iterated function systems](https://art-from-code.netlify.com/day-2/session-1) |
| 10:30 - 11:00 | *Coffee break*                                                                            |
| 11:00 - 12:30 | Session 2: [Tiles and tesselations](https://art-from-code.netlify.com/day-2/session-2)    |
| 12:30 - 13:30 | *Lunch break*                                                                             |
| 13:30 - 15:00 | Session 3: [Pixel shaders](https://art-from-code.netlify.com/day-2/session-3)             |
| 15:00 - 15:30 | *Coffee break*                                                                            |
| 15:30 - 17:00 | Session 4: [Wrap up](https://art-from-code.netlify.com/day-2/session-4)                   |

## Instructor

Danielle Navarro is a generative artist, data scientist, professional educator, mathematical psychologist, academic professor in recovery, open source R developer, and coauthor of “ggplot2: Elegant Graphics for Data Analysis” (3rd edition). She is currently a developer advocate at Voltron Data working with the Apache Arrow ecosystem. You can find her art at https://art.djnavarro.net, and other professional details at https://djnavarro.net. Danielle lives in Sydney, Australia with her two children and her Netflix subscription.

-----

![](https://i.creativecommons.org/l/by/4.0/88x31.png) This work is
licensed under a [Creative Commons Attribution 4.0 International
License](https://creativecommons.org/licenses/by/4.0/).
