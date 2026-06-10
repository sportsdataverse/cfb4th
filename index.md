# cfb4th

This is the package that powers the [A.I. Sports College Football 4th
Down Model](https://kazink.shinyapps.io/cfb_fourth_down/) discussed in
[this UteZone
article.](https://247sports.com/college/utah/LongFormArticle/Utah-Fourth-Downs-2020-159851937/)
The model is heavily based on the work and
[code](https://www.nfl4th.com/) produced by [Ben
Baldwin](https://twitter.com/benbbaldwin) for his [4th Down Calculator
for the NFL](https://rbsdm.com/stats/fourth_calculator/)

## **Installation**

You can install the development version of cfb4th from
[GitHub](https://github.com/sportsdataverse/cfb4th) with:

``` r

# install.packages("devtools")
devtools::install_github("sportsdataverse/cfb4th")
```

## **Features**

- The **go for it** model gives probabilities for possibilities of yards
  gained and includes the possibility of earning a first down via
  defensive penalty
- The **punt** model includes the possibility for getting blocked,
  returned for a touchdown, or fumbled on the return
- The **field goal** model is a simple model of field goal % by distance
  and roof type

## **Current limitations**

There are some edge cases that are not accounted for. These should only
make a marginal difference to the recommendations as they are largely
edge cases (e.g. the possibility for a field goal to be blocked and
returned).

- The **go for it** model does not allow for the possibility of a
  turnover return. However, long returns are extremely rare: For
  example, in 2018 and 2019 in the NFL there were only four defensive
  touchdowns on plays where teams went for fourth downs out of 1,236
  plays, and all of these happened when the game was well in hand for
  the other team. Additionally, it assumes that a touchdown is worth 7
  points and doesn’t account for go-for-2 situations (Future Work)
- The **punt** model doesn’t account for the punter or returner, ignores
  penalties on returns and ignores the potential for blocked punts to be
  returned for touchdowns
- The **field goal** model doesn’t account for who the kicker is, what
  the weather is (only relevant for outdoor games), or the possibility
  of a kick being blocked and returned for a touchdown

## **Get Started**

To get started with cfb4th please see [this
article](https://cfb4th.sportsdataverse.org/articles/cfb4th.html).

- [The logic of the shiny
  app](https://github.com/Kazink36/cfb_fourth_down/blob/main/app.R)

- [The code that powers the
  bot](https://github.com/Kazink36/cfb_fourth_down/tree/main/bot)

- [The bot can be found on twitter
  here.](https://twitter.com/aisports_4th)

## **Our Authors**

- [Jared Lee](https://twitter.com/JaredDLee)
  [![@JaredDLee](https://img.shields.io/twitter/follow/JaredDLee?color=blue&label=%40JaredDLee&logo=twitter&style=for-the-badge)](https://twitter.com/JaredDLee)
  [![@Kazink36](https://img.shields.io/github/followers/Kazink36?color=eee&logo=Github&style=for-the-badge)](https://github.com/Kazink36)

- [Sebastian Carl](https://twitter.com/mrcaseb)  
  [![@mrcaseb](https://img.shields.io/twitter/follow/mrcaseb?color=blue&label=%40mrcaseb&logo=twitter&style=for-the-badge)](https://twitter.com/mrcaseb)
  [![@mrcaseb](https://img.shields.io/github/followers/mrcaseb?color=eee&logo=Github&style=for-the-badge)](https://github.com/mrcaseb)

- [Ben Baldwin](https://twitter.com/benbbaldwin)  
  [![@benbbaldwin](https://img.shields.io/twitter/follow/benbbaldwin?color=blue&label=%40benbbaldwin&logo=twitter&style=for-the-badge)](https://twitter.com/benbbaldwin)
  [![@guga31bb](https://img.shields.io/github/followers/guga31bb?color=eee&logo=Github&style=for-the-badge)](https://github.com/guga31bb)

## **Citations**

To cite the [**`cfb4th`**](https://cfb4th.sportsdataverse.org/) R
package in publications, use:

BibTex Citation

``` bibtex
@misc{lee_et_al_2021_cfb4th,
  author = {Jared Lee and Sebastian Carl and Ben Baldwin},
  title = {cfb4th: The SportsDataverse's R Package for College Football 4th Down Modeling.},
  url = {https://cfb4th.sportsdataverse.org/},
  year = {2021}
}
```
