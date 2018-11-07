Variable Scales
================
Benefield
10/31/2018

## Scales of Variables

#### (1.) Scales of Variation in the sampling design.

**Scales**: Between state variation and between county variation
(residual)

**Grouping variables** The grouping variables / random effects are
states which are comprised of counties state(county)

#### (2.) Scale of Response Variable.

**Hectares of protected area (PA\_ha)**

#### (3.) Scales of measurement for Predictor Variables.

**GAP Status (1-4)** = between county predictor and **State GDP** =
between state predictor (?)

#### Sketch of Sample Design:

![](Conservation_glm_files/figure-markdown_github/Sample_Design.png)

## The Model:

From the feedback, I wasn’t sure if I was supposed to rewrite the
mathematical model as well as the code, so I did both.

#### (1.) The Statistical Model in mathematical notation:

**County Level Model**   
![y\_{i}\\sim N(X\_{i}\\beta+\\eta\_{j\[i\]}, \\sigma^{2}\_{y}),\\ for\\
i\\ =\\ 1,\\ ...,\\
n](https://latex.codecogs.com/png.latex?y_%7Bi%7D%5Csim%20N%28X_%7Bi%7D%5Cbeta%2B%5Ceta_%7Bj%5Bi%5D%7D%2C%20%5Csigma%5E%7B2%7D_%7By%7D%29%2C%5C%20for%5C%20i%5C%20%3D%5C%201%2C%5C%20...%2C%5C%20n
"y_{i}\\sim N(X_{i}\\beta+\\eta_{j[i]}, \\sigma^{2}_{y}),\\ for\\ i\\ =\\ 1,\\ ...,\\ n")  
**State Level Model:**   
![\\eta\_{j}\\sim N(0,
\\sigma\_{\\alpha}^{2})](https://latex.codecogs.com/png.latex?%5Ceta_%7Bj%7D%5Csim%20N%280%2C%20%5Csigma_%7B%5Calpha%7D%5E%7B2%7D%29
"\\eta_{j}\\sim N(0, \\sigma_{\\alpha}^{2})")  

  
![j\[i\]=State\\ j\\ that\\ contains \\ county\\
i](https://latex.codecogs.com/png.latex?j%5Bi%5D%3DState%5C%20j%5C%20that%5C%20contains%20%5C%20county%5C%20i
"j[i]=State\\ j\\ that\\ contains \\ county\\ i")  

  
![ X\\ is\\ a\\ 3\\ column\\ matrix\\ with:\\\\
A\\ constant\\ term,\\ X\_{(0)},\\\\
the\\ GAP\\ Status,\\ X\_{(1)},\\ and\\\\
the\\ state\\ GDP,
X\_{(2)}](https://latex.codecogs.com/png.latex?%20X%5C%20is%5C%20a%5C%203%5C%20column%5C%20matrix%5C%20with%3A%5C%5C%0AA%5C%20constant%5C%20term%2C%5C%20X_%7B%280%29%7D%2C%5C%5C%0Athe%5C%20GAP%5C%20Status%2C%5C%20X_%7B%281%29%7D%2C%5C%20and%5C%5C%0Athe%5C%20state%5C%20GDP%2C%20X_%7B%282%29%7D
" X\\ is\\ a\\ 3\\ column\\ matrix\\ with:\\\\
A\\ constant\\ term,\\ X_{(0)},\\\\
the\\ GAP\\ Status,\\ X_{(1)},\\ and\\\\
the\\ state\\ GDP, X_{(2)}")  

Is this ok for more than two
![X\_{(1)}](https://latex.codecogs.com/png.latex?X_%7B%281%29%7D
"X_{(1)}") measures? I have GAP Status 1:4, but there are only 2 floors
in the radon example. Also, I do have county-level (house-level in
radon) identifiers, but I’m not sure why they would be important. Do I
need to account for them in the
model?

#### (2.) Write down the corresponding linear model formula for stan\_lmer/stan\_glmer.

``` r
ppfit_bayes <- stan_lmer( log_PA_ha ~ Gap_Status_x + State_GDP + (1|State_Name), data=dat1 )
```

![](Conservation_glm_files/figure-gfm/unnamed-chunk-36-2.png)
