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

#### (1.) The Statistical Model in mathematical notation:

**State Level Model:**   
![\\mu \_{j}\\sim N(\\mu, \\sigma
^{2})](https://latex.codecogs.com/png.latex?%5Cmu%20_%7Bj%7D%5Csim%20N%28%5Cmu%2C%20%5Csigma%20%5E%7B2%7D%29
"\\mu _{j}\\sim N(\\mu, \\sigma ^{2})")  
**County Level Model**   
![y\_{i}\\sim N(\\mu \_{j\[i\]}, \\sigma
^{2})](https://latex.codecogs.com/png.latex?y_%7Bi%7D%5Csim%20N%28%5Cmu%20_%7Bj%5Bi%5D%7D%2C%20%5Csigma%20%5E%7B2%7D%29
"y_{i}\\sim N(\\mu _{j[i]}, \\sigma ^{2})")  

  
![\\mu \_{j}=State\\ Mean\\ Hectares\\ of\\ Protected\\
Area](https://latex.codecogs.com/png.latex?%5Cmu%20_%7Bj%7D%3DState%5C%20Mean%5C%20Hectares%5C%20of%5C%20Protected%5C%20Area
"\\mu _{j}=State\\ Mean\\ Hectares\\ of\\ Protected\\ Area")  

  
![j\[i\]=State\\ j\\ that\\ belongs\\ to\\ county\\
i](https://latex.codecogs.com/png.latex?j%5Bi%5D%3DState%5C%20j%5C%20that%5C%20belongs%5C%20to%5C%20county%5C%20i
"j[i]=State\\ j\\ that\\ belongs\\ to\\ county\\ i")
