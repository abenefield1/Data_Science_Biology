dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
MAKEVARS <- file.path(dotR, "Makevars")
if (!file.exists(MAKEVARS)) file.create(MAKEVARS)

cat(
  "\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined -Wno-unknown-pragmas",
  "\nCC=clang",
  "CXX=clang++ -arch x86_64 -ftemplate-depth-256",
  file = "MAKEVARS", 
  sep = "\n", 
  append = TRUE
)

# print the contents to the console 
cat(readLines(MAKEVARS), sep = "\n")
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)

fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
                           ' )
fx( 2L, 5 )


library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

schools_dat <- list(J = 8,
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat,
            iter = 1000, chains = 4)
print(fit)
