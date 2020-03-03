\name{similarity_measures}
\alias{similarity_measures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
similarity_measures
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
similarity_measures(dataframe, prompt, response, measure_type = "jaccard", ngrams)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{prompt}{
%%     ~~Describe \code{prompt} here~~
}
  \item{response}{
%%     ~~Describe \code{response} here~~
}
  \item{measure_type}{
%%     ~~Describe \code{measure_type} here~~
}
  \item{ngrams}{
%%     ~~Describe \code{ngrams} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dataframe, prompt, response, measure_type = "jaccard", 
    ngrams) 
{
    1 - stringdist(enc2utf8(dataframe[, prompt]), enc2utf8(dataframe[, 
        response]), useBytes = TRUE, method = measure_type, q = ngrams)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
