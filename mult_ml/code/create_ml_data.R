## Most of the code is borrowed from Patrick O Perry's JRSS-B paper
rm(list=ls())
setwd("/mnt/nfs/rdss/ssrivastva/ml_raw")

# read movies
movie <- read.csv("movie.csv", stringsAsFactors = FALSE)

# id
movie$id <- as.character(movie$id)

# genre dummies
genre <- strsplit(movie$genre, "\\|")
genre.levels <- c("(no genres listed)", "Action", "Animation", "Adventure",
                  "Animation", "Children", "Comedy", "Crime", "Documentary",
                  "Drama", "Fantasy", "Film-Noir", "IMAX", "Horror", "Musical",
                  "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
genre.codes <- tolower(genre.levels)
genre.codes[match(c("(no genres listed)", "Film-Noir", "Sci-Fi"),
                  genre.levels)] <- c("unknown", "filmnoir", "scifi")

for (i in seq_along(genre.levels)) {
    g <- genre.levels[i]
    movie[,genre.codes[i]] <- sapply(genre, function(x) g %in% x)
}

## (if IMAX is the only listed genre, code as "unknown")
movie[sapply(genre, length) == 1 & movie[,"imax"], "unknown"] <- TRUE

# read ratings (and users)
rating <- read.csv("rating.csv")

# users
user <- data.frame(id = as.character(sort(unique(rating$user))))

# ratings
rating$user <- match(rating$user, user$id)
rating$movie <- match(rating$movie, movie$id)
rating$score <- factor(rating$score, ordered=TRUE)
rating$time <- as.POSIXct(rating$timestamp, origin="1970-01-01", tz="UTC")
rating <- rating[,c("user", "movie", "score", "time")]

# tags
##tags <- read.csv("tag.csv")

rating.feature <- read.csv("rating-feature.csv")
user.liked.last <- rating.feature$user_pos_len >= 1

logit <- function(p) log(p / (1 - p))

movie.logit.pop <- with(rating.feature,
                        logit((movie_pos + 0.5) / (movie_pos + movie_neg + 1)))


# response and grouping factor
y <- (as.numeric(as.character(rating$score)))
group <- factor(rating$user, ordered=FALSE)

## # covariate: genre
action <- rowSums(movie[,c("action", "adventure", "fantasy", "horror", "scifi",
                            "thriller")])
children <- rowSums(movie[,c("animation", "children")])
comedy <- rowSums(movie[,c("comedy"),drop=FALSE])
drama <- rowSums(movie[,c("crime", "documentary", "drama", "filmnoir",
                           "musical", "mystery", "romance", "war",
                           "western")])
genre <- cbind(action, children, comedy, drama)
genre.names <- colnames(genre)
genre <- genre / pmax(1, rowSums(genre))
contr.genre <- rbind(-1, diag(ncol(genre) - 1L))
genre <- genre %*% contr.genre
colnames(genre) <- genre.names[-1]
genre <- genre[rating$movie,,drop=FALSE]

# n.movie
n.movie.raw <- by(rep(1, length(y)), rating$movie, sum)
n.movie <- numeric(nrow(movie))
names(n.movie) <- seq_len(nrow(movie))
n.movie[names(n.movie.raw)] <- n.movie.raw

# y.movie
y.movie.raw <- by(y, rating$movie, sum)
y.movie <- numeric(nrow(movie))
names(y.movie) <- seq_len(nrow(movie))
y.movie[names(y.movie.raw)] <- y.movie.raw

# fixed effect covariate matrix
fixef.mf <- model.frame(~ genre + movie.logit.pop + user.liked.last)
fixef.mt <- attr(fixef.mf, "terms")
x <- model.matrix(fixef.mt, fixef.mf)

ynew <- y
ynew[y == 0.5] <- 1
ynew[y == 1.5] <- 2
ynew[y == 2.5] <- 3
ynew[y == 3.5] <- 4
ynew[y == 4.5] <- 5

xnew <- as.matrix(x)

saveRDS(list(y = ynew, x = xnew), "/Shared/ssrivastva/mult_ml/data/ml_full.rds")
