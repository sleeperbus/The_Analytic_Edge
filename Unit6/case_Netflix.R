movies = read.table("movieLens.txt", stringsAsFactors=FALSE, sep="|", 
                    head=FALSE,quote="\"")
str(movies)

colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", 
                     "IMDB", "Unknown", "Action", "Adventure", 
                     "Animation", "Childrens", "Comedy", "Crime", 
                     "Documentary", "Drama", "Fantasy", "FilmNoir", 
                     "Horror", "Musical", "Mystery", "Romance", 
                     "SciFi", "Thriller", "War", "Western")
str(movies)
movies$ID = NULL
movies$ReleaseDate = NULL
movies$IMDB = NULL
movies$VideoReleaseDate = NULL
str(movies)
names(movies)

distMovie = dist(movies[2:20], method="euclidean")
clustMovie = hclust(distMovie, method="ward.D")
plot(clustMovie)

groupMovie = cutree(clustMovie, k=10)
tapply(movies$Comedy, groupMovie, mean)

colMeans(subset(movies[2:20], groupMovie==1))
spl = split(movies[2:20], groupMovie)
lapply(spl, colMeans)
