
dataDir     = '/ssd0/weather/data'
metadataDir = '/ssd0/weather/metadata'

read_metadata <- function() {
   h = read.csv(paste(metadataDir, "/", "isd-history.csv", sep=''), header=T)
   return (h)
}

get_stations_by_country <- function(h, ctry='US') {
   hh = h[h$CTRY == ctry,]
   return (hh)
}

getStationInfo <- function(usaf = "029070", wban = "99999") {
   md   = read_metadata()
   return (md[((md$USAF == usaf) & (md$WBAN == wban)), ])
}

getStations <- function(year = "1901") {
   files = list.files(path = paste(dataDir, "/", year, sep=''))
   stations = c()
   print (paste(dataDir, "/", year, sep=''))
   print(length(files))
   for (name in files) {
      n = nchar(name)
      m = nchar("-YYYY.gz")
      stations = c(stations, (substr(name, 1, n - m)))
   }
   return (unique(stations))
}

# Compute the number of files for each year in the dataset.
# The return value is a data frame with two colums: year and n
# where year is the year and n is the number of files in the
# corresponding year.  
filesPerYear <- function() {
   yearStrings = list.files(path = dataDir)
   years       = c()
   number      = c()
   year1 = as.numeric(yearStrings[1])
   for (ys in yearStrings) {
      if (nchar(ys) == 4) {      # Rough check that file name is YYYY
         year   = as.numeric(ys)
         path   = paste(dataDir, '/', ys, sep='')
         files  = list.files(path = path)
         years  = c(years, year)
         number = c(number, length(files))
      }
   }
   return (data.frame("year"=years, "n"=number))
}

# Compute the total number of files in the dataset.
nfiles <- function() {
   x = filesPerYear()
   return (sum(x$n))
}

