
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

# Compute the number of stations per year using the metadata file.
# Each row has a start and end date.  Use these to count the number of
# stations that are available each year.
#
# Some stations that, according to the metadata, were in existence during
# certain years may not have data recorded in the data set for those years.
mdStationsPerYear <- function() {
   files = list.files(path = dataDir) # Assume this contains only year files
   firstYear = 1901
   lastYear  = as.integer(format(Sys.time(), format="%Y"))
   years     = seq(firstYear, lastYear)   
   number    = rep(0, length(years))
   # Get the first year in the data set
   # Get the last year in the data set
   md = read_metadata()
   n = dim(md)[1]
   for (i in 1:n) {
      startYear = as.integer(substr(md$BEGIN[i], 1, 4))
      endYear   = as.integer(substr(md$END[i], 1, 4))
      #print(paste(i, startYear, endYear))
      for (year in seq(startYear, endYear)) {
         if (year == 1910)
	    print (paste(md$USAF[i], "-", md$WBAN[i], sep=''))
         number[year - firstYear + 1] = number[year - firstYear + 1] + 1
      }
   }
   return (data.frame("year"=years, "n"=number))   
}

# Are there any stations in the files that are not in the metadata
# during the years the metadata specifies?
sanity1 <- function() {
   md = read_metadata()
   years = list.files(path = dataDir)
   for (year in years) {
   #for each year
   # list the files in the year directory
   # extract the USAF and WBAN
   # Does the row exist in md?
   # Does the year occur in  the md range?
   }
}