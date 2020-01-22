# R commands for working with the National Climatic Data Center
# Integrated Surface Data.  This is a large set of hourly weather
# readings from stations across the globe staring in 1907.
#
# Author:  Ralph Wojtowicz
# Email :  rwojtowi@su.edu

# Assume that the data files are organized as on the government ftp site
# and located in a directory called data.  Within data, are directories
# for each year and within each year are .gz files for weather stations.
dataDir = "../data"

# See the ish-format-document.pdf for detailed documentation of the
# data file format.

# Import the time series library xts.  Each reading has not only a date
# but an hour and minute.  xts time series only support dates as indices.
# One option will be to write tools for working with date-time indices.
# Another is to write functions for taking daily averages and then converting
# weather data to xts objects
library(xts)

# POS: 1-4
# TOTAL-VARIABLE-CHARACTERS
getVariableCharacters <- function(line) {
   data = substr(line, 1, 4)
   start = 1
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 4)
      t = strtoi(substr(data, start, 4))
   return (t)
}

# POS: 5-10
# FIXED-WEATHER-STATION USAF MASTTER STTATITON CATALOG identifier
getUSAF.id <- function(line) {
   return (substr(line, 5, 10))
}

# POS: 11-15
# FIXED-WEATHER-STATION NCEI WBAN identifier
getWBAN.id <- function(line) {
   return (substr(line, 11, 15))
}

# POS: 16-19
# GEOPHYSICAL-POINT-OBSERVATION year
getYear <- function(line) {
   return (substr(line, 16, 19))
}

# POS: 20-21
# GEOPHYSICAL-POINT-OBSERVATION month
getMonth <- function(line) {
   return (substr(line, 20, 21))
}

# POS: 22-23
# GEOPHYSICAL-POINT-OBSERVATION day
getDay <- function(line) {
   return (substr(line, 22, 23))
}

# POS: 24-25
# GEOPHYSICAL-POINT-OBSERVATION hour
getHour <- function(line) {
   return (substr(line, 24, 25))
}

# POS: 26-27
# GEOPHYSICAL-POINT-OBSERVATION min
getMin <- function(line) {
   return (substr(line, 26, 27))
}

# POS: 28-28
# GEOPHYSICAL-POINT-OBSERVATION date source flag
getSource <- function(line) {
   return (substr(line, 28, 28))
}

# POS: 29-34
# GEOPHYSICAL-POINT-OBSERVATION latitude coordinate
getLat <- function(line) {
   data = substr(line, 29, 34)
   sign = 1
   if (substr(data, 1, 1) == '-')
      sign = -1
   start = 2;
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 6)
      t = sign * strtoi(substr(data, start, 6))
   return (t/1000)
}

# POS: 35-41
# GEOPHYSICAL-POINT-OBSERVATION longitude coordinate
getLon <- function(line) {
   data = substr(line, 35, 41)
   sign = 1
   if (substr(data, 1, 1) == '-')
      sign = -1
   start = 2;
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 6)
      t = sign * strtoi(substr(data, start, 6))
   return (t/1000)
}

# POS: 42-46
# GEOPHYSICAL-REPORT-TYPE code
getReportType <- function(line) {
   return (substr(line, 42, 46))
}

# POS: 47-51
# GEOPHYSICAL-POINT-OBSERVATION	elevation dimension
getElevation <- function(line) {
   data = substr(line, 47, 51)
   sign = 1
   if (substr(data, 1, 1) == '-')
      sign = -1
   start = 2;
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 5)
      t = sign * strtoi(substr(data, start, 5))
   return (t)
}

# POS: 52-56
# FIXED-WEATHER-STATION call letter identifier
getCallLetterID <- function(line) {
   return (substr(line, 52, 56))
}

# POS: 57-60
# METEOROLOGICAL-POINT-OBSERVATION quality control process name
getQualityControlProcessName <- function(line) {
   return (substr(line, 57, 60))
}

# POS: 61-63
# WIND-OBSERVATION direction angle
getDirection <- function(line) {
   data = substr(line, 61, 63)
   start = 1
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 3)
      t = strtoi(substr(data, start, 3))
   if (t == 999)
      return (NA)
   return (t)
}

# POS: 64-64
# WIND-OBSERVATION direction quality code
getDirectionQuality <- function(line) {
   return (substr(line, 64, 64))
}

# POS: 65-65
# WIND-OBSERVATION type code
getType <- function(line) {
   return (substr(line, 65, 65))
}

# POS: 66-69
# WIND-OBSERVATION speed rate
getSpeed <- function(line) {
   data = substr(line, 66, 69)
   start = 1
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 4)
      t = strtoi(substr(data, start, 4))
   if (t == 9999)
      return (NA)
   return (t/10)
}

# POS: 70-70
# WIND-OBSERVATION speed quality code
getSpeedQuality <- function(line) {
   return (substr(line, 70, 70))
}

# POS: 71-75
# SKY-CONDITION-OBSERVATION ceiling height dimension
getCeilingHeight <- function(line) {
   data = substr(line, 71, 75)
   start = 1
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 5)
      t = strtoi(substr(data, start, 5))
   if (t == 99999)
      return (NA)
   return (t)   
}

# POS: 76-76
# SKY-CONDITION-OBSERVATION ceiling quality code
getCeilingQuality <- function(line) {
   return (substr(line, 76, 76))
}

# POS: 77-77
# SKY-CONDITION-OBSERVATION ceiling determination code
getCeilingDetermination <- function(line) {
   return (substr(line, 77, 77))
}

# POS: 78-78
# SKY-CONDITION-OBSERVATION CAVOK code
getCavok <- function(line) {
   return (substr(line, 78, 78))
}

# POS: 79-84
# VISIBILITY-OBSERVATION distance dimension
getDistance <- function(line) {
   data = substr(line, 79, 84)
   start = 1
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 6)
      t = strtoi(substr(data, start, 6))
   if (t == 999999)
      return (NA)
   return (t)   
}

# POS: 85-85
# VISIBILITY-OBSERVATION distance quality code
getDistanceQuality <- function(line) {
   return (substr(line, 85, 85))
}

# POS: 86-86
# VISIBILITY-OBSERVATION variability code
getVariability <- function(line) {
   return (substr(line, 86, 86))
}

# POS: 87-87
# VISIBILITY-OBSERVATION quality variability code
getQualityVariability <- function(line) {
   return (substr(line, 87, 87))
}

# POS: 88-92
# AIR-TEMPERATURE-OBSERVATION air temperature
getTemperature <- function(line) {
   data = substr(line, 88, 92)
   sign = 1
   if (substr(data, 1, 1) == '-')
      sign = -1
   start = 2;
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 5)
      t = sign * strtoi(substr(data, start, 5))
   if (t == 9999)
      return (NA)
   return (t/10)
}

# POS: 93-93
# AIR-TEMPERATURE-OBSERVATION air temperature quality code
getTemperatureQuality <- function(line) {
   return (substr(line, 93, 93))
}

# POS: 94-98
# AIR-TEMPERATURE-OBSERVATION dew point temperature
getDewPoint <- function(line) {
   data = substr(line, 94, 98)
   sign = 1
   if (substr(data, 1, 1) == '-')
      sign = -1
   start = 2;
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 5)
      t = sign * strtoi(substr(data, start, 5))
   if (t == 9999)
      return (NA)
   return (t/10)
}

# POS: 99-99
# AIR-TEMPERATURE-OBSERVATION dew point quality code
getDewPointQuality <- function(line) {
   return (substr(line, 99, 99))
}

# POS: 100-104
# ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure
getPressure <- function(line) {
   data = substr(line, 100, 104)
   start = 1
   while (substr(data, start, start) == '0')
      start = start + 1
   t = 0
   if (start < 5)
      t = strtoi(substr(data, start, 5))
   return (t/10)
}

# POS: 105-105
# ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure quality code
getPressureQuality <- function(line) {
   return (substr(line, 105, 105))
}


getLines <- function(filename) {
   xz = gzcon(file(filename, open='rb'))
   lines = readLines(xz)
   close(xz)
   return(lines)
}

getStationDataForYear <- function(stationID, year) {
  filename = paste(dataDir, "/", year, "/", stationID, "-", year, ".gz", sep="")
   xz = gzcon(file(filename, open='rb'))
   lines = readLines(xz)
   close(xz)
   usaf.identifiers = c()
   wban.identifiers = c()
   
   years  = c()
   months = c()
   days   = c()
   
   hours  = c()
   mins   = c()
   datetimes  = c()

   data.source.flags = c()

   latitudes = c()
   longitudes = c()

   report.types = c()

   elevations = c()
   call.letter.ids = c()
   quality.control.process.names = c()

   direction.angles = c()
   direction.qualities = c()
   types = c()
   speeds = c()
   speed.qualities = c()

   ceiling.heights = c()
   ceiling.qualities = c()
   ceiling.determinations = c()
   cavoks = c()

   distances = c()
   distance.qualities = c()
   variabilities.codes = c()
   quality.variabilities = c()

   air.temperatures = c()
   air.temperature.qualities = c()
   dew.point.temperatures = c()
   dew.point.qualities = c()

   sea.level.pressures = c()
   sea.level.pressure.qualities = c()

   xz = gzcon(file(filename, open='rb'))
   lines = readLines(xz)
   close(xz)
   for (i in 1:length(lines)) {
      line = lines[i]

      year   = getYear(line)
      years  = c(years, year)
      month  = getMonth(line)
      months = c(months, month)
      day    = getDay(line)
      days   = c(days, day)

      hour   = getHour(line)
      hours  = c(hours, hour)
      min    = getMin(line)
      mins   = c(mins, min)

      dt     = paste(year, "-", month, "-", day, " ", hour, ":", min, ":00", sep='')
      dddd   = as.POSIXct(dt, format="%Y-%m-%d %H:%M", tz='')
      if (length(datetimes) == 0) 
         datetimes = dddd 
      else 	 
         datetimes  = c(datetimes, dddd) 

      usaf.identifiers = c(usaf.identifiers, getUSAF.id(line))
      wban.identifiers = c(wban.identifiers, getWBAN.id(line))
      data.source.flags = c(data.source.flags, getSource(line))
      latitudes = c(latitudes, getLat(line))
      longitudes = c(longitudes, getLon(line))

      report.types = c(report.types, getReportType(line))

      elevations = c(elevations, getElevation(line))
      call.letter.ids = c(call.letter.ids, getCallLetterID(line))
      quality.control.process.names = c(quality.control.process.names,
          getQualityControlProcessName(line))

      direction.angles = c(direction.angles, getDirection(line))
      direction.qualities = c(direction.qualities, getDirectionQuality(line))
      types = c(types, getType(line))
      speeds = c(speeds, getSpeed(line))
      speed.qualities = c(speed.qualities, getSpeedQuality(line))

      ceiling.heights = c(ceiling.heights, getCeilingHeight(line))
      ceiling.qualities = c(ceiling.qualities, getCeilingQuality(line))
      ceiling.determinations = c(ceiling.determinations, getCeilingDetermination(line))
      cavoks = c(cavoks, getCavok(line))

      distances = c(distances, getDistance(line))
      distance.qualities = c(distance.qualities, getDistanceQuality(line))
      variabilities.codes = c(variabilities.codes, getVariability(line))
      quality.variabilities = c(quality.variabilities, getQualityVariability(line))

      air.temperatures = c(air.temperatures, getTemperature(line))
      air.temperature.qualities = c(air.temperature.qualities, getTemperatureQuality(line))
      dew.point.temperatures = c(dew.point.temperatures, getDewPoint(line))
      dew.point.qualities = c(dew.point.qualities, getDewPointQuality(line))

      sea.level.pressures = c(sea.level.pressures, getPressure(line))
      sea.level.pressure.qualities = c(sea.level.pressure.qualities,
         getPressureQuality(line))

   }
   x = data.frame(datetime = datetimes,
                  usaf.identifier = usaf.identifiers,
                  wban.identifier = wban.identifiers,
		  data.source.flag = data.source.flags,
		  latitude=latitudes,
		  longitude = longitudes,
		  report.type = report.types,
		  elevation   = elevations,
		  call.letter.id = call.letter.ids,
		  quality.control.process.name = quality.control.process.names,
                  direction.angle = direction.angles,
                  direction.quality = direction.qualities,
                  type = types,
                  speed = speeds,
                  speed.quality = speed.qualities,
                  ceiling.height = ceiling.heights,
                  ceiling.quality = ceiling.qualities,
                  ceiling.determination = ceiling.determinations,
                  cavok = cavoks,
                  distance = distances,
                  distance.quality = distance.qualities,
                  variability = variabilities.codes,
                  quality.variability = quality.variabilities,
                  air.temperature = air.temperatures,
                  air.temperature.quality = air.temperature.qualities,
                  dew.point.temperature = dew.point.temperatures,
                  dew.point.quality = dew.point.qualities,
                  sea.level.pressure = sea.level.pressures,
                  sea.level.pressure.quality = sea.level.pressure.qualities)
    return (x)
}

getStationData <- function(stationID) {
   x = c()
   years = list.files(dataDir)
   for (year in years) {
      files = list.files(paste(dataDir, "/", year, sep=''))
      for (file in files) {
         m = nchar("-1901.gz")
	 n = nchar(file)
         sid = substr(file, 1, n - m)
	 if (sid == stationID) {
	    print (file)
	    if (length(x) == 0) {
	       x = getStationDataForYear(stationID, year)
	    }
	    else {
	       x = rbind(x, getStationDataForYear(stationID, year))
            }
	 }
      }
   }
   return (x)
}

