################################################################################
##
## File:    CalendarEffects-Student-Functions.R
## 
## Purpose: Functions to manage calendar effects.
##
## Created: 2018.03.03
##
## Version: 2024.10.24
## 
################################################################################

require(timeDate)

################################################################################
## Utilities
################################################################################

.recode <- function(x, tab)
{
  x <- data.frame(prog = 1 : NROW(x), x = x)
  tab <- data.frame(x = tab[, 1], y = tab[, 2])
  ans <- merge(x = x, y = tab, by = "x", all.x = TRUE, all.y = FALSE)
  ans$y[order(ans$prog)]
}
# ------------------------------------------------------------------------------


################################################################################
## Calendar
################################################################################

.calendar <- 
function(time)
{
  #### Settings
  require(lubridate)
  ## nobs
  nobs <- NROW(time)
  if (nobs < 2)
  {
    stop("Argument 'time' must have at least two elements.")
  }
  nobs1 <- nobs + 1
  ####
  x1 <- time[2] - time[1]
  if (round(x1) == 1)
  {
    by <- "day"
    from <- time[1]
    to <- time[nobs]
  }
  else if (round(x1 / 7) == 1)
  {
    by <- "week"
    from <- floor_date(time[1], by)
    to <- ceiling_date(time[nobs], by)
  }
  else if (round(x1 / 30.4375) == 1)
  {
    by <- "month"
    from <- floor_date(time[1], by)
    to <- ceiling_date(time[nobs], by)
  }
  else if (round(x1 / 91.3125) == 1)
  {
    by <- "quarter"
    from <- floor_date(time[1], by)
    to <- ceiling_date(time[nobs], by)
  }

  #### Days
  x1 <- seq(from = from, by = by, to = to)

  #### Answer
  data.frame(time = time, from = x1[-NROW(x1)], to = x1[-1]-1)
}
# ------------------------------------------------------------------------------


.singleHolidays <- 
function(year, country = "it")
{
  ## FUNCTION:
  
  #### Italy
  if (country == "it")
  {
    x1 <- c("01-01", "01-06", "04-25", "05-01", "06-02", "08-15", "11-01", 
      "12-08", "12-25", "12-26" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x1 <- c( x1, as.character(EasterMonday(year)) )
  }
  #### US
  else if (country == "us")
  {
    x1 <- c("01-01", "02-14", "03-17", "07-04", "10-31", "12-24", "12-25", "12-31" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- USThanksgivingDay(year)
    x3 <- as.timeDate(as.Date(x2) + 1)
    x1 <- c( x1,  
      as.character( c( USMemorialDay(year), x2, x3, USLaborDay(year) ) ) )
  }
  #### Germany
  else if (country == "de")
  {
    x1 <- c("01-01", "05-01", "10-03", "12-25", "12-26" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- c( GoodFriday(year), EasterMonday(year), PentecostMonday(year) )
    x1 <- c( x1, as.character( x2 ) )
  }
  #### Spain
  else if (country == "es")
  {
    x1 <- c("01-01", "01-06", "05-01", "08-15", "10-12", "11-01", "12-06", "12-08", 
      "12-25" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x1 <- c( x1, as.character(GoodFriday(year)) )
  }
  #### France
  else if (country == "fr")
  {
    x1 <- c("01-01", "05-01", "05-08", "07-14", "08-15", "11-01", "11-11", 
      "12-25", "12-26" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- c( GoodFriday(year), EasterMonday(year), Ascension(year), 
      PentecostMonday(year) )
    x1 <- c( x1, as.character(x2) )
  }
  #### Belgium
  else if (country == "be")
  {
    x1 <- c("01-01", "05-01", "07-21", "08-15", "11-01", "11-11", "12-25" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- Ascension(year)
    x3 <- PentecostMonday(year)
    x1 <- c( x1, as.character(EasterMonday(year)), as.character( c(x2, x3) ) )
  }
  #### Netherlands
  else if (country == "nl")
  {
    x1 <- c("01-01", "05-05", "12-25", "12-26" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- .NLKingsBirthday(year)
    x3 <- Ascension(year)
    x4 <- PentecostMonday(year)
    x1 <- c( x1, as.character( c(GoodFriday(year), EasterMonday(year)) ), 
      as.character( c(x2, x3, x4) ) )
  }
  #### Japan
  else if (country == "jp")
  {
    x1 <- c("01-01", "02-11", "02-23", "03-21", "04-29", 
      "05-02", "05-03", "05-04", 
      "08-11", "09-23", "11-03", "11-23" )
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x2 <- .JPJanuary02(year)                ## 01-02 only if it is Monday
    x3 <- .JPHolidayMondayDays(year,  1, 2) ## 01-2nd Monday
    x4 <- .JPHolidayMondayDays(year,  7, 3) ## 07-3rd Monday
    x5 <- .JPHolidayMondayDays(year,  9, 3) ## 09-3rd Monday
    x6 <- .JPHolidayMondayDays(year, 10, 2) ## 10-2nd Monday
    x1 <- c(x1, x2, x3, x4, x5, x6)
  }
  #### UK
  else if (country == "uk")
  {
    x1 <- c("12-26")
    x1 <- outer(FUN = paste, X = year, Y = x1, sep = "-")
    x1 <- as.Date(x1)
    x2 <- .UKNewYearsDay(year)
    x3 <- as.Date(GoodFriday(year))
    x4 <- as.Date(EasterMonday(year))
    x5 <- .UKEarlyMay(year)
    x6 <- .UKSpringHoliday(year)
    x7 <- .UKSummerHoliday(year)
    x8 <- .UKXMas(year) 
    x1 <- c(x1, x2, x3, x4, x5, x6, x7, x8)
  }
  else
  {
    stop("Country ", country, " not yet implemented")
  }
  
  #### Answer
  sort( as.Date( x1 ) )
}
# ------------------------------------------------------------------------------

.AscensionDay <- function(year)
{
  as.Date(EasterSunday(year)) + 39
}
# ------------------------------------------------------------------------------

.PentecostMonday <- function(year)
{
  as.Date(EasterSunday(year)) + 50
}
# ------------------------------------------------------------------------------

.NLKingsBirthday <- function(year)
{
  x1 <- as.Date( paste0(year, "-04-27") )
  ind <- format(x1, "%u") == "1"
  x1[ind] <- x1[ind] - 1
  x1
}
# ------------------------------------------------------------------------------

#### 01-02 only if it is Monday
.JPJanuary02 <- function(year)
{
  x1 <- as.Date( paste0(year, "-01-02") )
  ind <- format(x1, "%u") == "1"
  format(x1[ind], "%Y-%m-%d")
}
# ------------------------------------------------------------------------------


#### day-th Monday within months
.JPHolidayMondayDays <- function(year, month, day)
{
  fun <- function(year, month, day)
  {
    x1 <- seq(from = as.Date( paste0(year, "-", month, "-01") ), 
      by = "1 day", length.out = day * 7)
    ind <- format(x1, "%u") == "1"
    format(x1[ind][day], "%Y-%m-%d")
  }
  mapply(FUN = fun, year = year, 
    MoreArgs = list(month = month[1], day = day[1]), SIMPLIFY = TRUE)
}
# ------------------------------------------------------------------------------


#### UK
.UKNewYearsDay <- function(year)
{
  fun <- function(year)
  {
    x1 <- seq(from = as.Date( paste0(year, "-01-01") ), 
      by = "1 day", length.out = 7)
    ind <- !( format(x1, "%u") %in% c("6","7") )
    x1[ind][1]
  }
  as.Date( mapply(FUN = fun, year = year, SIMPLIFY = TRUE) )
}
# ------------------------------------------------------------------------------
.UKEarlyMay <- function(year)
{
  fun <- function(year)
  {
    x1 <- seq(from = as.Date( paste0(year, "-05-01") ), 
      by = "1 day", length.out = 7)
    ind <- format(x1, "%u") == "1"
    x1[ind]
  }
  as.Date( mapply(FUN = fun, year = year, SIMPLIFY = TRUE) )
}
# ------------------------------------------------------------------------------
.UKSpringHoliday <- function(year)
{
  fun <- function(year)
  {
    x1 <- seq(from = as.Date( paste0(year, "-05-31") ), 
      by = "-1 day", length.out = 7)
    ind <- format(x1, "%u") == "1"
    x1[ind]
  }
  as.Date( mapply(FUN = fun, year = year, SIMPLIFY = TRUE) )
}
# ------------------------------------------------------------------------------
.UKSummerHoliday <- function(year)
{
  fun <- function(year)
  {
    x1 <- seq(from = as.Date( paste0(year, "-08-31") ), 
      by = "-1 day", length.out = 7)
    ind <- format(x1, "%u") == "1"
    x1[ind]
  }
  as.Date( mapply(FUN = fun, year = year, SIMPLIFY = TRUE) )
}
# ------------------------------------------------------------------------------
.UKXMas <- function(year)
{
  fun <- function(year)
  {
    x1 <- as.Date( paste0(year, "-12-25") )
    x2 <- format(x1, "%u")
    x3 <- if (x2 %in% c("1", "2", "3", "4"))
    {
      c("25", "26")
    }
    else if (x2 == "5")
    {
      c("25", "28")
    }
    else if (x2 == "6")
    {
      c("27", "28")
    }
    else
    {
      c("26", "27")
    }
    paste0(year, "-12-", x3)
  }
  as.Date( mapply(FUN = fun, year = year, SIMPLIFY = TRUE) )
}
# ------------------------------------------------------------------------------

.easterHolidays <- 
function(year, len = 3)
{
  ## FUNCTION:
  
  #### Easter dates
  easter <- as.Date( EasterSunday( year ) )
  #### Answer
  fun <- function(easter, len)
  {
    seq(from = easter - 1, length.out = len, by = -1)
  }
  as.Date( unlist( 
    mapply( FUN = fun, easter = easter, MoreArgs = list(len = len), 
      SIMPLIFY = FALSE ) ) )
}
# ------------------------------------------------------------------------------


.calendarEffects.2 <- 
function(from, to, 
  easter.len = 3, country = "it")
{
  #### Year (used below)
  x1 <- as.numeric( format(x = c(from, to), format = "%Y") )
  year <- unique( x1[1] : x1[2] )

  #### Weekdays
  ## Start from from[1] - 1 and ends at to[1] + 1 to manage long weekend 
  ## holidays
  xx  <- seq(from = from[1] - 1, to = to[1] + 1, by = "day")
  wdx <- format(xx, "%u")
  #### Recode wd expressed in Italian to US
  x1 <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
  x1 <- data.frame(as.character(1 : NROW(x1)), x1)
  wdx <- .recode(x = wdx, tab = x1)

  #### Easter
  if (country == "it")
  {
    eh <- .easterHolidays(year = year, len = easter.len[1])
    ind <- xx %in% eh
    wdx[ind] <- "eh"
  }
  #### Single holidays
  single <- .singleHolidays(year = year, country = country[1])
  ind.single <- xx %in% single  

  #### Improve workdays: single holidays happening in weekdays different from Sun 
  ##   are isolated
  ind <- wdx != "Sun" & ind.single
  wdx[ind] <- "sh"

  #### Long weekend
  ind <- wdx %in% c("Sun", "sh")
  n1 <- NROW(ind)
  ind <- c(FALSE, ind[ 3 : n1 ] & !ind[2 : (n1 - 1)] & ind[ 1 : (n1 - 2) ], FALSE )
  wdx[ind] <- "lh"
  
  #### Cut the extra added days
  wdx[ -c(1, NROW(wdx)) ]
}
# ------------------------------------------------------------------------------


.calendarEffects.1 <- 
function(from, to, 
  easter.len = 3, country = "it")
{  
  #### Workdays 
  wd <- .calendarEffects.2(from = from, to = to, easter.len = easter.len, 
    country = country)

  #### Table
  weekd  <- table(wd, dnn = NULL)

  #### Append single, long weekend and Easter holidays if not included
  ind <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh") 
  x1 <- numeric(NROW(ind))
  names(x1) <- ind
  ind <- !(names(x1) %in% names(weekd))
  if ( any( ind ) )
  {
    weekd <- c(weekd, x1[ind])
  } 

  #### Workdays imbalance variable
  ind <- names(weekd) %in% c("Mon", "Tue", "Wed", "Thu", "Fri")
  workd  <- sum(weekd[ind]) - 2.5 * sum(weekd[!ind]) 
  
  #### Answer
  ## To sort
  ind <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh") 
  c(weekd[ind], wi = workd, nd = sum(weekd))
}
# ------------------------------------------------------------------------------


.calendarEffects <- 
function(time, country = "it")
{
  ##############################################################################
  ## Arguments
  ##  time:    (Date[n]) vector of dates.
  ##  country: (character[1]) abbreviated (two letter) country.
  ## Value
  ##  A matrix reporting, in each row (corresponding to a week, a month or a 
  ##   quarter), the number of days corresponding to the following types of days: 
  ##   "Mon": number of Mondays which are not sh, lh, or eh (see below).
  ##   "Tue": number of Tuesdays which are not sh, lh, or eh (see below).
  ##   ...
  ##   "Sat": number of Saturdays which are not sh, lh, or eh (see below).
  ##   "Sun": number of Sundays.
  ##   "sh":  number of single holidays (holidays not falling in Sun).
  ##   "lh":  number of long weekends (giorni di ponte) excluding Sun and 
  ##     sh.
  ##   "eh":  number of Easter holidays (different from Easter, included in 
  ##     Sun, and Easter Monday included in sh). By default, each year, 3 days 
  ##     are considered as eh.
  ##   "wi":  workdays imbalance. It is computed as 
  ##     wd = (Mon + Tue + ... + Fri) - 2.5 * (Sat + Sun).
  ##   "nd":  number of days. It is equal to (Mon + ... + Sun + sh + lh + eh)  
  ##############################################################################

  ## FUNCTION:
  
  #### Calendar
  # time <- as.Date( paste0(format(time, "%Y-%m-"), "01") ) 
  x1 <- .calendar(time = time)
  x1.g <<- x1
  #### Calendar effects
  x2 <- .calendarEffects.1(from = x1$from[1], to = x1$to[1], country = country)
  x1 <- do.call(what = rbind, args = mapply(FUN = .calendarEffects.1, 
    from = x1$from, to = x1$to, country = country, SIMPLIFY = FALSE) )
  #### Cut types with zero days
  ind <- apply(X = (x1 != 0), MARGIN = 2, FUN = any)
  x1 <- x1[, ind, drop = FALSE]
  #### Answer
  data.frame(time = time, x1)
}
# ------------------------------------------------------------------------------


.extend.time <- 
function(x, n.ahead, by = "month")
{
  ##############################################################################
  ## Arguments
  ##  x:       (Date[n]) vector of dates.
  ##  n.ahead: (numeric[1]) number of values after the last date in x.
  ##  by:      (character[1]) on among "day", "week", "month", "quarter", 
  ##           "year".
  ## Value
  ##  The argument x extended some steps ahead.
  ##############################################################################

  #### Answer
  seq(from = x[NROW(x)], by = by, length.out = n.ahead + 1)[-1]
}
# ------------------------------------------------------------------------------


.day.dummy <- function(from, to, country, ref = "Sun")
{
  ##############################################################################
  ## Arguments
  ##  from:    (Date[1]) starting date.
  ##  to:      (Date[1]) ending date.
  ##  country: (character[1]) country (2 letters code).
  ##  ref:     (character[1]) day taken as reference.
  ## 
  ## Value
  ##  Matrix of daily dummies.
  ##############################################################################

    #### Calendar
  cal <- .calendarEffects.2(from = from, to = to, country = country)
  #### Dummies
  x <- model.matrix(object = ~ 0 + cal)
  #### Fix names
  colnames(x) <- substr(colnames(x), 4, 1000)
  #### Sort
  ind <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh")
  x <- x[, ind, drop = FALSE]
  #### Remove the reference
  ind <- colnames(x) != ref[1]
  x[, ind, drop = FALSE]
}
# ------------------------------------------------------------------------------
