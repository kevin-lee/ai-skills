package aiskills.core.utils

import java.util.Date

import scala.annotation.tailrec

/** Generate an ISO-8601-like timestamp string, compatible with Scala Native. */
def isoNow(): String = {
  val now    = new Date()
  // Date.toISOString is not available, so we format manually
  val millis = now.getTime
  val secs   = millis / 1000
  val ms     = millis % 1000

  // Calculate date/time components from epoch millis
  val totalDays = (secs / 86400).toInt
  val timeOfDay = (secs      % 86400).toInt
  val hours     = timeOfDay / 3600
  val minutes   = (timeOfDay % 3600) / 60
  val seconds   = timeOfDay  % 60

  // Compute year/month/day from days since epoch (1970-01-01)
  val (y, daysAfterYear) = computeYear(1970, totalDays)

  val monthDays =
    if isLeapYear(y) then Array(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    else Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  val (m, daysAfterMonth) = computeMonth(0, daysAfterYear, monthDays)

  val day   = daysAfterMonth + 1
  val month = m + 1

  f"$y%04d-$month%02d-$day%02d" +
    f"T$hours%02d:$minutes%02d:$seconds%02d" +
    f".$ms%03dZ"
}

@tailrec
private def computeYear(year: Int, remainingDays: Int): (Int, Int) = {
  val daysInYear = if isLeapYear(year) then 366 else 365
  if remainingDays >= daysInYear then computeYear(year + 1, remainingDays - daysInYear)
  else (year, remainingDays)
}

@tailrec
private def computeMonth(month: Int, remainingDays: Int, monthDays: Array[Int]): (Int, Int) = {
  if month < 12 && remainingDays >= monthDays(month) then computeMonth(
    month + 1,
    remainingDays - monthDays(month),
    monthDays
  )
  else (month, remainingDays)
}

private def isLeapYear(y: Int): Boolean =
  (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)
