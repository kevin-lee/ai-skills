package aiskills.core.utils

import java.util.Date

/** Generate an ISO-8601-like timestamp string, compatible with Scala Native. */
def isoNow(): String =
  val now = new Date()
  // Date.toISOString is not available, so we format manually
  val millis = now.getTime
  val secs = millis / 1000
  val ms = millis % 1000

  // Calculate date/time components from epoch millis
  val totalDays = (secs / 86400).toInt
  val timeOfDay = (secs % 86400).toInt
  val hours = timeOfDay / 3600
  val minutes = (timeOfDay % 3600) / 60
  val seconds = timeOfDay % 60

  // Compute year/month/day from days since epoch (1970-01-01)
  var y = 1970
  var remaining = totalDays
  while
    val daysInYear = if isLeapYear(y) then 366 else 365
    remaining >= daysInYear
  do
    val daysInYear = if isLeapYear(y) then 366 else 365
    remaining -= daysInYear
    y += 1

  val monthDays =
    if isLeapYear(y) then Array(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    else Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  var m = 0
  while m < 12 && remaining >= monthDays(m) do
    remaining -= monthDays(m)
    m += 1

  val day = remaining + 1
  val month = m + 1

  f"$y%04d-$month%02d-$day%02d" +
    f"T$hours%02d:$minutes%02d:$seconds%02d" +
    f".$ms%03dZ"

private def isLeapYear(y: Int): Boolean =
  (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)
