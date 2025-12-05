import java.nio.charset.StandardCharsets
import scala.io.Source

// remembers which files we already warned about
object CSVCache {
  var alreadyPrinted = Set[String]()
}

trait CSVReader {
  // Reads a CSV file and returns the contents as a List[Array[String]].
  // Duplicate rows (after the header) are removed while preserving order.

  def readCSV(filePath: String): List[Array[String]] = {
    // Open the CSV file with ISO_8859_1 encoding
    val bufferedSource = Source.fromFile(filePath, StandardCharsets.ISO_8859_1.toString)
    val lines = bufferedSource.getLines().toList // Read all lines into a list
    bufferedSource.close() // Close the file
    // Handle empty CSV file
    if (lines.isEmpty) {
      println("Error: CSV file is empty.")
      return List.empty
    }

    // Extract header row (first line)
    val header = lines.head.split(",").map(_.trim)

    // Extract data rows (all lines after the header)
    val dataRows = lines.tail.map(_.split(",").map(_.trim))

    // Check for duplicates and also make sure the notice is printed once
    if (!CSVCache.alreadyPrinted.contains(filePath)) {
      val uniqueRowsSet = dataRows.map(_.mkString(",")).toSet
      if (uniqueRowsSet.size < dataRows.size) {
        println(s"Notice: ${dataRows.size - uniqueRowsSet.size} duplicate row(s) found and removed.")
        println("")
      }
      CSVCache.alreadyPrinted += filePath // mark this file as processed
    }

    // Remove exact duplicate rows
    val uniqueDataRows = dataRows.map(_.mkString(",")).toSet.map(_.split(",")).toList

    // Return header followed by unique data rows
    header +: uniqueDataRows
  }
}

// Utility methods for analyzing lists
trait AnalysisUtils {

  // Shared: find column index by name
  protected def columnIndex(csvData: List[Array[String]], name: String): Option[Int] = {
    csvData.headOption.flatMap { header =>
      val idx = header.indexOf(name)
      if (idx >= 0) Some(idx) else None
    }
  }

  // Shared: extract all values from one column
  protected def getColumn(csvData: List[Array[String]], name: String): List[String] = {
    columnIndex(csvData, name) match {
      case Some(idx) =>
        csvData.tail
          .filter(row => row.length > idx && row(idx).nonEmpty)
          .map(row => row(idx))
      case None => List.empty
    }
  }

  // Counts how many times each element appears in a list
  def countOccurrences[T](list: List[T]): Map[T, Int] =
    list.groupBy(identity)    // Group identical elements together
      .view.mapValues(_.size) // Count how many items are in each group
      .toMap                 // Convert to a normal Map

  // Finds the element that appears the most
  def maxByCount[T](counts: Map[T, Int]): (T, Int) =
    counts.maxBy(_._2)        // Return the element with the highest count

  //Helpers that help to convert the values safely
  //Helper to safely convert string to double
  protected def safeDouble(s: String): Double = scala.util.Try(s.toDouble).getOrElse(0.0)

  //Helper to safely convert string to int
  protected def safeInt(s: String): Int = scala.util.Try(s.toInt).getOrElse(0)

  //Helper to safely convert string percentage into normal numbers for calculation
  protected def safePercent(s: String): Double =
    scala.util.Try(s.replace("%", "").trim.toDouble).getOrElse(0.0) / 100.0

  //Safe min-max normalization (prevents division by zero, fair scoring)
  protected def normalize(value: Double, min: Double, max: Double, lowerBetter: Boolean): Double = {
    if (max <= min) 0.5 // no variation → neutral score
    else if (lowerBetter) 1 - (value - min) / (max - min)
    else (value - min) / (max - min)
  }
}


// Class to generate a report about favorite countries
// filePath: Path to the CSV file
class FavoriteCountryReport(filePath: String)
  extends CSVReader with AnalysisUtils { // Assumes CSVReader can read CSV and AnalysisUtils has helper methods

  private val csvData: List[Array[String]] = readCSV(filePath)      // Read the CSV file into a list of rows

  // Returns the favorite country (most frequent "Destination Country") and its count
  // Returns None if no countries are present
  def favoriteCountry: Option[(String, Int)] = {
    val countries = getColumn(csvData, "Destination Country") // Extract the "Destination Country" column
    if (countries.nonEmpty)
      Some(maxByCount(countOccurrences(countries))) // Count occurrences and find the most frequent
    else None
  }
}

class HotelAnalyzer(private val filePath: String)
  extends CSVReader with AnalysisUtils { // Assumes CSVReader can read CSV and AnalysisUtils has helper methods

  private val csvData: List[Array[String]] = readCSV(filePath) // Read the CSV file into a list of rows

  // A small detail list for each hotel
  case class HotelStats(
                         name: String,
                         country: String,
                         city: String,
                         avgPrice: Double,
                         avgDiscount: Double,
                         avgProfitMargin: Double,
                         totalVisitors: Int,
                         bookings: Int,
                         scorePrice: Double = 0.0,
                         scoreDiscount: Double = 0.0,
                         scoreMargin: Double = 0.0,
                         totalScore: Double = 0.0,
                         totalProfit: Double = 0.0
                       )

  // Method to find best value hotel for the customer
  def BestValueHotel: Option[HotelStats] = {

    // Extract columns that is needed for the analysis
    val hotelIdx = columnIndex(csvData, "Hotel Name")
    val countryIdx = columnIndex(csvData, "Destination Country")
    val cityIdx = columnIndex(csvData, "Destination City")
    val priceIdx = columnIndex(csvData, "Booking Price[SGD]")
    val discIdx = columnIndex(csvData, "Discount")
    val marginIdx = columnIndex(csvData, "Profit Margin")
    val peopleIdx = columnIndex(csvData, "No. Of People")

    // if cant fetch any data it will show this message and end the method
    if (List(hotelIdx, countryIdx, cityIdx, priceIdx, discIdx, marginIdx, peopleIdx).contains(None)) {
      println("Warning: Missing required columns for BestValueHotel analysis.")
      return None
    }

    // Go through every row and collect the values we need
    val stats = csvData.tail
      .map { row =>
        (
          (row(hotelIdx.get), row(countryIdx.get), row(cityIdx.get)),
          (
            safeDouble(row(priceIdx.get)),
            safePercent(row(discIdx.get)),
            safeDouble(row(marginIdx.get)),
            safeInt(row(peopleIdx.get))
          )
        )
      }
      .groupBy(_._1) // Group bookings by hotel (name, country, city)
      .view.mapValues { entries =>
        val rows = entries.map(_._2)
        val n = rows.size
        HotelStats(
          name = entries.head._1._1,
          country = entries.head._1._2,
          city = entries.head._1._3,
          avgPrice = rows.map(_._1).sum / n,
          avgDiscount = rows.map(_._2).sum / n,
          avgProfitMargin = rows.map(_._3).sum / n,
          totalVisitors = rows.map(_._4).sum,
          bookings = n
        )
      }
      .values
      .toList

    // If there is no data found it will return nothing
    if (stats.isEmpty) None
    else {

      // Find min/max for normalization
      val minPrice = stats.minBy(_.avgPrice).avgPrice
      val maxPrice = stats.maxBy(_.avgPrice).avgPrice
      val minDiscount = stats.minBy(_.avgDiscount).avgDiscount
      val maxDiscount = stats.maxBy(_.avgDiscount).avgDiscount
      val minMargin = stats.minBy(_.avgProfitMargin).avgProfitMargin
      val maxMargin = stats.maxBy(_.avgProfitMargin).avgProfitMargin

      // Score each hotel fairly using safe normalization
      val scoredStats = stats.map { h =>
        val scorePrice    = normalize(h.avgPrice, minPrice, maxPrice, lowerBetter = true)     // lower price = better
        val scoreDiscount = normalize(h.avgDiscount, minDiscount, maxDiscount, lowerBetter = false)  // higher discount = better
        val scoreMargin   = normalize(h.avgProfitMargin, minMargin, maxMargin, lowerBetter = true) // lower margin = better for customer

        val totalScore = (scorePrice + scoreDiscount + scoreMargin) / 3.0

        h.copy(
          scorePrice = scorePrice,
          scoreDiscount = scoreDiscount,
          scoreMargin = scoreMargin,
          totalScore = totalScore
        )
      }

      // Return the hotel with the highest score which is the most economical one
      Some(scoredStats.maxBy(_.totalScore))
    }
  }

  // Method to find the most profitable hotel
  def mostProfitableHotel: Option[HotelStats] = {

    // Extract columns that is needed for the analysis
    val hotelIdx = columnIndex(csvData, "Hotel Name")
    val countryIdx = columnIndex(csvData, "Destination Country")
    val cityIdx = columnIndex(csvData, "Destination City")
    val priceIdx = columnIndex(csvData, "Booking Price[SGD]")
    val discIdx = columnIndex(csvData, "Discount")
    val marginIdx = columnIndex(csvData, "Profit Margin")
    val peopleIdx = columnIndex(csvData, "No. Of People")

    // Check for required columns
    if (List(hotelIdx, countryIdx, cityIdx, priceIdx, discIdx, marginIdx, peopleIdx).contains(None)) {
      println("Warning: Missing required columns for mostProfitableHotel analysis.")
      return None
    }

    // ggregate all necessary stats for all hotels
    val stats = csvData.tail
      .map { row =>
        (
          (row(hotelIdx.get), row(countryIdx.get), row(cityIdx.get)),
          (
            safeDouble(row(priceIdx.get)),
            safePercent(row(discIdx.get)),
            safeDouble(row(marginIdx.get)),
            safeInt(row(peopleIdx.get))
          )
        )
      }
      .groupBy(_._1) // Group bookings by hotel (name, country, city)
      .view.mapValues { entries =>
        val rows = entries.map(_._2)
        val n = rows.size
        // Calculate Total Profit here to populate total profit
        val totalProfit = entries.map { case (_, (price, _, margin, people)) => price * margin * people }.sum

        HotelStats(
          name = entries.head._1._1,
          country = entries.head._1._2,
          city = entries.head._1._3,
          avgPrice = rows.map(_._1).sum / n,
          avgDiscount = rows.map(_._2).sum / n,
          avgProfitMargin = rows.map(_._3).sum / n,
          totalVisitors = rows.map(_._4).sum,
          bookings = n,
          totalProfit = totalProfit
        )
      }
      .values
      .toList

    if (stats.isEmpty) None
    else {

      // Find min/max for normalization
      val minMargin = stats.minBy(_.avgProfitMargin).avgProfitMargin
      val maxMargin = stats.maxBy(_.avgProfitMargin).avgProfitMargin
      // Convert Ints to Double for min/max comparison for normalization
      val minVisitors = stats.minBy(_.totalVisitors).totalVisitors.toDouble
      val maxVisitors = stats.maxBy(_.totalVisitors).totalVisitors.toDouble

      // core each hotel based on Margin and Visitors
      val scoredStats = stats.map { h =>
        // higher margin = better for hotel
        val scoreMargin = normalize(h.avgProfitMargin, minMargin, maxMargin, lowerBetter = false)
        // higher visitors = better for hotel
        val scoreVisitors = normalize(h.totalVisitors.toDouble, minVisitors, maxVisitors, lowerBetter = false)

        // Calculate Total Score
        // This is the Profitability Score, balancing scale and efficiency
        val totalScore = (scoreMargin + scoreVisitors) / 2.0

        h.copy(
          scoreMargin = scoreMargin,
          totalScore = totalScore
        )
      }

      // Return the hotel with the highest Profitability Score
      Some(scoredStats.maxBy(_.totalScore))
    }
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    // Read the CSV file
    val filePath = "resources/Hotel_Dataset.csv"
    val countryReport = new FavoriteCountryReport(filePath)
    val hotelReport   = new HotelAnalyzer(filePath)

    // Print the country name and the total amount
    countryReport.favoriteCountry match {
      case Some((country, count)) =>
        println(s"Country with the highest number of bookings: $country")
      case None =>
        println("No data found or 'Destination Country' column missing!")
    }

    println("")

    // Show the hotel that gives customers the best deal
    hotelReport.BestValueHotel match {
      case Some(h) =>
        println(s"The Most Economical Hotel is: ${h.name} (${h.city}, ${h.country})")
      case None =>
        println("No hotel data available.")
    }

    println("")

    // Show the most profitable hotel
    hotelReport.mostProfitableHotel match {
      case Some(h) =>
        println(s"Most Profitable Hotel (By Profibility Score): ${h.name} (${h.city}, ${h.country})")
        println(f"Total Profit: SGD ${h.totalProfit}%.2f")
        println(s"Total Visitors: ${h.totalVisitors}")
      case None =>
        println("Could not determine most profitable hotel.")
    }
  }
}