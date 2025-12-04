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

    val bufferedSource = Source.fromFile(filePath, StandardCharsets.ISO_8859_1.toString)
    val lines = bufferedSource.getLines().toList
    bufferedSource.close()

    if (lines.isEmpty) {
      println("Error: CSV file is empty.")
      return List.empty
    }

    // Header row (keep as-is)
    val header = lines.head.split(",").map(_.trim)

    // Data rows (remove duplicates, keep first occurrence)
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

    // Return header + unique rows
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
                         avgPrice: Double,
                         avgDiscount: Double,
                         avgProfitMargin: Double,
                         totalVisitors: Int,
                         bookings: Int,
                         scorePrice: Double = 0.0,
                         scoreDiscount: Double = 0.0,
                         scoreMargin: Double = 0.0,
                         totalScore: Double = 0.0
                       )

  // Method to find best value hotel for the customer
  def BestValueHotel: Option[HotelStats] = {

    // Extract columns that is needed for the analysis
    val hotelIdx = columnIndex(csvData, "Hotel Name")
    val priceIdx = columnIndex(csvData, "Booking Price[SGD]")
    val discIdx = columnIndex(csvData, "Discount")
    val marginIdx = columnIndex(csvData, "Profit Margin")
    val peopleIdx = columnIndex(csvData, "No. Of People")

    // if cant fetch any data it will show this message and end the method
    if (List(hotelIdx, priceIdx, discIdx, marginIdx, peopleIdx).contains(None)) {
      println("Warning: Missing required columns for BestValueHotel analysis.")
      return None
    }

    // Go through every row and collect the 5 values we need
    val stats = csvData.tail
      .map { row =>
        (
          row(hotelIdx.get),
          safeDouble(row(priceIdx.get)),
          safePercent(row(discIdx.get)),
          safeDouble(row(marginIdx.get)),
          safeInt(row(peopleIdx.get))
        )
      }
      .groupBy(_._1) // group the booking records by hotel name, then calculate averages and totals for each hotel
      .map { case (name, rows) =>
        val n = rows.size
        HotelStats(
          name = name,
          avgPrice = rows.map(_._2).sum / n,
          avgDiscount = rows.map(_._3).sum / n,
          avgProfitMargin = rows.map(_._4).sum / n,
          totalVisitors = rows.map(_._5).sum,
          bookings = n
        )
      }
      .toList

    // If there is no data found it will return nothing
    if (stats.isEmpty) None
    else {

      // Find min and max values for all averages
      val minPrice = stats.minBy(_.avgPrice).avgPrice
      val maxPrice = stats.maxBy(_.avgPrice).avgPrice
      val minDiscount = stats.minBy(_.avgDiscount).avgDiscount
      val maxDiscount = stats.maxBy(_.avgDiscount).avgDiscount
      val minMargin = stats.minBy(_.avgProfitMargin).avgProfitMargin
      val maxMargin = stats.maxBy(_.avgProfitMargin).avgProfitMargin

      // Helper function to check for division by zero
      def safeDivide(numerator: Double, denominator: Double): Double =
        if (denominator == 0.0) 0.0 else numerator / denominator

      // Normalize and calculate the total economic score of each hotel
      val scoredStats = stats.map { h =>

        // Price Score (Lower is better)
        val scorePrice = safeDivide(h.avgPrice - minPrice, maxPrice - minPrice)

        // Discount Score (Higher is better)
        val scoreDiscount = safeDivide(maxDiscount - h.avgDiscount, maxDiscount - minDiscount)

        // Profit Margin Score (Lower is better)
        val scoreMargin = safeDivide(h.avgProfitMargin - minMargin, maxMargin - minMargin)

        // Calculate total score
        val totalScore = scorePrice + scoreDiscount + scoreMargin

        h.copy(
          scorePrice = scorePrice,
          scoreDiscount = scoreDiscount,
          scoreMargin = scoreMargin,
          totalScore = totalScore
        )
      }

      // Return the hotel with the lowest score which is the most economical one
      Some(scoredStats.minBy(_.totalScore))
    }
  }

  // Method to find the most profitable hotel
  def mostProfitableHotel: Option[HotelStats] = {

    // Extract columns that is needed for the analysis
    val hotelIdx = columnIndex(csvData, "Hotel Name")
    val priceIdx = columnIndex(csvData, "Booking Price[SGD]")
    val discIdx = columnIndex(csvData, "Discount")
    val marginIdx = columnIndex(csvData, "Profit Margin")
    val peopleIdx = columnIndex(csvData, "No. Of People")

    // if cant fetch any data it will show this message and end the method
    if (List(hotelIdx, priceIdx, marginIdx).contains(None)) {
      println("Warning: Missing required columns for mostProfitableHotel analysis.")
      return None
    }

    // Calculate Total Profit (Sum of Price * Margin) for each hotel
    val hotelProfits = csvData.tail
      .map { row =>
        (
          row(hotelIdx.get),
          safeDouble(row(priceIdx.get)),
          safeDouble(row(marginIdx.get))
        )
      }
      .groupBy(_._1) // group them by hotel
      .map { case (name, rows) =>
        val totalProfit = rows.map { case (_, price, margin) => price * margin }.sum
        (name, totalProfit)
      }

    // Find the winner by Total Profit
    val winnerName = hotelProfits.maxBy(_._2)._1

    // Get all records for the winning hotel
    val winnerRows = csvData.tail.filter(row => row(hotelIdx.get) == winnerName)
    val n = winnerRows.size

    if (n == 0) None
    else {
      // Recalculate full stats for the winner to return HotelStats object
      Some(HotelStats(
        name = winnerName,
        avgPrice = winnerRows.map(r => safeDouble(r(priceIdx.get))).sum / n,
        avgDiscount = winnerRows.map(r => safePercent(r(discIdx.get))).sum / n,
        avgProfitMargin = winnerRows.map(r => safeDouble(r(marginIdx.get))).sum / n,
        totalVisitors = winnerRows.map(r => safeInt(r(peopleIdx.get))).sum,
        bookings = n
      ))
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
        println(s"The Most Economical Hotel is: ${h.name}")
      case None =>
        println("No hotel data available.")
    }

    println("")

    // Show the most profitable hotel
    hotelReport.mostProfitableHotel match {
      case Some(h) =>
        println(s"Most Profitable Hotel: ${h.name}")
      case None =>
        println("Could not determine most profitable hotel.")
    }
  }
}