import java.nio.charset.StandardCharsets
import scala.io.Source


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

    // Check for duplicates
    val uniqueRowsSet = dataRows.map(_.mkString(",")).toSet
    if (uniqueRowsSet.size < dataRows.size) {
      println(s"Notice: ${dataRows.size - uniqueRowsSet.size} duplicate row(s) found and removed.")
    }

    // Remove exact duplicate rows
    val uniqueDataRows = uniqueRowsSet.map(_.split(",")).toList

    // Return header + unique rows
    header +: uniqueDataRows
  }
}

// Utility methods for analyzing lists
trait AnalysisUtils {

  // Counts how many times each element appears in a list
  def countOccurrences[T](list: List[T]): Map[T, Int] =
    list.groupBy(identity)    // Group identical elements together
      .view.mapValues(_.size) // Count how many items are in each group
      .toMap                 // Convert to a normal Map

  // Finds the element that appears the most
  def maxByCount[T](counts: Map[T, Int]): (T, Int) =
    counts.maxBy(_._2)        // Return the element with the highest count
}


// Class to generate a report about favorite countries
// filePath: Path to the CSV file
class FavoriteCountryReport(filePath: String)
  extends CSVReader with AnalysisUtils { // Assumes CSVReader can read CSV and AnalysisUtils has helper methods

  private val csvData: List[Array[String]] = readCSV(filePath)      // Read the CSV file into a list of rows

  private def columnIndex(name: String): Option[Int] = {
    csvData.headOption.flatMap { header =>
      val idx = header.indexOf(name)
      if (idx >= 0) Some(idx) else None
    }
  }

  private def getColumn(name: String): List[String] = {
    columnIndex(name) match {
      case Some(idx) =>
        csvData.tail
          .filter(row => row.length > idx && row(idx).nonEmpty)
          .map(row => row(idx))
      case None => List.empty
    }
  }

  // Returns the favorite country (most frequent "Destination Country") and its count
  // Returns None if no countries are present
  def favoriteCountry: Option[(String, Int)] = {
    val countries = getColumn("Destination Country") // Extract the "Destination Country" column
    if (countries.nonEmpty)
      Some(maxByCount(countOccurrences(countries))) // Count occurrences and find the most frequent
    else None
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    // Read the CSV file
    val filePath = "resources/Hotel_Dataset.csv"
    val report = new FavoriteCountryReport(filePath)

    // Print the country name and the total amount
    report.favoriteCountry match {
      case Some((country, count)) =>
        println(s"Country with the highest number of bookings: $country")
        println(s"Number of bookings (unique rows only): $count")
      case None =>
        println("No data found or 'Destination Country' column missing!")

    }
  }
}