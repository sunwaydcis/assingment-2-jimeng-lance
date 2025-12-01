import java.nio.charset.StandardCharsets
import scala.io.Source


trait CSVReader {


  // Reads a CSV file and returns the contents as a List[Array[String]].
  // Duplicate rows (after the header) are removed while preserving order.

  def readCSV(filePath: String): List[Array[String]] = {

    // Open the file using ISO-8859-1 encoding
    val file = Source.fromFile(filePath, StandardCharsets.ISO_8859_1.toString)

    // Read all lines from the file into a List[String]
    val lines = file.getLines().toList

    // Close the file to release system resources
    file.close()

    if (lines.isEmpty) {
      println("Error: CSV file is empty.")
      return List.empty
    }

    // Header row (keep as-is)
    val header = lines.head.split(",").map(_.trim)

    // Data rows (remove duplicates, keep first occurrence)
    val dataRows = lines.tail.map(_.split(",").map(_.trim))

    // Remove duplicates while preserving order
    val uniqueDataRows =
      dataRows.foldLeft((Set[String](), List[Array[String]]())) {
        case ((seen, acc), row) =>
          val rowKey = row.mkString(",") // convert to comparable string
          if (seen.contains(rowKey)) {
            (seen, acc) // duplicate → skip
          } else {
            (seen + rowKey, acc :+ row) // new row → keep
          }
      }._2

    // Print how many duplicates were removed
    val duplicates = dataRows.length - uniqueDataRows.length
    if (duplicates > 0)
      println(s"Removed $duplicates duplicate row(s).")

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

// Class to extract a specific column from CSV data
// csvData: List of rows, each row is an Array of Strings
class ColumnExtractor(csvData: List[Array[String]]) {

  // Returns the index of the column with the given name, if it exists
  private def columnIndex(name: String): Option[Int] = {
    val header = csvData.head           // The first row is assumed to be the header
    val idx = header.indexOf(name)      // Find the index of the column by name
    if (idx >= 0) Some(idx) else None   // Return Some(index) if found, None otherwise
  }

  // Returns all values from a column as a List[String]
  // If the column doesn't exist, returns an empty list
  def getColumn(name: String): List[String] =
    columnIndex(name)                     // Get the column index
      .map(i => csvData.tail.map(row => row(i))) // Map over all rows except the header
      .getOrElse(List.empty)              // Return empty list if column not found
}

// Class to generate a report about favorite countries
// filePath: Path to the CSV file
class FavoriteCountryReport(filePath: String)
  extends CSVReader with AnalysisUtils { // Assumes CSVReader can read CSV and AnalysisUtils has helper methods

  private val data = readCSV(filePath)       // Read the CSV file into a list of rows
  private val extractor = new ColumnExtractor(data) // Create a ColumnExtractor for easy column access

  // Returns the favorite country (most frequent "Destination Country") and its count
  // Returns None if no countries are present
  def favoriteCountry: Option[(String, Int)] = {
    val countries = extractor.getColumn("Destination Country") // Extract the "Destination Country" column
    if (countries.nonEmpty)
      Some(maxByCount(countOccurrences(countries))) // Count occurrences and find the most frequent
    else None
  }
}

object Main{

  def main(args: Array[String]): Unit = {
    // Read the CSV file
    val report = new FavoriteCountryReport("resources/Hotel_Dataset.csv")

    // Print the country name and the total amount
    println(report.favoriteCountry)

  }
}
