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


object Main extends CSVReader {

  def main(args: Array[String]): Unit = {
    // Read the CSV file
    val data = readCSV("resources/Hotel_Dataset.csv")

    // Print how many rows were loaded from the CSV file after removing duplicate
    println(s"Loaded ${data.length} rows after removing duplicates.")

  }
}
