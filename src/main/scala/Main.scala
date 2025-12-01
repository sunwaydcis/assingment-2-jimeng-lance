import java.nio.charset.StandardCharsets
import scala.io.Source


trait CSVReader {

  // Reads a CSV file from the given file path and returns the contents
  // as a List of Array[String], where each Array represents a row split by commas.
  def readCSV(filePath: String): List[Array[String]] = {

    // Open the file using ISO-8859-1 encoding
    val file = Source.fromFile(filePath, StandardCharsets.ISO_8859_1.toString)

    // Read all lines from the file into a List[String]
    val lines = file.getLines().toList

    // Close the file to release system resources
    file.close()

    // Split each line by commas, trim whitespace, and convert to List[Array[String]]
    lines.map(_.split(",").map(_.trim))
  }
}


object Main extends CSVReader {

  def main(args: Array[String]): Unit = {
    // Read the CSV file and store the parsed rows in 'data'
    val data = readCSV("resources/Hotel_Dataset.csv")

    // Print how many rows were loaded from the CSV file
    println(s"Loaded ${data.length} rows.")

  }
  }

