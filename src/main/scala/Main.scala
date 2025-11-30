import java.nio.charset.StandardCharsets
import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    // Path to your CSV file
    val filePath = "resources/Hotel_Dataset.csv"

    // Open the CSV file with ISO_8859_1 encoding
    val bufferedSource = Source.fromFile(filePath, StandardCharsets.ISO_8859_1.toString)

    // Read all lines into a List[String]
    val lines = bufferedSource.getLines().toList

    // Close the file to free resources
    bufferedSource.close()

    // Check if the CSV is empty
    if (lines.isEmpty) {
      println("Error: The dataset is empty.")
    } else {
      // Split the first line to get the header (column names)
      val header = lines.head.split(",").map(_.trim)

      // Print the header to verify the columns
      println("Columns in CSV:")
      header.foreach { column =>
        println(s"- $column") // Each column printed with a dash for clarity
      }


      }
    }
  }

