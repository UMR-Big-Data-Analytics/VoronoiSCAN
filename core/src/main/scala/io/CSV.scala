package io

import com.opencsv.{CSVParserBuilder, CSVReaderBuilder}
import com.opencsv.enums.CSVReaderNullFieldIndicator
import data.Point
import data.Point.Embedding

import java.nio.file.{Files, Path}
import scala.util.Using

class CSVWriter(path: String) extends DataWriter {

  override def write(data: Array[Embedding], labels: Array[Int]): Unit = {
    require(data.length == labels.length, "Data and labels must have the same length")
    require(data.nonEmpty, "Data must not be empty")
    val dimensions = data.head.length
    Using(new java.io.PrintWriter(new java.io.File(path))) { writer =>
      writer.write((0 until dimensions).map(i => s"dim$i").mkString(",") + ",label\n")
      for (i <- data.indices)
        writer.write(s"${data(i).mkString(",")}, ${labels(i)}\n")
    }
  }

  def write(data: Array[Point], labels: Array[Int]): Unit = {
    val vectors = data.map(_.vector)
    write(vectors, labels)
  }

  def write(data: Array[Embedding]): Unit = {
    require(data.nonEmpty, "Data must not be empty")
    val dimensions = data.head.length
    Using(new java.io.PrintWriter(new java.io.File(path))) { writer =>
      writer.write((0 until dimensions).map(i => s"dim$i").mkString(",") + "\n")
      for (i <- data.indices)
        writer.write(s"${data(i).mkString(",")}\n")
    }
  }

}

class CSVReader(path: String) extends DataReader {

  override def read: Array[Embedding] =
    Using(scala.io.Source.fromFile(path)) { source =>
      val vectors = source
        .getLines()
        .drop(1)
        .map { line =>
          val values = line.split(",").map(_.toFloat)
          values
        }
        .toArray
      vectors
    }.get

}

class OpenCSVReader(path: String) extends DataReader {

  override def read: Array[Embedding] =
    Using(createCSVReader(path)) { csvReader =>
      val header = csvReader.readNext() // Read header
      if (header == null || header.isEmpty) {
        throw new IllegalArgumentException(s"CSV file at $path is empty or has no header.")
      }
      val vectors = Iterator
        .continually(csvReader.readNext())
        .takeWhile(_ != null)
        .map { row =>
          val values = row.map(_.toFloat)
          values
        }
        .toArray
      vectors
    }.getOrElse(Array.empty[Embedding])

  private def createCSVReader(inputPath: String) = {
    val parser = new CSVParserBuilder()
      .withSeparator(',')
      .withQuoteChar('"')
      .withEscapeChar('\\')
      .withStrictQuotes(false)
      .withIgnoreLeadingWhiteSpace(false)
      .withFieldAsNull(CSVReaderNullFieldIndicator.EMPTY_SEPARATORS)
      .build
    val buffer = Files.newBufferedReader(Path.of(inputPath))
    new CSVReaderBuilder(buffer).withCSVParser(parser).build
  }

}
