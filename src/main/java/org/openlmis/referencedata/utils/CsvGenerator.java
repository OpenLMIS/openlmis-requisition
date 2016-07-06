package org.openlmis.referencedata.utils;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.openlmis.referencedata.exception.CsvInputNotValidException;

import java.io.IOException;
import java.util.List;

public class CsvGenerator {

  /**
   * Prepare headlines of CSV.
   *
   * @param headlines List of column names in CSV file
   * @return result String with headlines formatted for CSV file
   */
  public String appendHeadlines(final List<String> headlines) throws CsvInputNotValidException {
    StringBuilder result = new StringBuilder();

    try {
      CSVPrinter printer = new CSVPrinter(result, CSVFormat.DEFAULT);
      printer.printRecord(headlines);
    } catch (IOException exception) {
      throw new CsvInputNotValidException("Invalid input for CSV generator", exception);
    }

    return result.toString();
  }

  /**
   * Append record to CSV string.
   *
   * @param recordContent List of fields to write in CSV record
   * @param csv String with earlier generated CSV records and headlines
   * @return result String with headlines and records formatted for CSV file
   */
  public String appendRecord(
      final List<String> recordContent, String csv) throws CsvInputNotValidException {
    StringBuilder result = new StringBuilder(csv);

    try {
      CSVPrinter printer = new CSVPrinter(result, CSVFormat.DEFAULT);
      printer.printRecord(recordContent);
    } catch (IOException exception) {
      throw new CsvInputNotValidException("Invalid input for CSV generator", exception);
    }

    return result.toString();
  }
}
