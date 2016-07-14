package org.openlmis.download;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.supercsv.io.CsvMapWriter;
import org.supercsv.io.ICsvMapWriter;
import org.supercsv.prefs.CsvPreference;

import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.List;
import java.util.Map;

public class CsvGenerator {

  Logger logger = LoggerFactory.getLogger(CsvGenerator.class);

  public CsvGenerator() {
  }

  /**
   * generates CSV String from given rows.
   *
   * @param rows - list of Map which values are Csv row values
   * @param chosenColumns - array of chosen columns names which defines
   *         what fields of Object will be written to CSV
   * @return String containing Object exported to CSV format
   */
  public String toCsv(List<Map<String, Object>> rows, String[] chosenColumns) {
    String csv = null;

    if (!rows.isEmpty()) {

      StringWriter writer = new StringWriter();

      ICsvMapWriter mapWriter = null;
      try {
        mapWriter = new CsvMapWriter(writer, CsvPreference.STANDARD_PREFERENCE);

        mapWriter.writeHeader(chosenColumns);

        for (Map<String, Object> row : rows) {
          mapWriter.write(row, chosenColumns);
        }
      } catch (IOException ex) {
        logger.debug(ex.getMessage(), ex);
      } finally {
        close(mapWriter);
        csv = writer.toString();
      }
    }

    return csv;
  }

  /**
   * generates CSV file to given output builded from given rows.
   *
   * @param rows - list of Map which values are Csv row values
   * @param chosenColumns - array of chosen columns names which defines
   *        what fields of Object will be written to CSV
   * @param output - csv will be writen on this output
   */
  public void toCsv(List<Map<String, Object>> rows, String[] chosenColumns, OutputStream output) {
    if (!rows.isEmpty()) {
      Writer writer = new BufferedWriter(new OutputStreamWriter(output));

      ICsvMapWriter mapWriter = null;
      try {
        mapWriter = new CsvMapWriter(writer, CsvPreference.STANDARD_PREFERENCE);

        mapWriter.writeHeader(chosenColumns);

        for (Map<String, Object> row : rows) {
          mapWriter.write(row, chosenColumns);
        }
      } catch (IOException ex) {
        logger.debug(ex.getMessage(), ex);
      } finally {
        close(mapWriter);
      }
    }
  }

  private void close(Closeable closeable) {
    if (closeable == null) {
      return;
    }

    try {
      closeable.close();
    } catch (IOException ex) {
      logger.debug(ex.getMessage(), ex);
    }
  }
}
