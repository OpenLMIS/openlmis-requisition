package org.openlmis.csv;

import static ch.qos.logback.core.util.CloseUtil.closeQuietly;

import lombok.NoArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.supercsv.io.CsvMapWriter;
import org.supercsv.io.ICsvMapWriter;
import org.supercsv.prefs.CsvPreference;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.List;
import java.util.Map;

@NoArgsConstructor
public class CsvGenerator {

  private static final Logger LOGGER = LoggerFactory.getLogger(CsvGenerator.class);

  /**
   * Generates CSV String from given rows.
   *
   * @param rows - list of Map which values are Csv row values
   * @param chosenColumns - array of chosen columns names which defines
   *        what fields of Object will be written to CSV
   * @return String containing Object exported to CSV format
   */
  public String toCsv(List<Map<String, Object>> rows, String[] chosenColumns) {
    StringWriter writer = new StringWriter();
    writeCsv(rows, chosenColumns, writer);
    return writer.toString();
  }

  /**
   * Generates CSV file to given output built from given rows.
   *
   * @param rows - list of Map which values are Csv row values
   * @param chosenColumns - array of chosen columns names which defines
   *        what fields of Object will be written to CSV
   * @param output - csv will be writen on this output
   */
  public void toCsv(List<Map<String, Object>> rows, String[] chosenColumns, OutputStream output) {
    Writer writer = new BufferedWriter(new OutputStreamWriter(output));
    writeCsv(rows, chosenColumns, writer);
  }

  private void writeCsv(List<Map<String, Object>> rows, String[] chosenColumns, Writer writer) {
    if (!rows.isEmpty()) {
      ICsvMapWriter mapWriter = null;
      try {
        mapWriter = new CsvMapWriter(writer, CsvPreference.STANDARD_PREFERENCE);
        mapWriter.writeHeader(chosenColumns);

        for (Map<String, Object> row : rows) {
          mapWriter.write(row, chosenColumns);
        }
      } catch (IOException ex) {
        LOGGER.debug(ex.getMessage(), ex);
      } finally {
        closeQuietly(mapWriter);
      }
    }
  }
}
