package org.openlmis.referencedata.utils;

import lombok.Getter;
import lombok.Setter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/*import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;*/

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/*import java.time.format.DateTimeFormatter;
import java.util.Locale;*/

public class Version {

  public static final String VERSION = "build/resources/main/version";
  public static final String VERSION_TEMPLATE = "src/main/resources/version";

  @Getter
  @Setter
  private String service;

  @Getter
  @Setter
  private String build;

  @Getter
  @Setter
  private String branch;

  @Getter
  @Setter
  private String timeStamp;

  @Getter
  @Setter
  private String version;

  Logger logger = LoggerFactory.getLogger(Version.class);

  /**
   * Allow displaying build information.
   */
  public Version() {
    File file = new File(VERSION);
    if (!file.exists()) {
      file = new File(VERSION_TEMPLATE);
    }
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new FileReader(file));
      this.service = getValueFromLine(reader.readLine());
      this.build = getValueFromLine(reader.readLine());
      this.branch = getValueFromLine(reader.readLine());
      this.timeStamp = getValueFromLine(reader.readLine());
      this.version = getValueFromLine(reader.readLine());
      reader.close();
    } catch (FileNotFoundException ex) {
      logger.error("Error opening non-existing file");
    } catch (IOException ex) {
      logger.error("Error reading line from file");
    }
  }

  private String getValueFromLine(String line) {
    return line.substring(line.indexOf(' ') + 1);
  }
}
