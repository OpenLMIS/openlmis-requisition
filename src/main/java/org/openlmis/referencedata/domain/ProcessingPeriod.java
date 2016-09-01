package org.openlmis.referencedata.domain;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.referencedata.utils.LocalDatePersistenceConverter;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.time.LocalDate;

@Entity
@Table(name = "periods", schema = "referencedata")
@NoArgsConstructor
public class ProcessingPeriod extends BaseEntity {

  @ManyToOne
  @JoinColumn(name = "processingScheduleId", nullable = false)
  @Getter
  @Setter
  private ProcessingSchedule processingSchedule;

  @Column(nullable = false, columnDefinition = "text")
  @Getter
  @Setter
  private String name;

  @Column(nullable = true, columnDefinition = "text")
  @Getter
  @Setter
  private String description;

  @JsonSerialize(using = LocalDateSerializer.class)
  @JsonDeserialize(using = LocalDateDeserializer.class)
  @Convert(converter = LocalDatePersistenceConverter.class)
  @Column(nullable = false)
  @Getter
  @Setter
  private LocalDate startDate;

  @JsonSerialize(using = LocalDateSerializer.class)
  @JsonDeserialize(using = LocalDateDeserializer.class)
  @Convert(converter = LocalDatePersistenceConverter.class)
  @Column(nullable = false)
  @Getter
  @Setter
  private LocalDate endDate;

  /**
   * Copy values of attributes into new or updated ProcessingPeriod.
   *
   * @param processingPeriod ProcessingPeriod with new values.
   */
  public void updateFrom(ProcessingPeriod processingPeriod) {
    this.processingSchedule = processingPeriod.getProcessingSchedule();
    this.name = processingPeriod.getName();
    this.description = processingPeriod.getDescription();
    this.startDate = processingPeriod.getStartDate();
    this.endDate = processingPeriod.getEndDate();
  }

}