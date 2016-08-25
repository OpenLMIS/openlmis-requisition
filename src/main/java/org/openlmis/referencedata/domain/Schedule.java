package org.openlmis.referencedata.domain;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.fulfillment.utils.LocalDateTimePersistenceConverter;

import java.time.LocalDateTime;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;


@Entity
@Table(name = "schedules", schema = "referencedata")
@NoArgsConstructor
public class Schedule extends BaseEntity {

  @Column(nullable = false, unique = true, columnDefinition = "text")
  @Getter
  @Setter
  private String code;

  @Column(columnDefinition = "text")
  @Getter
  @Setter
  private String description;

  @JsonSerialize(using = LocalDateTimeSerializer.class)
  @JsonDeserialize(using = LocalDateTimeDeserializer.class)
  @Convert(converter = LocalDateTimePersistenceConverter.class)
  @Getter
  @Setter
  private LocalDateTime modifiedDate;

  @Column(nullable = false, unique = true, columnDefinition = "text")
  @Getter
  @Setter
  private String name;

  @PrePersist
  @PreUpdate
  private void setModifiedDate() {
    this.modifiedDate = LocalDateTime.now();
  }

}
