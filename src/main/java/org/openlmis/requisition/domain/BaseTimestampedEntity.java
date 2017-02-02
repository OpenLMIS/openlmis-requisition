package org.openlmis.requisition.domain;

import com.fasterxml.jackson.annotation.JsonView;
import lombok.Getter;
import lombok.Setter;
import org.openlmis.util.View;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import java.time.ZonedDateTime;

@MappedSuperclass
public abstract class BaseTimestampedEntity extends BaseEntity {

  @Column(columnDefinition = "timestamp with time zone")
  @JsonView(View.BasicInformation.class)
  @Getter
  @Setter
  private ZonedDateTime createdDate;

  @Column(columnDefinition = "timestamp with time zone")
  @JsonView(View.BasicInformation.class)
  @Getter
  @Setter
  private ZonedDateTime modifiedDate;

  @PrePersist
  private void prePersist() {
    this.createdDate = ZonedDateTime.now();
  }

  @PreUpdate
  private void preUpdate() {
    this.modifiedDate = ZonedDateTime.now();
  }
}
