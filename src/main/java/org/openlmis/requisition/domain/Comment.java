package org.openlmis.requisition.domain;

import com.fasterxml.jackson.annotation.JsonView;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.hierarchyandsupervision.domain.User;
import org.openlmis.referencedata.domain.BaseEntity;
import org.openlmis.view.View;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Table(name = "comments", schema = "referencedata")
@NoArgsConstructor
public class Comment extends BaseEntity {

  @ManyToOne
  @JoinColumn(name = "requisitionId", nullable = false)
  @JsonView(View.BasicInformation.class)
  @Getter
  @Setter
  private Requisition requisition;

  @OneToOne
  @JoinColumn(name = "userId", nullable = false)
  @JsonView(View.BasicInformation.class)
  @Getter
  @Setter
  private User author;

  @JsonView(View.BasicInformation.class)
  @Getter
  @Setter
  private String body;

  @JsonView(View.BasicInformation.class)
  @Getter
  @Setter
  private LocalDateTime createdDate;

  @PrePersist
  private void prePersist() {
    this.createdDate = LocalDateTime.now();
  }
}