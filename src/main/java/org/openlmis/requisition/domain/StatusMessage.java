package org.openlmis.requisition.domain;

import org.hibernate.annotations.Type;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.UUID;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "status_messages")
@NoArgsConstructor
public class StatusMessage extends BaseTimestampedEntity {

  private static final String UUID = "pg-uuid";

  @ManyToOne(cascade = {CascadeType.REFRESH})
  @JoinColumn(name = "requisitionId", nullable = false)
  @Getter
  @Setter
  private Requisition requisition;

  @Getter
  @Setter
  @Type(type = UUID)
  private UUID authorId;

  @Getter
  @Setter
  private String authorFirstName;

  @Getter
  @Setter
  private String authorLastName;

  @Column(nullable = false)
  @Enumerated(EnumType.STRING)
  @Getter
  @Setter
  private RequisitionStatus status;

  @Column(nullable = false)
  @Getter
  @Setter
  private String body;

  private StatusMessage(Requisition requisition, UUID authorId, String authorFirstName,
                        String authorLastName, String body) {
    this.requisition = Objects.requireNonNull(requisition);
    this.authorId = authorId;
    this.authorFirstName = authorFirstName;
    this.authorLastName = authorLastName;
    this.status = Objects.requireNonNull(requisition.getStatus());
    this.body = Objects.requireNonNull(body);
  }
  
  public static StatusMessage newStatusMessage(Requisition requisition, UUID authorId,
                                               String authorFirstName, String authorLastName,
                                               String body) {
    return new StatusMessage(requisition, authorId, authorFirstName, authorLastName, body);
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Exporter exporter) {
    exporter.setId(id);
    exporter.setAuthorId(authorId);
    exporter.setAuthorFirstName(authorFirstName);
    exporter.setAuthorLastName(authorLastName);
    exporter.setRequisitionId(requisition.getId());
    exporter.setStatus(status);
    exporter.setBody(body);
    exporter.setCreatedDate(getCreatedDate());

  }

  public interface Exporter {
    void setId(UUID id);

    void setAuthorId(UUID authorId);

    void setAuthorFirstName(String authorFirstName);

    void setAuthorLastName(String authorLastName);

    void setRequisitionId(UUID requisitionId);

    void setBody(String body);
    
    void setStatus(RequisitionStatus status);

    void setCreatedDate(LocalDateTime createdDate);
  }
}
