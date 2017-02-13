package org.openlmis.requisition.domain;

import java.time.ZonedDateTime;
import java.util.UUID;

/**
 * AuditLogEntry is used to encapsulate data within our audit-log's entries.
 */
public class AuditLogEntry {
  private UUID authorId;
  private ZonedDateTime changeDate;

  public AuditLogEntry(){}

  public AuditLogEntry(UUID authorId, ZonedDateTime changeDate) {
    this.authorId = authorId;
    this.changeDate = changeDate;
  }

  public UUID getAuthorId() {
    return authorId;
  }

  public void setAuthorId(UUID authorId) {
    this.authorId = authorId;
  }

  public ZonedDateTime getChangeDate() {
    return changeDate;
  }

  public void setChangeDate(ZonedDateTime changeDate) {
    this.changeDate = changeDate;
  }
}
