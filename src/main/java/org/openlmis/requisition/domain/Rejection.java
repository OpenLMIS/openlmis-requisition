/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org.
 */

package org.openlmis.requisition.domain;

import java.time.ZonedDateTime;

import java.util.UUID;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.domain.requisition.StatusChange;

@Entity
@Table(name = "rejections")
@NoArgsConstructor
public class Rejection extends BaseTimestampedEntity {

  @ManyToOne(cascade = {CascadeType.REFRESH})
  @JoinColumn(name = "rejectionReasonId", nullable = false)
  @Getter
  @Setter
  private RejectionReason rejectionReason;

  @ManyToOne(cascade = {CascadeType.ALL})
  @JoinColumn(name = "statusChangeId", nullable = false)
  @Setter
  @Getter
  private StatusChange statusChange;

  private Rejection(RejectionReason rejectionReason, StatusChange statusChange) {
    this.statusChange = statusChange;
    this.rejectionReason = rejectionReason;
  }

  public static Rejection newRejection(RejectionReason rejectionReason,
                                        StatusChange statusChange) {
    return new Rejection(rejectionReason, statusChange);
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Rejection.Exporter exporter) {
    exporter.setCreatedDate(getCreatedDate());
    exporter.setRejectionReason(rejectionReason);
    exporter.setId(id);
  }

  public interface Exporter {
    void setCreatedDate(ZonedDateTime createdDate);

    void setRejectionReason(RejectionReason rejectionReason);

    void setId(UUID id);
  }
}