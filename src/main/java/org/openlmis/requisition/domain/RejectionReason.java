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

import java.util.Objects;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.javers.core.metamodel.annotation.TypeName;

@Entity
@Table(name = "rejection_reasons")
@NoArgsConstructor
@TypeName("RejectionReason")
@SuppressWarnings({"PMD.UnusedPrivateField"})
public class RejectionReason extends BaseEntity {
  private static final String TEXT = "text";

  @Column(nullable = false, unique = true, columnDefinition = TEXT)
  @Getter
  private String name;

  @Column(nullable = false, unique = true, columnDefinition = TEXT)
  @Getter
  @Setter
  private String code;

  @ManyToOne
  @JoinColumn(name = "rejectionreasoncategoryid", nullable = false)
  @Getter
  @Setter
  private RejectionReasonCategory rejectionReasonCategory;

  @Getter
  @Setter
  private Boolean active;

  private RejectionReason(String name, String code,
                          RejectionReasonCategory rejectionReasonCategory,
                          Boolean active) {
    this.name = name;
    this.code = code;
    this.rejectionReasonCategory = rejectionReasonCategory;
    this.active = active;
  }

  /**
   * Static factory method for constructing a new right with a name and type.
   * @param rejectionReason rejection reason  category
   */
  public void updateFrom(RejectionReason rejectionReason) {
    this.name = rejectionReason.getName();
    this.code = rejectionReason.getCode();
    this.rejectionReasonCategory = rejectionReason.getRejectionReasonCategory();
    this.active = rejectionReason.getActive();
  }

  /**
   * Static factory method for constructing a new
   * right with a name and type.
   *
   * @param name                    rejection reason name
   * @param code                    rejection reason  code
   * @param rejectionReasonCategory rejection
   *                                reason  category
   */
  public static RejectionReason newRejectionReason(
          String name, String code,
          RejectionReasonCategory rejectionReasonCategory, Boolean active) {
    return new RejectionReason(name, code, rejectionReasonCategory, active);
  }

  /**
   * Static factory method for constructing a new rejection reason using an importer (DTO).
   *
   * @param importer the rejection reason importer (DTO)
   */
  public static RejectionReason newRejectionReason(RejectionReason.Importer importer) {
    RejectionReason newRejectionReason = new RejectionReason(importer.getName(),
            importer.getCode(), importer.getRejectionReasonCategory(),
            importer.getActive());
    newRejectionReason.id = importer.getId();
    newRejectionReason.code = importer.getCode();
    newRejectionReason.rejectionReasonCategory = importer.getRejectionReasonCategory();
    newRejectionReason.active = importer.getActive();
    return newRejectionReason;
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Exporter exporter) {
    exporter.setId(id);
    exporter.setName(name);
    exporter.setCode(code);
    exporter.setRejectionReasonCategory(rejectionReasonCategory);
    exporter.setActive(active);
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof RejectionReason)) {
      return false;
    }
    RejectionReason rejectionReason = (RejectionReason) obj;
    return Objects.equals(name, rejectionReason.name);
  }

  public interface Exporter {
    void setId(UUID id);

    void setName(String name);

    void setCode(String code);

    void setRejectionReasonCategory(RejectionReasonCategory rejectionReasonCategory);

    void setActive(Boolean active);
  }

  public interface Importer {
    UUID getId();

    String getName();

    String getCode();

    RejectionReasonCategory getRejectionReasonCategory();

    Boolean getActive();
  }
}