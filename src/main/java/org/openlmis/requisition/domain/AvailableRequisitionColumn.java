package org.openlmis.requisition.domain;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import lombok.Getter;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.Table;

@Entity
@Table(name = "available_requisition_columns", schema = "requisition")
@Getter
@Setter
public class AvailableRequisitionColumn extends BaseEntity {

  private String name;

  @ElementCollection(fetch = FetchType.EAGER, targetClass = SourceType.class)
  @Enumerated(EnumType.STRING)
  @Column(name = "value")
  @CollectionTable(
      name = "available_requisition_column_sources",
      joinColumns = @JoinColumn(name = "columnId")
  )
  private Set<SourceType> sources;

  @OneToMany(
      mappedBy = "requisitionColumn",
      cascade = {CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.REMOVE},
      fetch = FetchType.EAGER)
  private Set<AvailableRequisitionColumnOption> options;

  private String label;

  private String indicator;

  private Boolean mandatory;

  private Boolean isDisplayRequired;

  private Boolean canChangeOrder;

  private Boolean canBeChangedByUser;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  private String definition;

  @Column(nullable = false)
  @Enumerated(EnumType.STRING)
  private ColumnType columnType;

  public AvailableRequisitionColumn() {
    this.sources = new HashSet<>();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }

    if (!(obj instanceof AvailableRequisitionColumn)) {
      return false;
    }

    AvailableRequisitionColumn that = (AvailableRequisitionColumn) obj;

    return new EqualsBuilder()
        .append(name, that.name)
        .append(sources, that.sources)
        .append(options, that.options)
        .append(label, that.label)
        .append(indicator, that.indicator)
        .append(mandatory, that.mandatory)
        .append(isDisplayRequired, that.isDisplayRequired)
        .append(canChangeOrder, that.canChangeOrder)
        .append(canBeChangedByUser, that.canBeChangedByUser)
        .append(definition, that.definition)
        .append(columnType, that.columnType)
        .isEquals();
  }

  @Override
  public int hashCode() {
    return new HashCodeBuilder(17, 37)
        .append(name)
        .append(sources)
        .append(options)
        .append(label)
        .append(indicator)
        .append(mandatory)
        .append(isDisplayRequired)
        .append(canChangeOrder)
        .append(canBeChangedByUser)
        .append(definition)
        .append(columnType)
        .toHashCode();
  }
}
