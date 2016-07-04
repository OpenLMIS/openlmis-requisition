package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.MapKeyColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "requisition_templates")
@NoArgsConstructor
public class RequisitionTemplate extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "programid", nullable = false)
  @Getter
  @Setter
  private Program program;

  @ElementCollection
  @MapKeyColumn(name = "key")
  @Column(name = "value")
  // second String is temporary, needs to be replaced with RequisitionTemplateColumn
  private Map<String,String> columnsMap = new HashMap<>();

  /** Allows creating RequisitionTemplate with pre existing columns. */
  public RequisitionTemplate(List<? extends String> columns) {
    if (columns != null) {
      for (String column : columns) {
        columnsMap.put(column, column);
      }
    }
  }

}
