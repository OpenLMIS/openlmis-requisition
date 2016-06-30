package org.openlmis.referencedata.domain;

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

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "requisitionTemplates")
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

  public RequisitionTemplate(List<? extends String> columns) {
    if (columns != null) {
      for (String column : columns) {
        columnsMap.put(column, column);
      }
    }
  }

}
