package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.OneToOne;
import javax.persistence.JoinColumn;
import javax.persistence.ElementCollection;
import javax.persistence.MapKeyColumn;
import javax.persistence.Column;
import java.util.Map;
import java.util.HashMap;
import java.util.List;

@Entity
@Table(name = "requisitionTemplates")
@NoArgsConstructor
public class RequisitionTemplate extends BaseEntity {

  @OneToOne
  @JoinColumn(name = "programid", nullable=false)
  @Getter
  @Setter
  private Program program;

  @ElementCollection
  @MapKeyColumn(name = "key")
  @Column(name = "value")
  // second String is temporary, needs to be replaced with RequisitionTemplateColumn
  private Map<String,String> columnsMap = new HashMap<>();

  public RequisitionTemplate(List<? extends String> columns) {
    if(columns != null) {
      for (String column : columns) {
        columnsMap.put(column, column);
      }
    }
  }

}
