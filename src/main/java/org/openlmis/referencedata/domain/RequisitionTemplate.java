package org.openlmis.referencedata.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.util.*;

@Entity
@Table(name = "requisitionTemplates")
@NoArgsConstructor
public class RequisitionTemplate extends BaseEntity {

  @OneToOne(cascade = CascadeType.MERGE)
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
    columnsMap = new HashMap<>();
    if(columns != null) {
      for (String column : columns) {
        columnsMap.put(column, column);
      }
    }
  }

}
