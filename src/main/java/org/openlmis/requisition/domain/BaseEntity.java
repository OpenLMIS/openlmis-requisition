package org.openlmis.requisition.domain;

import com.fasterxml.jackson.annotation.JsonView;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Type;
import org.openlmis.view.View;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;

@MappedSuperclass
public abstract class BaseEntity {
  static final String TEXT_COLUMN_DEFINITION = "text";
  
  @Id
  @GeneratedValue(generator = "uuid-gen")
  @GenericGenerator(name = "uuid-gen", strategy = "uuid2")
  @JsonView(View.BasicInformation.class)
  @Type(type = "pg-uuid")
  @Getter
  @Setter
  protected UUID id;
}
