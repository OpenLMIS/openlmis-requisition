package org.openlmis.reporting.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.domain.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "template_parameters")
@NoArgsConstructor
@AllArgsConstructor
public class TemplateParameter extends BaseEntity {

  private static final String TEXT = "text";

  @ManyToOne
  @JoinColumn(name = "templateId", nullable = false)
  @Getter
  @Setter
  private Template template;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String name;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String displayName;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String defaultValue;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String dataType;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String selectSql;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String description;
}
