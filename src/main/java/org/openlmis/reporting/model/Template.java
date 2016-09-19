package org.openlmis.reporting.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.domain.BaseEntity;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.List;

@Entity
@Table(name = "templates")
@NoArgsConstructor
@AllArgsConstructor
public class Template extends BaseEntity {

  private static final String TEXT = "text";

  @Column(columnDefinition = TEXT, unique = true, nullable = false)
  @Getter
  @Setter
  private String name;

  @Column
  @Getter
  @Setter
  private byte[] data;

  @OneToMany(mappedBy = "template", cascade = CascadeType.REMOVE)
  @Getter
  @Setter
  private List<TemplateParameter> templateParameters;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String type;

  @Column(columnDefinition = TEXT)
  @Getter
  @Setter
  private String description;

  /**
   * Copy values of attributes into new or updated Template.
   *
   * @param template Template with new values.
   */
  public void updateFrom(Template template) {
    this.name = template.getName();
    this.data = template.getData();
    this.templateParameters = template.getTemplateParameters();
    this.type = template.getType();
    this.description = template.getDescription();
  }
}
