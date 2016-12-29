package org.openlmis.requisition.domain;

import java.util.UUID;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "template_parameters")
@NoArgsConstructor
@AllArgsConstructor
public class TemplateParameter extends BaseEntity {

  @ManyToOne(cascade = CascadeType.REFRESH)
  @JoinColumn(name = "templateId", nullable = false)
  @Getter
  @Setter
  private Template template;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  @Getter
  @Setter
  private String name;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  @Getter
  @Setter
  private String displayName;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  @Getter
  @Setter
  private String defaultValue;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  @Getter
  @Setter
  private String dataType;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  @Getter
  @Setter
  private String selectSql;

  @Column(columnDefinition = TEXT_COLUMN_DEFINITION)
  @Getter
  @Setter
  private String description;

  /**
   * Create new instance of TemplateParameter based on given {@link TemplateParameter.Importer}
   * @param importer instance of {@link TemplateParameter.Importer}
   * @return instance of TemplateParameter.
   */
  public static TemplateParameter newInstance(Importer importer) {
    TemplateParameter templateParameter = new TemplateParameter();
    templateParameter.setId(importer.getId());
    templateParameter.setName(importer.getName());
    templateParameter.setDisplayName(importer.getDisplayName());
    templateParameter.setDefaultValue(importer.getDefaultValue());
    templateParameter.setSelectSql(importer.getSelectSql());
    templateParameter.setDescription(importer.getDescription());
    templateParameter.setDataType(importer.getDataType());
    return templateParameter;
  }

  /**
   * Export this object to the specified exporter (DTO).
   *
   * @param exporter exporter to export to
   */
  public void export(Exporter exporter) {
    exporter.setId(id);
    exporter.setName(name);
    exporter.setDescription(description);
    exporter.setDataType(dataType);
    exporter.setDefaultValue(defaultValue);
    exporter.setDisplayName(displayName);
    exporter.setSelectSql(selectSql);
  }

  public interface Exporter {
    void setId(UUID id);

    void setName(String name);

    void setDisplayName(String displayName);

    void setDefaultValue(String defaultValue);

    void setDataType(String dataType);

    void setSelectSql(String selectSql);

    void setDescription(String description);

  }

  public interface Importer {
    UUID getId();

    String getName();

    String getDisplayName();

    String getDefaultValue();

    String getDataType();

    String getSelectSql();

    String getDescription();

  }
}
