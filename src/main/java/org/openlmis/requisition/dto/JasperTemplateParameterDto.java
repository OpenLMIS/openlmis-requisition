package org.openlmis.requisition.dto;


import org.openlmis.requisition.domain.JasperTemplateParameter;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class JasperTemplateParameterDto implements JasperTemplateParameter.Importer,
    JasperTemplateParameter.Exporter {

  private UUID id;
  private String name;
  private String displayName;
  private String defaultValue;
  private String dataType;
  private String selectSql;
  private String description;

  /**
   * Create new instance of JasperTemplateParameterDto based on given
   * {@link JasperTemplateParameter}
   *
   * @param jasperTemplateParameter instance of Template
   * @return new instance of JasperTemplateDto.
   */
  public static JasperTemplateParameterDto newInstance(
      JasperTemplateParameter jasperTemplateParameter) {
    JasperTemplateParameterDto jasperTemplateParameterDto = new JasperTemplateParameterDto();
    jasperTemplateParameter.export(jasperTemplateParameterDto);
    return jasperTemplateParameterDto;
  }
}
