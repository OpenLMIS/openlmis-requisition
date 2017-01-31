package org.openlmis.requisition.dto;


import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.openlmis.requisition.domain.JasperTemplateParameter;

@Getter
@Setter
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
  private List<String> selectValues;

  /**
   * Create new instance of JasperTemplateParameterDto based on given {@link
   * JasperTemplateParameter}
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
