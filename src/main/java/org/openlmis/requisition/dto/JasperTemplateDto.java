package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.JasperTemplate;
import org.openlmis.requisition.domain.JasperTemplateParameter;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@AllArgsConstructor
@NoArgsConstructor
public class JasperTemplateDto implements JasperTemplate.Importer, JasperTemplate.Exporter {

  @Getter
  @Setter
  private UUID id;

  @Getter
  @Setter
  private String name;

  @Getter
  @Setter
  private String type;

  @Getter
  @Setter
  private String description;

  @Getter
  @Setter
  private byte[] data;

  @Setter
  private List<JasperTemplateParameterDto> templateParameters;

  @Override
  public List<JasperTemplateParameter.Importer> getTemplateParameters() {
    return new ArrayList<>(
        Optional.ofNullable(templateParameters).orElse(Collections.emptyList())
    );
  }

  /**
   * Create new list of JasperTemplateDto based on given list of {@link JasperTemplate}
   *
   * @param templates list of {@link JasperTemplate}
   * @return new list of JasperTemplateDto.
   */
  public static Iterable<JasperTemplateDto> newInstance(Iterable<JasperTemplate> templates) {

    List<JasperTemplateDto> jasperTemplateDtos = new ArrayList<>();
    templates.forEach(t -> jasperTemplateDtos.add(newInstance(t)));
    return jasperTemplateDtos;
  }

  /**
   * Create new instance of JasperTemplateDto based on given {@link JasperTemplate}
   *
   * @param jasperTemplate instance of Template
   * @return new instance of JasperTemplateDto.
   */
  public static JasperTemplateDto newInstance(JasperTemplate jasperTemplate) {
    if (jasperTemplate == null) {
      return null;
    }
    JasperTemplateDto jasperTemplateDto = new JasperTemplateDto();
    jasperTemplate.export(jasperTemplateDto);

    if (jasperTemplate.getTemplateParameters() != null) {
      jasperTemplateDto.setTemplateParameters(jasperTemplate.getTemplateParameters().stream()
          .map(JasperTemplateParameterDto::newInstance).collect(Collectors.toList()));
    }
    return jasperTemplateDto;
  }
}
