package org.openlmis.requisition.dto;

import org.openlmis.requisition.domain.Template;
import org.openlmis.requisition.domain.TemplateParameter;

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
public class TemplateDto implements Template.Importer, Template.Exporter {

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
  private List<TemplateParameterDto> templateParameters;

  @Override
  public List<TemplateParameter.Importer> getTemplateParameters() {
    return new ArrayList<>(
        Optional.ofNullable(templateParameters).orElse(Collections.emptyList())
    );
  }

  /**
   * Create new list of TemplateDto based on given list of {@link Template}
   *
   * @param templates list of {@link Template}
   * @return new list of TemplateDto.
   */
  public static Iterable<TemplateDto> newInstance(Iterable<Template> templates) {

    List<TemplateDto> templateDtos = new ArrayList<>();
    templates.forEach(t -> templateDtos.add(newInstance(t)));
    return templateDtos;
  }

  /**
   * Create new instance of TemplateDto based on given {@link Template}
   *
   * @param template instance of Template
   * @return new instance of TemplateDto.
   */
  public static TemplateDto newInstance(Template template) {
    if (template == null) {
      return null;
    }
    TemplateDto templateDto = new TemplateDto();
    template.export(templateDto);

    if (template.getTemplateParameters() != null) {
      templateDto.setTemplateParameters(template.getTemplateParameters().stream()
          .map(TemplateParameterDto::newInstance).collect(Collectors.toList()));
    }
    return templateDto;
  }
}
