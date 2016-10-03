package org.openlmis.requisition.service;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionTemplateServiceTest {

  @Mock
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @InjectMocks
  private RequisitionTemplateService requisitionTemplateService;

  @Test
  public void shouldFindRequisitionTemplateIfItExists() {
    ProgramDto program = mock(ProgramDto.class);
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);

    when(requisitionTemplateRepository
            .getTemplateForProgram(program.getId()))
            .thenReturn(requisitionTemplate);

    RequisitionTemplate template =
            requisitionTemplateService.getTemplateForProgram(program.getId());

    assertNotNull(template);
    assertEquals(requisitionTemplate, template);
  }
}
