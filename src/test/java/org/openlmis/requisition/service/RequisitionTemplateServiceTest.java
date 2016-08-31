package org.openlmis.requisition.service;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
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
    Program program = mock(Program.class);
    RequisitionTemplate requisitionTemplate = mock(RequisitionTemplate.class);

    when(requisitionTemplateRepository
            .searchRequisitionTemplates(program))
            .thenReturn(Arrays.asList(requisitionTemplate));

    List<RequisitionTemplate> receivedRequisitionTemplates =
            requisitionTemplateService.searchRequisitionTemplates(program);

    assertEquals(1, receivedRequisitionTemplates.size());
    assertEquals(requisitionTemplate, receivedRequisitionTemplates.get(0));
  }
}
