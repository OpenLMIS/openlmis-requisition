package org.openlmis.requisition.service;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.openlmis.referencedata.domain.Program;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class RequisitionTemplateServiceTest {

  @Mock
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @InjectMocks
  private RequisitionTemplateService requisitionTemplateService;

  private RequisitionTemplate requisitionTemplate;
  private Integer currentInstanceNumber;

  @Before
  public void setUp() {
    currentInstanceNumber = 0;
    requisitionTemplate = generateRequisitionTemplate();
    initMocks(this);
  }

  @Test
  public void shouldFindRequisitionTemplateIfItExists() {
    when(requisitionTemplateRepository
            .searchRequisitionTemplates(requisitionTemplate.getProgram()))
            .thenReturn(Arrays.asList(requisitionTemplate));
    List<RequisitionTemplate> receivedRequisitionTemplates
            = requisitionTemplateService
            .searchRequisitionTemplates(requisitionTemplate.getProgram());
    assertEquals(1,receivedRequisitionTemplates.size());

    assertEquals(
            requisitionTemplate.getProgram().getId(),
            receivedRequisitionTemplates.get(0).getProgram().getId());
  }

  private RequisitionTemplate generateRequisitionTemplate() {
    requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setProgram(generateProgram());
    return requisitionTemplate;
  }

  private Program generateProgram() {
    Program program = new Program();
    program.setId(UUID.randomUUID());
    program.setCode("code" + generateInstanceNumber());
    program.setPeriodsSkippable(false);
    return program;
  }

  private Integer generateInstanceNumber() {
    currentInstanceNumber += 1;
    return currentInstanceNumber;
  }
}
