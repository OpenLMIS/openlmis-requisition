package org.openlmis.requisition.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.repository.RequisitionTemplateRepository;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionTemplateServiceTest {

  @Mock
  private RequisitionTemplateRepository requisitionTemplateRepository;

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private RequisitionTemplate requisitionTemplate;

  @Mock
  private Requisition requisition;

  private UUID templateUuid = UUID.randomUUID();

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

  @Test
  public void shouldUpdateRequisitionTemplateIfItHasNoRequisitions() {
    // given
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    UUID templateId = UUID.randomUUID();
    requisitionTemplate.setId(templateId);

    when(requisitionTemplateRepository.save(requisitionTemplate)).thenReturn(requisitionTemplate);
    when(requisitionRepository.findByTemplateId(templateId)).thenReturn(Collections.emptyList());

    // when
    RequisitionTemplate result = requisitionTemplateService.save(requisitionTemplate);

    // then
    assertEquals(templateId, result.getId());
  }

  @Test
  public void shouldSaveNewRequisitionTemplateIfItHasSomeRequisitions() {
    // given
    RequisitionTemplate requisitionTemplate = new RequisitionTemplate();
    requisitionTemplate.setId(UUID.randomUUID());

    when(requisitionTemplateRepository.save(requisitionTemplate)).thenReturn(requisitionTemplate);
    when(requisitionRepository.findByTemplateId(requisitionTemplate.getId()))
        .thenReturn(Collections.singletonList(mock(Requisition.class)));

    // when
    RequisitionTemplate result = requisitionTemplateService.save(requisitionTemplate);

    // then
    assertNotEquals(requisitionTemplate, result);
  }

  @Test
  public void shouldDeleteRequisitionTemplateIfNotUsed() {
    //given
    when(requisitionRepository.findByTemplateId(templateUuid)).thenReturn(Collections.EMPTY_LIST);

    //when
    requisitionTemplateService.delete(requisitionTemplate);

    //then
    verify(requisitionTemplateRepository).delete(requisitionTemplate);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenAttemptingToDeleteRequisitionTemplateInUse() {
    //given
    List<Requisition> requisitions = new ArrayList<>();
    requisitions.add(requisition);
    when(requisitionTemplate.getId()).thenReturn(templateUuid);

    when(requisitionRepository.findByTemplateId(templateUuid)).thenReturn(requisitions);

    //when
    requisitionTemplateService.delete(requisitionTemplate);
  }
}
