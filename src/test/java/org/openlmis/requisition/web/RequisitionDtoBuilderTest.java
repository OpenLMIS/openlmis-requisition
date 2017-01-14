package org.openlmis.requisition.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionLineItem;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.dto.RequisitionDto;
import org.openlmis.requisition.dto.RequisitionLineItemDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.utils.RequisitionExportHelper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

public class RequisitionDtoBuilderTest {

  @Mock
  private FacilityReferenceDataService facilityReferenceDataService;

  @Mock
  private PeriodService periodService;

  @Mock
  private ProgramReferenceDataService programReferenceDataService;

  @Mock
  private RequisitionExportHelper requisitionExportHelper;

  @InjectMocks
  private RequisitionDtoBuilder requisitionDtoBuilder = new RequisitionDtoBuilder();

  private Requisition requisition;

  @Mock
  private RequisitionLineItem requisitionLineItem;

  @Mock
  private RequisitionTemplate requisitionTemplate;

  @Mock
  private FacilityDto facilityDto;

  @Mock
  private ProcessingPeriodDto processingPeriodDto;

  @Mock
  private ProgramDto programDto;

  private List<RequisitionLineItemDto> lineItemDtos = new ArrayList<>();

  private UUID requisitionUuid = UUID.randomUUID();
  private UUID facilityUuid = UUID.randomUUID();
  private UUID processingPeriodUuid = UUID.randomUUID();
  private UUID programUuid = UUID.randomUUID();
  private UUID supervisoryNodeUuid = UUID.randomUUID();
  private UUID creatorUuid = UUID.randomUUID();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(requisitionExportHelper.exportToDtos(anyListOf(RequisitionLineItem.class)))
        .thenReturn(lineItemDtos);

    requisition = buildRequisition();
  }

  @Test
  public void shouldBuildDtoFromRequisition() {
    when(facilityReferenceDataService.findOne(facilityUuid)).thenReturn(facilityDto);
    when(programReferenceDataService.findOne(programUuid)).thenReturn(programDto);
    when(periodService.getPeriod(processingPeriodUuid)).thenReturn(processingPeriodDto);

    RequisitionDto requisitionDto = requisitionDtoBuilder.build(requisition);

    verify(requisitionExportHelper).exportToDtos(anyListOf(RequisitionLineItem.class));

    assertNotNull(requisitionDto);
    assertEquals(requisition.getId(), requisitionDto.getId());
    assertEquals(requisition.getCreatorId(), requisitionDto.getCreatorId());
    assertEquals(requisition.getSupervisoryNodeId(), requisitionDto.getSupervisoryNode());
    assertEquals(requisition.getTemplate().getId(), requisitionDto.getTemplate());
    assertEquals(requisition.getEmergency(), requisitionDto.getEmergency());
    assertEquals(facilityDto, requisitionDto.getFacility());
    assertEquals(programDto, requisitionDto.getProgram());
    assertEquals(processingPeriodDto, requisitionDto.getProcessingPeriod());
    assertEquals(lineItemDtos, requisitionDto.getRequisitionLineItems());
  }

  @Test
  public void shouldBuildDtoFromRequisitionWhenReferenceDataInstancesDoNotExist() {
    when(facilityReferenceDataService.findOne(facilityUuid)).thenReturn(null);
    when(programReferenceDataService.findOne(programUuid)).thenReturn(null);
    when(periodService.getPeriod(processingPeriodUuid)).thenReturn(null);

    RequisitionDto requisitionDto = requisitionDtoBuilder.build(requisition);

    verify(requisitionExportHelper).exportToDtos(anyListOf(RequisitionLineItem.class));

    assertNotNull(requisitionDto);
    assertNull(requisitionDto.getFacility());
    assertNull(requisitionDto.getProgram());
    assertNull(requisitionDto.getProcessingPeriod());
  }

  private Requisition buildRequisition() {
    Requisition requisition = new Requisition(facilityUuid, programUuid, processingPeriodUuid,
        creatorUuid, RequisitionStatus.INITIATED, false);
    requisition.setId(requisitionUuid);
    requisition.setSupervisoryNodeId(supervisoryNodeUuid);
    requisition.setTemplate(requisitionTemplate);
    requisition.setRequisitionLineItems(Collections.singletonList(requisitionLineItem));

    return requisition;
  }
}
