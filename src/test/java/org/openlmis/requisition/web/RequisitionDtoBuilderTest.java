/*
 * This program is part of the OpenLMIS logistics management information system platform software.
 * Copyright © 2017 VillageReach
 *
 * This program is free software: you can redistribute it and/or modify it under the terms
 * of the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see
 * http://www.gnu.org/licenses.  For additional information contact info@OpenLMIS.org. 
 */

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
import org.openlmis.requisition.dto.RequisitionTemplateDto;
import org.openlmis.requisition.service.PeriodService;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.utils.RequisitionExportHelper;
import java.time.LocalDate;
import java.time.ZonedDateTime;
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

  @Mock
  private RequisitionLineItem requisitionLineItem;

  @Mock
  private FacilityDto facilityDto;

  @Mock
  private ProcessingPeriodDto processingPeriodDto;

  @Mock
  private ProgramDto programDto;

  @InjectMocks
  private RequisitionDtoBuilder requisitionDtoBuilder = new RequisitionDtoBuilder();

  private Requisition requisition;

  private List<RequisitionLineItemDto> lineItemDtos = new ArrayList<>();

  private UUID requisitionUuid = UUID.randomUUID();
  private UUID facilityUuid = UUID.randomUUID();
  private UUID processingPeriodUuid = UUID.randomUUID();
  private UUID programUuid = UUID.randomUUID();
  private UUID supervisoryNodeUuid = UUID.randomUUID();

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
    assertEquals(requisition.getSupervisoryNodeId(), requisitionDto.getSupervisoryNode());
    assertEquals(RequisitionTemplateDto.newInstance(requisition.getTemplate()).getId(),
        requisitionDto.getTemplate().getId());
    assertEquals(requisition.getEmergency(), requisitionDto.getEmergency());
    assertEquals(facilityDto, requisitionDto.getFacility());
    assertEquals(programDto, requisitionDto.getProgram());
    assertEquals(processingPeriodDto, requisitionDto.getProcessingPeriod());
    assertEquals(requisition.getModifiedDate(), requisitionDto.getModifiedDate());
    assertEquals(lineItemDtos, requisitionDto.getRequisitionLineItems());
    assertEquals(
        requisition.getDatePhysicalStockCountCompleted(),
        requisitionDto.getDatePhysicalStockCountCompleted());
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
        RequisitionStatus.INITIATED, false);
    requisition.setId(requisitionUuid);
    requisition.setSupervisoryNodeId(supervisoryNodeUuid);
    RequisitionTemplate template = new RequisitionTemplate();
    template.setId(UUID.randomUUID());
    template.setProgramId(UUID.randomUUID());
    requisition.setTemplate(template);
    requisition.setModifiedDate(ZonedDateTime.now());
    requisition.setRequisitionLineItems(Collections.singletonList(requisitionLineItem));
    requisition.setDatePhysicalStockCountCompleted(LocalDate.now());

    return requisition;
  }
}
