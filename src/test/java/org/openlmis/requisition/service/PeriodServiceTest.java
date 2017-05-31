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

package org.openlmis.requisition.service;

import static java.time.temporal.TemporalAdjusters.firstDayOfMonth;
import static java.time.temporal.TemporalAdjusters.lastDayOfMonth;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.RequisitionStatus.AUTHORIZED;
import static org.openlmis.requisition.domain.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.domain.RequisitionStatus.SUBMITTED;

import com.google.common.collect.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.Requisition;
import org.openlmis.requisition.domain.RequisitionStatus;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.exception.ContentNotFoundMessageException;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.repository.RequisitionRepository;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ScheduleReferenceDataService;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@RunWith(MockitoJUnitRunner.class)
@SuppressWarnings("PMD.TooManyMethods")
public class PeriodServiceTest {

  @Mock
  private Requisition initiatedRequsition;

  @Mock
  private Requisition submittedRequsition;

  @Mock
  private Requisition authorizedRequsition;

  @Mock
  private Requisition approvedRequsition;

  @Mock
  private PeriodReferenceDataService periodReferenceDataService;

  @Mock
  private RequisitionRepository requisitionRepository;

  @Mock
  private ScheduleReferenceDataService scheduleReferenceDataService;

  @InjectMocks
  private PeriodService periodService;

  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();
  private UUID facility2Id = UUID.randomUUID();

  private ProcessingPeriodDto period1;
  private ProcessingPeriodDto period2;
  private ProcessingPeriodDto period3;
  private ProcessingPeriodDto period4;
  private ProcessingPeriodDto currentPeriod;

  @Before
  public void setUp() throws Exception {
    period1 = createPeriod(0);
    period2 = createPeriod(1);
    period3 = createPeriod(2);
    period4 = createPeriod(3);
    currentPeriod = createPeriod(4);

    doReturn(INITIATED).when(initiatedRequsition).getStatus();
    doReturn(SUBMITTED).when(submittedRequsition).getStatus();
    doReturn(AUTHORIZED).when(authorizedRequsition).getStatus();
    doReturn(APPROVED).when(approvedRequsition).getStatus();

    doReturn(Collections.emptyList())
        .when(requisitionRepository)
        .searchRequisitions(period1.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(initiatedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(period2.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(submittedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(period3.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(authorizedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(period4.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(approvedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, false);
  }

  @Test
  public void shouldReturnCurrentPeriod() throws Exception {
    Requisition requisition = new Requisition();
    requisition.setStatus(SUBMITTED);

    doReturn(Collections.singletonList(period1))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(period1.getId(), facilityId, programId, false);

    List<ProcessingPeriodDto> currentPeriods =
        periodService.getCurrentPeriods(programId, facilityId);

    assertThat(currentPeriods, hasSize(1));
    assertThat(currentPeriods.iterator().next().getId(), is(equalTo(period1.getId())));
  }

  @Test
  public void shouldReturnCurrentPeriodsIfThereIsMoreThanOne() throws Exception {
    Requisition requisition = new Requisition();
    requisition.setStatus(SUBMITTED);

    ProcessingPeriodDto period6 = new ProcessingPeriodDto();
    period6.setId(UUID.randomUUID());
    period6.setStartDate(period1.getStartDate());
    period6.setEndDate(period1.getEndDate());

    doReturn(Arrays.asList(period1, period6))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(period1.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(period6.getId(), facilityId, programId, false);

    List<ProcessingPeriodDto> currentPeriods =
        periodService.getCurrentPeriods(programId, facilityId);

    List<UUID> ids = currentPeriods
        .stream()
        .map(ProcessingPeriodDto::getId)
        .collect(Collectors.toList());

    assertThat(ids, hasSize(2));
    assertThat(ids, contains(period1.getId(), period6.getId()));
  }

  @Test
  public void shouldReturnCurrentPeriodIfThereIsNoRequisition() throws Exception {
    doReturn(Collections.singletonList(period1))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    doReturn(Collections.emptyList())
        .when(requisitionRepository)
        .searchRequisitions(period1.getId(), facilityId, programId, false);

    List<ProcessingPeriodDto> currentPeriods =
        periodService.getCurrentPeriods(programId, facilityId);

    assertThat(currentPeriods, hasSize(1));
  }

  @Test
  public void shouldNotReturnCurrentPeriodIfItDoesNotExist() throws Exception {
    doReturn(Collections.singletonList(period2))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    List<ProcessingPeriodDto> currentPeriods =
        periodService.getCurrentPeriods(programId, facilityId);

    assertThat(currentPeriods, hasSize(0));
  }

  @Test
  public void shouldReturnCurrentPeriodIfThereIsNonSubmittedRequisition() throws Exception {
    Requisition requisition = new Requisition();
    requisition.setStatus(INITIATED);

    doReturn(Collections.singletonList(period1))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(period1.getId(), facilityId, programId, false);

    List<ProcessingPeriodDto> currentPeriods =
        periodService.getCurrentPeriods(programId, facilityId);

    assertThat(currentPeriods, hasSize(1));
  }

  @Test
  public void shouldReturnOnlyValidPeriodsForRequisitionInitiate() {
    List<ProcessingPeriodDto> list = Lists.newArrayList(
        period1, period2, period3, period4, currentPeriod
    );

    doReturn(list)
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    Collection<ProcessingPeriodDto> periods =
        periodService.getPeriods(programId, facilityId, false);

    verify(requisitionRepository, times(5)).searchRequisitions(
        any(UUID.class), any(UUID.class), any(UUID.class), any());

    assertNotNull(periods);
    assertEquals(3, periods.size());

    List<UUID> periodUuids = periods
        .stream()
        .map(ProcessingPeriodDto::getId)
        .collect(Collectors.toList());

    assertTrue(periodUuids.contains(period1.getId()));
    assertTrue(periodUuids.contains(period2.getId()));
    assertTrue(periodUuids.contains(period3.getId()));
  }

  @Test
  public void shouldReturnPreviousPeriods() throws Exception {
    doReturn(currentPeriod)
        .when(periodReferenceDataService)
        .findOne(currentPeriod.getId());
    doReturn(Arrays.asList(period1, period2, period3, period4, currentPeriod))
        .when(periodReferenceDataService)
        .search(any(), any());

    List<ProcessingPeriodDto> previousPeriods =
        periodService.findPreviousPeriods(currentPeriod.getId(), 2);


    assertEquals(Arrays.asList(period4, period3), previousPeriods);
  }

  @Test
  public void shouldReturnPreviousPeriod() throws Exception {
    doReturn(currentPeriod)
        .when(periodReferenceDataService)
        .findOne(currentPeriod.getId());
    doReturn(Arrays.asList(period1, period2, period3, period4, currentPeriod))
        .when(periodReferenceDataService)
        .search(any(), any());

    ProcessingPeriodDto previousPeriod =
        periodService.findPreviousPeriod(currentPeriod.getId());


    assertEquals(period4, previousPeriod);
  }

  @Test
  public void shouldReturnWholeListIfAmountGreaterThanListSize() throws Exception {
    doReturn(currentPeriod)
        .when(periodReferenceDataService)
        .findOne(currentPeriod.getId());
    doReturn(Arrays.asList(period1, period2, period3, period4, currentPeriod))
        .when(periodReferenceDataService)
        .search(any(), any());

    List<ProcessingPeriodDto> previousPeriods =
        periodService.findPreviousPeriods(currentPeriod.getId(), 5);

    assertEquals(Arrays.asList(period4, period3, period2, period1), previousPeriods);
  }

  private ProcessingPeriodDto createPeriod(int plusMonth) {
    ProcessingPeriodDto dto = new ProcessingPeriodDto();
    dto.setId(UUID.randomUUID());
    dto.setProcessingSchedule(mock(ProcessingScheduleDto.class));
    dto.setStartDate(LocalDate.now().plusMonths(plusMonth).with(firstDayOfMonth()));
    dto.setEndDate(LocalDate.now().plusMonths(plusMonth).with(lastDayOfMonth()));

    return dto;
  }

  @Test(expected = ContentNotFoundMessageException.class)
  public void shouldThrowExceptionIfScheduleDoesNotExist()
      throws ContentNotFoundMessageException {
    when(periodReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(Lists.newArrayList(period1));
    when(scheduleReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(null);

    periodService.findPeriod(programId, facilityId, null, false);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionIfPeriodIsNotTheOldest() {
    ProcessingScheduleDto processingScheduleDto = new ProcessingScheduleDto();
    processingScheduleDto.setId(UUID.randomUUID());

    period1.setProcessingSchedule(processingScheduleDto);
    period1.setId(UUID.randomUUID());

    when(periodReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(Lists.newArrayList(period1));

    when(scheduleReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(Collections.singletonList(processingScheduleDto));

    periodService.findPeriod(programId, facilityId, UUID.randomUUID(), false);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenInitiatingReqPeriodDoesNotBelongToTheSameScheduleAsProgram() {

    ProcessingScheduleDto processingScheduleDto = new ProcessingScheduleDto();
    processingScheduleDto.setId(UUID.randomUUID());
    when(scheduleReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(Collections.singletonList(processingScheduleDto));

    periodService.findPeriod(programId, facilityId, null, false);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenPreviousReqHasInitiatedStatus() {

    Requisition requisition = getRequisition(INITIATED);
    when(requisitionRepository.getLastRegularRequisition(facilityId, programId))
        .thenReturn(requisition);

    periodService.findPeriod(programId, facilityId, null, false);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenPreviousReqHasSubmittedStatus() {

    Requisition requisition = getRequisition(SUBMITTED);
    when(requisitionRepository.getLastRegularRequisition(facilityId, programId))
        .thenReturn(requisition);

    periodService.findPeriod(programId, facilityId, null, false);
  }

  @Test
  public void shouldSucceedWhenPreviousReqHasAuthorizedStatus() {
    Requisition requisition = getRequisition(AUTHORIZED);
    setMockForFindPeriod(requisition);

    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);
    assertEquals(period1, period);
  }

  @Test
  public void shouldSucceedWhenPreviousReqHasApprovedStatus() {

    Requisition requisition = getRequisition(APPROVED);
    setMockForFindPeriod(requisition);

    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);
    assertEquals(period1, period);
  }

  @Test
  public void shouldSucceedWhenPreviousReqHasSkippedStatus() {

    Requisition requisition = getRequisition(SKIPPED);
    setMockForFindPeriod(requisition);

    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);
    assertEquals(period1, period);
  }

  @Test
  public void shouldUseNextAvailablePeriodWhenOneIsTaken() {
    //given
    Requisition requisition = getRequisition(APPROVED);
    setMockForFindPeriod(requisition);

    //we mock the requisition search to return a requisition for the first period
    // but not for the second
    doReturn(1)
        .when(requisitionRepository)
        .getRequisitionsCount(period1.getId(), facilityId, programId, false);
    doReturn(0)
        .when(requisitionRepository)
        .getRequisitionsCount(period2.getId(), facilityId, programId, false);

    //when
    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);

    // then, we expect a second period to be returned
    assertEquals(period2, period);
  }

  @Test
  public void shouldAllowUsingTheSamePeriodInMultipleFacilities() {
    //given
    Requisition requisition = getRequisition(APPROVED);
    setMockForFindPeriod(requisition);

    // we mock the requisition search to return a requisition for the first period
    // in facility with ID facilityId
    doReturn(Collections.singletonList(initiatedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(period1.getId(), facilityId, programId, false);

    //when
    ProcessingPeriodDto period = periodService.findPeriod(programId, facility2Id, null, false);

    // then, we expect a first period to be returned for second facility, despite it being used in
    // the first facility
    assertEquals(period1, period);
  }

  private Requisition getRequisition(RequisitionStatus status) {
    Requisition requisition = new Requisition();
    requisition.setStatus(status);
    return requisition;
  }

  private void setMockForFindPeriod(Requisition requisition) {
    when(requisitionRepository.getLastRegularRequisition(facilityId, programId))
        .thenReturn(requisition);
    List<ProcessingPeriodDto> periods = new ArrayList<>();
    periods.add(period1);
    periods.add(period2);
    periods.add(period3);
    when(periodReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(periods);
    when(periodReferenceDataService.searchByProgramAndFacility(programId, facility2Id))
        .thenReturn(periods);

    ProcessingScheduleDto processingScheduleDto = new ProcessingScheduleDto();
    UUID id = UUID.randomUUID();
    processingScheduleDto.setId(id);
    when(scheduleReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(Collections.singletonList(processingScheduleDto));
    when(scheduleReferenceDataService.searchByProgramAndFacility(programId, facility2Id))
        .thenReturn(Collections.singletonList(processingScheduleDto));
    when(period1.getProcessingSchedule().getId()).thenReturn(id);
    when(period2.getProcessingSchedule().getId()).thenReturn(id);
  }

}
