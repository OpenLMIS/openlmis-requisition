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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.APPROVED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.AUTHORIZED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.INITIATED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.RELEASED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SKIPPED;
import static org.openlmis.requisition.domain.requisition.RequisitionStatus.SUBMITTED;

import com.google.common.collect.Lists;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionStatus;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.openlmis.requisition.dto.RequisitionPeriodDto;
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

  private ProcessingPeriodDto currentPeriod;
  private ProcessingPeriodDto period1;
  private ProcessingPeriodDto period2;
  private ProcessingPeriodDto period3;
  private ProcessingPeriodDto period4;

  @Before
  public void setUp() throws Exception {
    currentPeriod = createPeriod(0);
    period1 = createPeriod(1);
    period2 = createPeriod(2);
    period3 = createPeriod(3);
    period4 = createPeriod(4);

    doReturn(INITIATED).when(initiatedRequsition).getStatus();
    doReturn(SUBMITTED).when(submittedRequsition).getStatus();
    doReturn(AUTHORIZED).when(authorizedRequsition).getStatus();
    doReturn(APPROVED).when(approvedRequsition).getStatus();

    doReturn(Collections.emptyList())
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(initiatedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(period1.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(submittedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(period2.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(authorizedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(period3.getId(), facilityId, programId, false);
    doReturn(Collections.singletonList(approvedRequsition))
        .when(requisitionRepository)
        .searchRequisitions(period4.getId(), facilityId, programId, false);
  }

  @Test
  public void shouldReturnCurrentPeriod() throws Exception {
    Requisition requisition = new Requisition();
    requisition.setStatus(SUBMITTED);

    doReturn(Collections.singletonList(currentPeriod))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, false);

    List<ProcessingPeriodDto> currentPeriods =
        periodService.getCurrentPeriods(programId, facilityId);

    assertThat(currentPeriods, hasSize(1));
    assertThat(currentPeriods.iterator().next().getId(), is(equalTo(currentPeriod.getId())));
  }

  @Test
  public void shouldReturnCurrentPeriodsIfThereIsMoreThanOne() throws Exception {
    Requisition requisition = new Requisition();
    requisition.setStatus(SUBMITTED);

    ProcessingPeriodDto period6 = new ProcessingPeriodDto();
    period6.setId(UUID.randomUUID());
    period6.setStartDate(currentPeriod.getStartDate());
    period6.setEndDate(currentPeriod.getEndDate());

    doReturn(Arrays.asList(currentPeriod, period6))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, false);
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
    assertThat(ids, contains(currentPeriod.getId(), period6.getId()));
  }

  @Test
  public void shouldReturnCurrentPeriodIfThereIsNoRequisition() throws Exception {
    doReturn(Collections.singletonList(currentPeriod))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    doReturn(Collections.emptyList())
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, false);

    List<ProcessingPeriodDto> currentPeriods =
        periodService.getCurrentPeriods(programId, facilityId);

    assertThat(currentPeriods, hasSize(1));
  }

  @Test
  public void shouldNotReturnCurrentPeriodIfItDoesNotExist() throws Exception {
    doReturn(Collections.singletonList(period1))
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

    doReturn(Collections.singletonList(currentPeriod))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, false);

    List<ProcessingPeriodDto> currentPeriods =
        periodService.getCurrentPeriods(programId, facilityId);

    assertThat(currentPeriods, hasSize(1));
  }

  @Test
  public void shouldReturnOnlyValidPeriodsForRequisitionInitiate() {
    List<ProcessingPeriodDto> list = Lists.newArrayList(
        currentPeriod, period1, period2, period3, period4
    );

    doReturn(list)
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    Collection<RequisitionPeriodDto> periods =
        periodService.getPeriods(programId, facilityId, false);

    verify(requisitionRepository, times(5)).searchRequisitions(
        any(UUID.class), any(UUID.class), any(UUID.class), any());

    assertNotNull(periods);
    assertEquals(3, periods.size());

    List<UUID> periodUuids = periods
        .stream()
        .map(ProcessingPeriodDto::getId)
        .collect(Collectors.toList());

    assertTrue(periodUuids.contains(currentPeriod.getId()));
    assertTrue(periodUuids.contains(period1.getId()));
    assertTrue(periodUuids.contains(period2.getId()));
  }

  @Test
  public void shouldIncludeRequisitionIdAndStatus() {
    doReturn(Collections.singletonList(currentPeriod))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setStatus(INITIATED);

    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, false);

    Collection<RequisitionPeriodDto> periods =
        periodService.getPeriods(programId, facilityId, false);

    assertNotNull(periods);
    assertThat(periods, hasSize(1));
    RequisitionPeriodDto period = periods.iterator().next();
    assertEquals(period.getRequisitionId(), requisition.getId());
    assertEquals(period.getRequisitionStatus(), requisition.getStatus());
  }

  @Test
  public void shouldReturnMultipleRequisitionPeriodsForEmergency() {
    doReturn(Collections.singletonList(currentPeriod))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setStatus(INITIATED);
    requisition.setEmergency(true);

    Requisition anotherRequisition = new Requisition();
    anotherRequisition.setId(UUID.randomUUID());
    anotherRequisition.setStatus(SUBMITTED);
    anotherRequisition.setEmergency(true);

    doReturn(Arrays.asList(requisition, anotherRequisition))
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, true);

    Collection<RequisitionPeriodDto> periods =
        periodService.getPeriods(programId, facilityId, true);

    assertNotNull(periods);
    assertThat(periods, hasSize(3));

    for (RequisitionPeriodDto period : periods) {
      assertEquals(currentPeriod.getId(), period.getId());
    }
    List<UUID> requisitionIds = periods
        .stream()
        .map(RequisitionPeriodDto::getRequisitionId)
        .collect(Collectors.toList());
    assertTrue(requisitionIds.contains(requisition.getId()));
    assertTrue(requisitionIds.contains(anotherRequisition.getId()));
    // should allow to initiate another requisition for the same period
    assertTrue(requisitionIds.contains(null));
  }

  @Test
  public void shouldNotReturnRequisitionsPostAuthorizeForEmergency() {
    doReturn(Collections.singletonList(currentPeriod))
        .when(periodReferenceDataService)
        .searchByProgramAndFacility(programId, facilityId);

    Requisition requisition = new Requisition();
    requisition.setId(UUID.randomUUID());
    requisition.setStatus(APPROVED);
    requisition.setEmergency(true);

    Requisition anotherRequisition = new Requisition();
    anotherRequisition.setId(UUID.randomUUID());
    anotherRequisition.setStatus(RELEASED);
    anotherRequisition.setEmergency(true);

    doReturn(Arrays.asList(requisition, anotherRequisition))
        .when(requisitionRepository)
        .searchRequisitions(currentPeriod.getId(), facilityId, programId, true);

    Collection<RequisitionPeriodDto> periods =
        periodService.getPeriods(programId, facilityId, true);

    assertNotNull(periods);
    assertThat(periods, hasSize(1));
    assertNull(periods.iterator().next().getRequisitionId());
  }

  @Test
  public void shouldReturnPreviousPeriods() throws Exception {
    doReturn(period4)
        .when(periodReferenceDataService)
        .findOne(period4.getId());
    doReturn(Arrays.asList(currentPeriod, period1, period2, period3, period4))
        .when(periodReferenceDataService)
        .search(any(), any());

    List<ProcessingPeriodDto> previousPeriods =
        periodService.findPreviousPeriods(period4.getId(), 2);


    assertEquals(Arrays.asList(period3, period2), previousPeriods);
  }

  @Test
  public void shouldReturnPreviousPeriod() throws Exception {
    doReturn(period4)
        .when(periodReferenceDataService)
        .findOne(period4.getId());
    doReturn(Arrays.asList(currentPeriod, period1, period2, period3, period4))
        .when(periodReferenceDataService)
        .search(any(), any());

    ProcessingPeriodDto previousPeriod =
        periodService.findPreviousPeriod(period4.getId());


    assertEquals(period3, previousPeriod);
  }

  @Test
  public void shouldReturnWholeListIfAmountGreaterThanListSize() throws Exception {
    doReturn(period4)
        .when(periodReferenceDataService)
        .findOne(period4.getId());
    doReturn(Arrays.asList(currentPeriod, period1, period2, period3, period4))
        .when(periodReferenceDataService)
        .search(any(), any());

    List<ProcessingPeriodDto> previousPeriods =
        periodService.findPreviousPeriods(period4.getId(), 5);

    assertEquals(Arrays.asList(period3, period2, period1, currentPeriod), previousPeriods);
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
        .thenReturn(Lists.newArrayList(currentPeriod));
    when(scheduleReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(null);

    periodService.findPeriod(programId, facilityId, null, false);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionIfPeriodIsNotTheOldest() {
    ProcessingScheduleDto processingScheduleDto = new ProcessingScheduleDto();
    processingScheduleDto.setId(UUID.randomUUID());

    currentPeriod.setProcessingSchedule(processingScheduleDto);
    currentPeriod.setId(UUID.randomUUID());

    when(periodReferenceDataService.searchByProgramAndFacility(programId, facilityId))
        .thenReturn(Lists.newArrayList(currentPeriod));

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
    setMockForFindPeriod();

    mockRequisitionFound(getRequisition(INITIATED), currentPeriod.getId(), facilityId, programId);
    mockNoRequisitionFound(period1.getId(), facilityId, programId);

    periodService.findPeriod(programId, facilityId, null, false);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenPreviousReqHasSubmittedStatus() {
    setMockForFindPeriod();

    mockRequisitionFound(getRequisition(SUBMITTED), currentPeriod.getId(), facilityId, programId);
    mockNoRequisitionFound(period1.getId(), facilityId, programId);

    periodService.findPeriod(programId, facilityId, null, false);
  }

  @Test
  public void shouldSucceedWhenPreviousReqHasAuthorizedStatus() {
    setMockForFindPeriod();

    mockRequisitionFound(getRequisition(AUTHORIZED), currentPeriod.getId(), facilityId, programId);
    mockNoRequisitionFound(period1.getId(), facilityId, programId);

    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);
    assertEquals(period1, period);
  }

  @Test
  public void shouldSucceedWhenPreviousReqHasApprovedStatus() {
    setMockForFindPeriod();

    mockRequisitionFound(getRequisition(APPROVED), currentPeriod.getId(), facilityId, programId);
    mockNoRequisitionFound(period1.getId(), facilityId, programId);

    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);
    assertEquals(period1, period);
  }

  @Test
  public void shouldSucceedWhenPreviousReqHasSkippedStatus() {
    setMockForFindPeriod();

    mockRequisitionFound(getRequisition(SKIPPED), currentPeriod.getId(), facilityId, programId);
    mockNoRequisitionFound(period1.getId(), facilityId, programId);

    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);
    assertEquals(period1, period);
  }

  @Test
  public void shouldSucceedWhenRequisitionForNextPeriodIsPresent() {
    setMockForFindPeriod();

    mockRequisitionFound(getRequisition(AUTHORIZED), currentPeriod.getId(), facilityId, programId);
    mockNoRequisitionFound(period1.getId(), facilityId, programId);
    mockRequisitionFound(getRequisition(INITIATED), period2.getId(), facilityId, programId);

    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);
    assertEquals(period1, period);
  }

  @Test
  public void shouldUseNextAvailablePeriodWhenOneIsTaken() {
    //given
    setMockForFindPeriod();

    // we mock the requisition search to return a requisition for the first period
    // but not for the second
    mockRequisitionFound(getRequisition(APPROVED), currentPeriod.getId(), facilityId, programId);
    mockNoRequisitionFound(period1.getId(), facilityId, programId);

    //when
    ProcessingPeriodDto period = periodService.findPeriod(programId, facilityId, null, false);

    // then, we expect a second period to be returned
    assertEquals(period1, period);
  }

  @Test
  public void shouldAllowUsingTheSamePeriodInMultipleFacilities() {
    //given
    setMockForFindPeriod();

    // we mock the requisition search to return a requisition for the first period
    // in facility with ID facilityId
    mockRequisitionFound(initiatedRequsition, currentPeriod.getId(), facilityId, programId);

    //when
    ProcessingPeriodDto period = periodService.findPeriod(programId, facility2Id, null, false);

    // then, we expect a first period to be returned for second facility, despite it being used in
    // the first facility
    assertEquals(currentPeriod, period);
  }

  private Requisition getRequisition(RequisitionStatus status) {
    Requisition requisition = new Requisition();
    requisition.setStatus(status);
    return requisition;
  }

  private void mockRequisitionFound(
      Requisition requisition, UUID periodId, UUID facilityId, UUID programId) {
    doReturn(Collections.singletonList(requisition))
        .when(requisitionRepository)
        .searchRequisitions(periodId, facilityId, programId, false);
  }

  private void mockNoRequisitionFound(UUID periodId, UUID facilityId, UUID programId) {
    doReturn(Collections.emptyList())
        .when(requisitionRepository)
        .searchRequisitions(periodId, facilityId, programId, false);
  }

  private void setMockForFindPeriod() {
    List<ProcessingPeriodDto> periods = new ArrayList<>();
    periods.add(currentPeriod);
    periods.add(period1);
    periods.add(period2);

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
    when(currentPeriod.getProcessingSchedule().getId()).thenReturn(id);
    when(period1.getProcessingSchedule().getId()).thenReturn(id);
  }

}
