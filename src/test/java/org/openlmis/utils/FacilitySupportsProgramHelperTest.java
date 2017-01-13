package org.openlmis.utils;

import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.SupportedProgramDto;
import org.openlmis.requisition.exception.ValidationMessageException;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.UUID;

@RunWith(MockitoJUnitRunner.class)
public class FacilitySupportsProgramHelperTest {

  private static final String SUPPORT_START_DATE = "2011-12-12";

  @Mock
  FacilityReferenceDataService facilityReferenceDataService;

  @InjectMocks
  FacilitySupportsProgramHelper facilitySupportsProgramHelper;

  private FacilityDto facilityDto;
  private UUID programId = UUID.randomUUID();
  private UUID facilityId = UUID.randomUUID();

  @Before
  public void setUp() {
    facilityDto = new FacilityDto();

    when(facilityReferenceDataService.findOne(facilityId)).thenReturn(facilityDto);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenNoSupportedPrograms() {
    facilityDto.setSupportedPrograms(Collections.emptyList());

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenProgramIsNotSupported() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(UUID.randomUUID());
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(true);
    supportedProgramDto.setSupportStartDate(SUPPORT_START_DATE);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test
  public void shouldPassWhenProgramIsSupported() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(true);
    supportedProgramDto.setSupportStartDate(SUPPORT_START_DATE);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenSupportIsNotActive() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(false);
    supportedProgramDto.setProgramActive(true);
    supportedProgramDto.setSupportStartDate(SUPPORT_START_DATE);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenProgramIsNotActive() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(false);
    supportedProgramDto.setSupportStartDate(SUPPORT_START_DATE);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test(expected = ValidationMessageException.class)
  public void shouldThrowExceptionWhenStartDateIsAfterCurrentDate() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(false);
    String tomorrow = LocalDate.now().plusDays(1).format(DateTimeFormatter.ISO_LOCAL_DATE);
    supportedProgramDto.setSupportStartDate((tomorrow));

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

  @Test
  public void shouldPassWhenProgramStartDateIsNull() {
    SupportedProgramDto supportedProgramDto = new SupportedProgramDto();
    supportedProgramDto.setId(programId);
    supportedProgramDto.setSupportActive(true);
    supportedProgramDto.setProgramActive(true);
    supportedProgramDto.setSupportStartDate(null);

    facilityDto.setSupportedPrograms(Collections.singletonList(supportedProgramDto));

    facilitySupportsProgramHelper.checkIfFacilitySupportsProgram(facilityId, programId);
  }

}