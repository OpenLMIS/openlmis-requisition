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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.time.LocalDate;
import java.util.UUID;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.openlmis.requisition.code.CodeGenerationException;
import org.openlmis.requisition.code.CodeSequenceRepository;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.domain.requisition.RequisitionDataBuilder;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.FacilityTypeDto;
import org.openlmis.requisition.dto.GeographicLevelDto;
import org.openlmis.requisition.dto.GeographicZoneDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.utils.DateHelper;

@RunWith(MockitoJUnitRunner.Silent.class)
public class RequisitionCodeGeneratorTest {

  private static final String DEFAULT_PATTERN =
      "REQ-{facilityCode}-{programCode}-{date:yyyyMM}-{seq:4}";
  private static final String DEFAULT_SCOPE = "facility,program,year,month";

  @Mock
  private CodeSequenceRepository sequences;

  @Mock
  private FacilityReferenceDataService facilities;

  @Mock
  private ProgramReferenceDataService programs;

  @Mock
  private PeriodReferenceDataService periods;

  @Mock
  private DateHelper dateHelper;

  private Requisition requisition;

  @Before
  public void setUp() {
    requisition = new RequisitionDataBuilder()
        .withEmergency(true)
        .buildInitiatedRegularRequisition();

    GeographicZoneDto country = zone("MW", 1, null);
    GeographicZoneDto region = zone("MW-S", 2, country);
    final GeographicZoneDto district = zone("MW-S-BL", 3, region);

    FacilityTypeDto type = new FacilityTypeDto();
    type.setCode("HC");

    FacilityDto facility = new FacilityDto();
    facility.setId(requisition.getFacilityId());
    facility.setCode("HC01");
    facility.setType(type);
    facility.setGeographicZone(district);
    when(facilities.findOne(requisition.getFacilityId())).thenReturn(facility);

    ProgramDto program = new ProgramDto();
    program.setId(requisition.getProgramId());
    program.setCode("PRG001");
    when(programs.findOne(requisition.getProgramId())).thenReturn(program);

    ProcessingPeriodDto period = new ProcessingPeriodDto();
    period.setId(requisition.getProcessingPeriodId());
    period.setName("Jan2026");
    period.setStartDate(LocalDate.of(2026, 1, 1));
    period.setEndDate(LocalDate.of(2026, 1, 31));
    when(periods.findOne(requisition.getProcessingPeriodId())).thenReturn(period);

    when(dateHelper.getCurrentDateWithSystemZone()).thenReturn(LocalDate.of(2026, 10, 30));
    when(sequences.next(anyString())).thenReturn(7);
  }

  @Test
  public void shouldGenerateCodeWithDefaultPattern() {
    String code = generator(DEFAULT_PATTERN, DEFAULT_SCOPE).generate(requisition);

    assertThat(code).isEqualTo("REQ-HC01-PRG001-202610-0007");
    ArgumentCaptor<String> key = ArgumentCaptor.forClass(String.class);
    Mockito.verify(sequences).next(key.capture());
    assertThat(key.getValue()).isEqualTo("requisition|facility=" + requisition.getFacilityId()
        + ";program=" + requisition.getProgramId() + ";year=2026;month=10");
  }

  @Test
  public void shouldGenerateCodeWithAllTokens() {
    String code = generator("{facilityTypeCode}/{zoneCode:2}/{zoneCode}/{programCode}/"
        + "{periodName}/{periodStart:ddMM}-{periodEnd:ddMM}/{emergency:E|R}/{seq:3}",
        "facility,program,period,emergency").generate(requisition);

    assertThat(code).isEqualTo("HC/MW-S/MW-S-BL/PRG001/Jan2026/0101-3101/E/007");
  }

  @Test
  public void shouldFailWhenZoneLevelIsNotInHierarchy() {
    RequisitionCodeGenerator generator = generator("{zoneCode:4}{seq}", "");

    assertThatThrownBy(() -> generator.generate(requisition))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("at level 4");
  }

  @Test
  public void shouldFailWhenFacilityIsNotFound() {
    when(facilities.findOne(requisition.getFacilityId())).thenReturn(null);
    RequisitionCodeGenerator generator = generator(DEFAULT_PATTERN, DEFAULT_SCOPE);

    assertThatThrownBy(() -> generator.generate(requisition))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("facility code");
  }

  @Test
  public void shouldNotOfferSupplyingFacility() {
    assertThatThrownBy(() -> generator("{supplyingFacilityCode}{seq}", ""))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("Unknown token 'supplyingFacilityCode'");
  }

  private RequisitionCodeGenerator generator(String pattern, String scope) {
    RequisitionCodeGenerator generator = new RequisitionCodeGenerator(pattern, scope, sequences,
        facilities, programs, periods, dateHelper);
    generator.compile();
    return generator;
  }

  private static GeographicZoneDto zone(String code, int levelNumber, GeographicZoneDto parent) {
    GeographicLevelDto level = new GeographicLevelDto();
    level.setLevelNumber(levelNumber);
    GeographicZoneDto zone = new GeographicZoneDto();
    zone.setId(UUID.randomUUID());
    zone.setCode(code);
    zone.setLevel(level);
    zone.setParent(parent);
    return zone;
  }
}
