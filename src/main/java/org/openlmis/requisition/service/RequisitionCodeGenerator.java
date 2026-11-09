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

import java.time.LocalDate;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;
import org.openlmis.requisition.code.AbstractPatternCodeGenerator;
import org.openlmis.requisition.code.CodeGenerationException;
import org.openlmis.requisition.code.CodeSequenceRepository;
import org.openlmis.requisition.code.ConditionalToken;
import org.openlmis.requisition.code.DateToken;
import org.openlmis.requisition.code.LookupToken;
import org.openlmis.requisition.code.PatternToken;
import org.openlmis.requisition.code.RandomToken;
import org.openlmis.requisition.domain.requisition.Requisition;
import org.openlmis.requisition.dto.FacilityDto;
import org.openlmis.requisition.dto.FacilityTypeDto;
import org.openlmis.requisition.dto.GeographicZoneDto;
import org.openlmis.requisition.dto.ProcessingPeriodDto;
import org.openlmis.requisition.dto.ProgramDto;
import org.openlmis.requisition.service.referencedata.FacilityReferenceDataService;
import org.openlmis.requisition.service.referencedata.PeriodReferenceDataService;
import org.openlmis.requisition.service.referencedata.ProgramReferenceDataService;
import org.openlmis.requisition.utils.DateHelper;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Generates requisition codes from the {@code requisition.code.pattern} property.
 *
 * <p>Supported tokens: {@code facilityCode}, {@code facilityTypeCode}, {@code zoneCode:level},
 * {@code programCode}, {@code periodName}, {@code periodStart:format}, {@code periodEnd:format},
 * {@code date:format}, {@code emergency:valueIfTrue|valueIfFalse}, {@code random:length} and
 * {@code seq:width}.
 *
 * <p>Supported sequence scope dimensions: {@code facility}, {@code program}, {@code period},
 * {@code emergency}, {@code year} and {@code month}.
 *
 * <p>The supplying facility is not known when a requisition is initiated, so it is deliberately
 * not offered as a token.
 */
@Component
public class RequisitionCodeGenerator extends AbstractPatternCodeGenerator<Requisition> {

  static final String SEQUENCE_NAME = "requisition";

  private final DateHelper dateHelper;

  /**
   * Creates the generator and registers everything a requisition offers to the pattern.
   */
  public RequisitionCodeGenerator(
      @Value("${requisition.code.pattern}") String pattern,
      @Value("${requisition.code.sequenceScope}") String sequenceScope,
      CodeSequenceRepository sequences,
      @Qualifier("facilityReferenceDataService") FacilityReferenceDataService facilities,
      ProgramReferenceDataService programs,
      PeriodReferenceDataService periods,
      DateHelper dateHelper) {
    super(SEQUENCE_NAME, pattern, splitScope(sequenceScope), sequences);
    this.dateHelper = dateHelper;

    final Function<UUID, Optional<FacilityDto>> facility = id -> Optional.ofNullable(
        facilities.findOne(id));
    final Function<UUID, Optional<ProcessingPeriodDto>> period = id -> Optional.ofNullable(
        periods.findOne(id));

    token("facilityCode", new LookupToken<>("facility code", Requisition::getFacilityId,
        id -> facility.apply(id).map(FacilityDto::getCode).orElse(null)));
    token("facilityTypeCode", new LookupToken<>("facility type code",
        Requisition::getFacilityId,
        id -> facility.apply(id).map(FacilityDto::getType).map(FacilityTypeDto::getCode)
            .orElse(null)));
    token("zoneCode", new ZoneCodeToken(id -> facility.apply(id)
        .map(FacilityDto::getGeographicZone).orElse(null)));
    token("programCode", new LookupToken<>("program code", Requisition::getProgramId,
        id -> Optional.ofNullable(programs.findOne(id)).map(ProgramDto::getCode).orElse(null)));
    token("periodName", new LookupToken<>("period name", Requisition::getProcessingPeriodId,
        id -> period.apply(id).map(ProcessingPeriodDto::getName).orElse(null)));
    token("periodStart", new DateToken<>("period start date", r -> period
        .apply(r.getProcessingPeriodId()).map(ProcessingPeriodDto::getStartDate).orElse(null)));
    token("periodEnd", new DateToken<>("period end date", r -> period
        .apply(r.getProcessingPeriodId()).map(ProcessingPeriodDto::getEndDate).orElse(null)));
    token("date", new DateToken<>("current date", r -> today()));
    token("emergency", new ConditionalToken<>("emergency flag", Requisition::getEmergency));
    token("random", new RandomToken<>());

    dimension("facility", Requisition::getFacilityId);
    dimension("program", Requisition::getProgramId);
    dimension("period", Requisition::getProcessingPeriodId);
    dimension("emergency", Requisition::getEmergency);
    dimension("year", r -> today().getYear());
    dimension("month", r -> today().getMonthValue());
  }

  private LocalDate today() {
    return dateHelper.getCurrentDateWithSystemZone();
  }

  /**
   * Renders the code of the facility's geographic zone at the level given as the argument, for
   * example {@code {zoneCode:2}}. Without an argument it renders the facility's own zone.
   */
  static final class ZoneCodeToken implements PatternToken<Requisition> {

    private final Function<UUID, GeographicZoneDto> zoneOf;

    ZoneCodeToken(Function<UUID, GeographicZoneDto> zoneOf) {
      this.zoneOf = zoneOf;
    }

    @Override
    public Function<Requisition, String> bind(String argument) {
      Integer level = parseLevel(argument);
      return requisition -> render(requisition, level);
    }

    @Override
    public String apply(Requisition requisition, String argument) {
      return render(requisition, parseLevel(argument));
    }

    private static Integer parseLevel(String argument) {
      if (argument == null || argument.isEmpty()) {
        return null;
      }
      try {
        return Integer.valueOf(argument);
      } catch (NumberFormatException ex) {
        throw new CodeGenerationException("Zone level must be a number: " + argument, ex);
      }
    }

    private String render(Requisition requisition, Integer level) {
      if (requisition.getFacilityId() == null) {
        throw new CodeGenerationException("Code pattern requires facility but it is not set");
      }
      GeographicZoneDto zone = zoneOf.apply(requisition.getFacilityId());
      while (zone != null && level != null && !level.equals(levelNumber(zone))) {
        zone = zone.getParent();
      }
      if (zone == null || zone.getCode() == null) {
        throw new CodeGenerationException("Facility " + requisition.getFacilityId()
            + " has no geographic zone" + (level == null ? "" : " at level " + level));
      }
      return zone.getCode();
    }

    private static Integer levelNumber(GeographicZoneDto zone) {
      return zone.getLevel() == null ? null : zone.getLevel().getLevelNumber();
    }
  }
}
