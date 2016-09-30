package org.openlmis.requisition.service.referencedata;

import org.openlmis.requisition.dto.ProcessingScheduleDto;
import org.springframework.stereotype.Service;

@Service
public class ScheduleReferenceDataService extends BaseReferenceDataService<ProcessingScheduleDto> {

  @Override
  protected String getUrl() {
    return "/api/processingSchedule/";
  }

  @Override
  protected Class<ProcessingScheduleDto> getResultClass() {
    return ProcessingScheduleDto.class;
  }
}
