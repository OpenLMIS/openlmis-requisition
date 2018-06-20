package org.openlmis.requisition.dto;

import org.junit.Test;
import org.openlmis.requisition.testutils.ToStringTestUtils;

public class ResultDtoTest extends DtoTest<ResultDto> {

  @Override
  protected Class<ResultDto> getTestClass() {
    return ResultDto.class;
  }

  @Test
  public void shouldImplementToString() {
    ToStringTestUtils.verify(ResultDto.class, new ResultDto<>());
  }

}
