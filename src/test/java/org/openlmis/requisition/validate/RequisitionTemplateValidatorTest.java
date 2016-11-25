package org.openlmis.requisition.validate;

import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.requisition.domain.AvailableRequisitionColumn;
import org.openlmis.requisition.domain.RequisitionTemplate;
import org.openlmis.requisition.domain.RequisitionTemplateColumn;
import org.openlmis.requisition.domain.SourceType;
import org.springframework.validation.Errors;

import java.util.HashMap;
import java.util.Map;

@RunWith(MockitoJUnitRunner.class)
public class RequisitionTemplateValidatorTest {

  @InjectMocks
  private RequisitionTemplateValidator validator;

  private Errors errors = mock(Errors.class);

  @Test
  public void shouldRejectIfRequestedQuantityAndExplanationIsDisplayedValuesAreDifferent() {
    Map<String, RequisitionTemplateColumn> columnMap = new HashMap<>();
    columnMap.put(RequisitionTemplateValidator.REQUESTED_QUANTITY,
        generateTemplateColumn(RequisitionTemplateValidator.REQUESTED_QUANTITY, "J", true));
    columnMap.put(RequisitionTemplateValidator.REQUESTED_QUANTITY_EXPLANATION,
        generateTemplateColumn(RequisitionTemplateValidator.REQUESTED_QUANTITY_EXPLANATION,
            "W", false));

    RequisitionTemplate requisitionTemplate = new RequisitionTemplate(columnMap);

    validator.validate(requisitionTemplate, errors);

    verify(errors).rejectValue(eq(RequisitionTemplateValidator.COLUMNS_MAP),
        eq(RequisitionTemplateValidator.REQUESTED_QUANTITY_EXPLANATION
            + " must be displayed when requested quantity is displayed."));
  }

  private RequisitionTemplateColumn generateTemplateColumn(String name, String indicator,
                                                           boolean displayed) {
    AvailableRequisitionColumn columnDefinition = new AvailableRequisitionColumn();
    columnDefinition.setName(name);
    columnDefinition.setIndicator(indicator);

    RequisitionTemplateColumn requisitionTemplateColumn =
        new RequisitionTemplateColumn(columnDefinition);
    requisitionTemplateColumn.setSource(SourceType.USER_INPUT);
    requisitionTemplateColumn.setName(name);
    requisitionTemplateColumn.setIsDisplayed(displayed);
    return requisitionTemplateColumn;
  }
}
