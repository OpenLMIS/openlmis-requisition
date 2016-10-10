package org.openlmis.fulfillment.validate;


import org.openlmis.fulfillment.domain.OrderFileColumn;
import org.openlmis.fulfillment.domain.OrderFileTemplate;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Arrays;
import java.util.List;

@Component
public class OrderFileTemplateValidator implements Validator {

  private static final  String INVALID_FORMAT_DATE = "Invalid date format";

  private static final String[] ACCEPTED_VALUES = {"MM/yy", "MM/yyyy", "yy/MM", "yyyy/MM",
      "dd/MM/yy", "dd/MM/yyyy", "MM/dd/yy", "MM/dd/yyyy", "yy/MM/dd", "yyyy/MM/dd", "MM-yy",
      "MM-yyyy", "yy-MM", "yyyy-MM", "dd-MM-yy", "dd-MM-yyyy", "MM-dd-yy", "MM-dd-yyyy", "yy-MM-dd",
      "yyyy-MM-dd", "MMyy", "MMyyyy", "yyMM", "yyyyMM", "ddMMyy", "ddMMyyyy", "MMddyy", "MMddyyyy",
      "yyMMdd", "yyyyMMdd"};

  @Override
  public boolean supports(Class<?> clazz) {
    return OrderFileTemplate.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    OrderFileTemplate orderFileTemplate = (OrderFileTemplate) target;
    List<OrderFileColumn> columns = orderFileTemplate.getOrderFileColumns();
    List<String> acceptedValues = Arrays.asList(ACCEPTED_VALUES);

    for (int i = 0; i < columns.size(); i++) {
      OrderFileColumn orderFileColumn = columns.get(i);
      if ((orderFileColumn.getFormat() != null)
          && (!acceptedValues.contains(orderFileColumn.getFormat()))) {
        errors.rejectValue("orderFileColumns[" + i + "].format" ,
            INVALID_FORMAT_DATE, INVALID_FORMAT_DATE);
      }
    }
  }
}



