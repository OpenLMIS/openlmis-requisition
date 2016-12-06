package org.openlmis.requisition.web;

import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.HashMap;
import java.util.Map;

@RequestMapping("/api")
public abstract class BaseController {

  protected Map<String, String> getErrors(BindingResult bindingResult) {
    Map<String, String> errors = new HashMap<>();

    for (FieldError error : bindingResult.getFieldErrors()) {
      errors.put(error.getField(), error.getCode());
    }

    return errors;
  }
}
