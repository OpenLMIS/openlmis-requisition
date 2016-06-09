package org.openlmis.referencedata.web;

import org.openlmis.referencedata.i18n.ExposedMessageSource;
import org.openlmis.referencedata.util.ServiceSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class ServiceNameController {

  Logger logger = LoggerFactory.getLogger(ServiceNameController.class);

  @Autowired
  private ExposedMessageSource messageSource;

  @RequestMapping("/")
  public ServiceSignature index() {
    logger.debug("Returning service name and version");
    return new ServiceSignature(ServiceSignature.SERVICE_NAME, ServiceSignature.SERVICE_VERSION);
  }

  @RequestMapping("/hello")
  public String hello() {
    String[] msgArgs = {"world"};
    logger.debug("Returning hello world message");
    return messageSource.getMessage("requisition.message.hello", msgArgs, LocaleContextHolder
        .getLocale());
  }
}