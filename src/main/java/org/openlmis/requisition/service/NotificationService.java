package org.openlmis.requisition.service;

import static org.openlmis.requisition.service.AuthService.ACCESS_TOKEN;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_NOREPLY;

import org.openlmis.requisition.dto.UserDto;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.openlmis.util.NotificationRequest;
import org.openlmis.utils.RequestHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestOperations;
import org.springframework.web.client.RestTemplate;

@Service
public class NotificationService {
  private final Logger logger = LoggerFactory.getLogger(getClass());

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Autowired
  private AuthService authService;

  @Value("${notification.url}")
  private String notificationUrl;

  private RestOperations restTemplate = new RestTemplate();

  /**
   * Send an email notification.
   *
   * @param user    receiver of the notification
   * @param subject subject of the email
   * @param content content of the email
   * @return true if success, false if failed.
   */
  boolean notify(UserDto user, String subject, String content) {
    String from = configurationSettingService.getStringValue(REQUISITION_EMAIL_NOREPLY);
    String url = notificationUrl + "/api/notification";

    RequestParameters parameters = RequestParameters
        .init()
        .set(ACCESS_TOKEN, authService.obtainAccessToken());

    NotificationRequest request = new NotificationRequest(
        from, user.getEmail(), subject, content
    );
    
    try {
      restTemplate.postForObject(RequestHelper.createUri(url, parameters), request, Object.class);
    } catch (RestClientException ex) {
      logger.error("Can not send notification ", ex);
      return false;
    }
    return true;
  }

  void setRestTemplate(RestOperations restTemplate) {
    this.restTemplate = restTemplate;
  }
}
