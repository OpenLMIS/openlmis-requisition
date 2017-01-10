package org.openlmis.requisition.service;

import org.openlmis.requisition.dto.UserDto;
import org.openlmis.settings.exception.ConfigurationSettingException;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.HashMap;
import java.util.Map;

import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_NOREPLY;

@Service
public class NotificationService extends BaseCommunicationService {
  private final Logger logger = LoggerFactory.getLogger(getClass());

  @Autowired
  private ConfigurationSettingService configurationSettingService;

  @Value("${notification.url}")
  private String notificationUrl;

  public NotificationService() {
    this(new RestTemplate());
  }

  NotificationService(RestTemplate restTemplate) {
    this.restTemplate = restTemplate;
  }

  /**
   * Send an email notification.
   *
   * @param user    receiver of the notification
   * @param subject subject of the email
   * @param content content of the email
   * @return true if success, false if failed.
   */
  public boolean notify(UserDto user, String subject, String content)
      throws ConfigurationSettingException {
    String from = configurationSettingService.getStringValue(REQUISITION_EMAIL_NOREPLY);
    String url = notificationUrl + "/api/notification";

    Map<String, String> params = new HashMap<>();
    params.put(ACCESS_TOKEN, obtainAccessToken());

    NotificationRequest request = new NotificationRequest(from, user.getEmail(),
        subject, content, null);
    try {
      restTemplate.postForObject(buildUri(url, params), request, Object.class);
    } catch (RestClientException ex) {
      logger.error("Can not send notification ", ex);
      return false;
    }
    return true;
  }

}
