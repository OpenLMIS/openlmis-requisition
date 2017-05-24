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

import static org.openlmis.requisition.service.AuthService.ACCESS_TOKEN;

import org.openlmis.requisition.dto.UserDto;
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
  private AuthService authService;

  @Value("${notification.url}")
  private String notificationUrl;

  @Value("${email.noreply}")
  private String from;

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
