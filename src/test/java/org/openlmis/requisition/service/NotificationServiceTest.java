package org.openlmis.requisition.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.openlmis.utils.ConfigurationSettingKeys.REQUISITION_EMAIL_NOREPLY;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.runners.MockitoJUnitRunner;
import org.openlmis.util.NotificationRequest;
import org.openlmis.requisition.dto.UserDto;
import org.openlmis.settings.service.ConfigurationSettingService;
import org.powermock.api.mockito.PowerMockito;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.net.URI;
import java.util.Collections;
import java.util.Map;

@RunWith(MockitoJUnitRunner.class)
public class NotificationServiceTest {

  private static final String ACCESS_TOKEN = "token";
  private static final String USER_EMAIL = "test@test.te";
  private static final String MAIL_SUBJECT = "subject";
  private static final String MAIL_CONTENT = "content";
  private static final String BASE_URL = "http://localhost";

  @Mock
  private ConfigurationSettingService configurationSettingService;

  @Spy
  @InjectMocks
  private NotificationService notificationService;

  private RestTemplate restTemplate = PowerMockito.mock(RestTemplate.class);

  @Before
  public void before() {
    ReflectionTestUtils.setField(notificationService, "notificationUrl", BASE_URL);
    ReflectionTestUtils.setField(notificationService, "authorizationUrl", BASE_URL);
    ReflectionTestUtils.setField(notificationService, "restTemplate", restTemplate);
    ReflectionTestUtils.setField(notificationService, "configurationSettingService",
        configurationSettingService);
    Map<String, String> body = Collections.singletonMap(NotificationService.ACCESS_TOKEN,
        ACCESS_TOKEN);
    ResponseEntity<Object> response = new ResponseEntity<>(body, HttpStatus.OK);
    when(restTemplate.exchange(any(URI.class), eq(HttpMethod.POST),
        any(HttpEntity.class), eq(Object.class))).thenReturn(response);
  }

  @Test
  public void shouldNotifyUser() throws Exception {
    UserDto user = mock(UserDto.class);
    when(user.getEmail()).thenReturn(USER_EMAIL);

    notificationService.notify(user, MAIL_SUBJECT, MAIL_CONTENT);

    verify(configurationSettingService).getStringValue(REQUISITION_EMAIL_NOREPLY);

    verify(restTemplate).postForObject(eq(
        new URI(BASE_URL + "/api/notification?access_token=" + ACCESS_TOKEN)),
        any(NotificationRequest.class), eq(Object.class));
  }
}
